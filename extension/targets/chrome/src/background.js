// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// background.js — MV3 service worker, acts as router.
//
// Three roles:
//   1. Boot and own the offscreen document (the WebSocket lives there).
//   2. Merge bundled config.json with chrome.storage.local and rebuild the
//      context menu tree.  Two menu items are hardcoded baseline (ORG_CAPTURE,
//      ORG_ROAM_CAPTURE); everything else comes from config.
//   3. Dispatch context-menu clicks, keyboard commands, and popup messages by
//      gathering the payload (URL, selection, page HTML, custom adapter) and
//      asking offscreen to send a request to Emacs.  Dispatch Emacs-initiated
//      requests by looking up the request name in the handlers[] config.

import { dispatchEmacsRequest } from "./handlers.js";
import { gatherPayload } from "./payload.js";
import {
  forgetTabConsent,
  getTabConsent,
  rememberTabConsent,
  tabHasConsent,
  CONSENT_DURATIONS,
} from "./consent.js";

// ── Per-tab icon (red when this tab has eval consent) ───────────────────────
//
// chrome.action.setIcon({ path: ... }) in MV3 service workers triggers a
// fetch that often fails with "Failed to fetch" regardless of whether the
// path is correct.  The reliable workaround is to pass ImageData built from
// the PNGs ourselves via OffscreenCanvas.  We cache the loaded ImageData
// per icon size so the conversion only happens once per SW lifetime.

const ICON_SIZES = [16, 48, 128];
const iconCache = { normal: null, red: null };

async function loadIconImageData(isRed) {
  const out = {};
  for (const size of ICON_SIZES) {
    const file = isRed ? `icons/icon-red-${size}.png` : `icons/icon${size}.png`;
    const url  = chrome.runtime.getURL(file);
    const blob = await (await fetch(url)).blob();
    const bmp  = await createImageBitmap(blob);
    const canvas = new OffscreenCanvas(size, size);
    const ctx    = canvas.getContext("2d");
    ctx.drawImage(bmp, 0, 0, size, size);
    out[size] = ctx.getImageData(0, 0, size, size);
  }
  return out;
}

async function getIconImageData(isRed) {
  const key = isRed ? "red" : "normal";
  if (!iconCache[key]) iconCache[key] = await loadIconImageData(isRed);
  return iconCache[key];
}

async function refreshTabIcon(tabId) {
  try {
    const granted   = await tabHasConsent(tabId);
    const imageData = await getIconImageData(granted);
    await chrome.action.setIcon({ tabId, imageData });
  } catch (e) {
    // Tab may have closed between consent change and icon update.
    log("setIcon failed for", tabId, e?.message);
  }
}

async function refreshAllTabIcons() {
  const tabs = await chrome.tabs.query({});
  await Promise.all(tabs.map((t) => refreshTabIcon(t.id)));
}

const OFFSCREEN_URL = "html/offscreen.html";
// All menus, baseline included, live in config.json's `menus[]` array.
// Entries with `baseline: true` are surfaced as immutable in the options
// page UI so they can't be removed accidentally, but they are otherwise
// regular config-driven menus.

let cachedConfig = null;
let wsStatus = "DISCONNECTED";

// ── Logging / notifications ──────────────────────────────────────────────────

function log(...args) {
  console.log("[bg]", ...args);
}

function notify(message) {
  chrome.notifications.create({
    type: "basic",
    iconUrl: chrome.runtime.getURL("icons/icon48.png"),
    title: "Chrome Server",
    message,
  });
}

let badgeTimer = null;
function badge(text, color, durationMs = 3000) {
  if (badgeTimer) clearTimeout(badgeTimer);
  chrome.action.setBadgeText({ text });
  chrome.action.setBadgeBackgroundColor({ color });
  badgeTimer = setTimeout(() => {
    chrome.action.setBadgeText({ text: "" });
    badgeTimer = null;
  }, durationMs);
}
function badgeOk()           { badge("✓", "#4a4"); }
function badgeError(message) { badge("!", "#c33", 5000); notify(message); }

// ── Offscreen document lifecycle ─────────────────────────────────────────────

async function ensureOffscreen() {
  if (!chrome.offscreen) {
    throw new Error("chrome.offscreen API unavailable");
  }
  const existing = await chrome.runtime.getContexts({
    contextTypes: ["OFFSCREEN_DOCUMENT"],
    documentUrls: [chrome.runtime.getURL(OFFSCREEN_URL)],
  });
  if (existing.length > 0) return;
  await chrome.offscreen.createDocument({
    url: OFFSCREEN_URL,
    reasons: ["WEB_RTC"], // closest documented reason for a long-lived socket
    justification: "Hold the WebSocket connection to the Emacs chrome-server.",
  });
  log("offscreen document created");
}

async function offscreenSend(message) {
  await ensureOffscreen();
  return new Promise((resolve, reject) => {
    chrome.runtime.sendMessage({ target: "offscreen", ...message }, (response) => {
      if (chrome.runtime.lastError) {
        reject(new Error(chrome.runtime.lastError.message));
        return;
      }
      resolve(response);
    });
  });
}

async function sendRequestToEmacs(name, payload) {
  const response = await offscreenSend({ type: "SEND_REQUEST", name, payload });
  if (!response?.ok) {
    throw new Error(response?.error ?? "send failed");
  }
  return response.payload;
}

// ── Config (bundled + storage merge) ─────────────────────────────────────────

async function loadBundledConfig() {
  try {
    const res = await fetch(chrome.runtime.getURL("config.json"));
    return await res.json();
  } catch (e) {
    log("could not load bundled config.json:", e);
    notify(`Could not load bundled config.json: ${e.message}`);
    return { menus: [], handlers: [] };
  }
}

async function loadConfig() {
  const bundled = await loadBundledConfig();
  const stored  = await chrome.storage.local.get(["menus", "handlers"]);
  return {
    menus:    stored.menus    ?? bundled.menus    ?? [],
    handlers: stored.handlers ?? bundled.handlers ?? [],
  };
}

// Serialise refreshes.  Several entry points call `refreshConfig`
// during start-up (the boot IIFE, `chrome.runtime.onInstalled`,
// `chrome.runtime.onStartup`, and `chrome.storage.onChanged`).  When
// two of them run concurrently both pass through
// `chrome.contextMenus.removeAll` before either issues its
// `contextMenus.create` calls, and the second `create` batch ends up
// trying to register ids that the first batch has already created.
// By caching the in-flight promise we make every concurrent caller
// await the same rebuild.
let refreshInFlight = null;
async function refreshConfig() {
  if (refreshInFlight) return refreshInFlight;
  refreshInFlight = (async () => {
    try {
      cachedConfig = await loadConfig();
      log("config loaded:", cachedConfig);
      await rebuildContextMenus(cachedConfig);
      return cachedConfig;
    } finally {
      refreshInFlight = null;
    }
  })();
  return refreshInFlight;
}

// ── Context menus ────────────────────────────────────────────────────────────

function triggerToContexts(trigger) {
  // `trigger` may be a string (one context) or an array of strings.
  // Strings other than the four recognised ones fall back to "page".
  const triggers = Array.isArray(trigger) ? trigger : [trigger ?? "page"];
  const VALID = new Set(["selection", "link", "image", "page"]);
  return triggers.map((t) => (VALID.has(t) ? t : "page"));
}

async function rebuildContextMenus(config) {
  await new Promise((resolve) => chrome.contextMenus.removeAll(resolve));
  for (const m of config.menus ?? []) {
    if (!m.id || !m.title || !m.request) {
      log("skipping malformed menu entry:", m);
      continue;
    }
    const opts = {
      id:       m.id,
      title:    m.title,
      contexts: triggerToContexts(m.trigger),
    };
    // documentUrlPatterns filters by the PAGE url (the document the click
    // happened on).  targetUrlPatterns filters by the LINK/IMG url (only
    // applies to those contexts).  Both are optional.
    if (Array.isArray(m.documentUrlPatterns) && m.documentUrlPatterns.length > 0) {
      opts.documentUrlPatterns = m.documentUrlPatterns;
    }
    if (Array.isArray(m.targetUrlPatterns) && m.targetUrlPatterns.length > 0) {
      opts.targetUrlPatterns = m.targetUrlPatterns;
    }
    chrome.contextMenus.create(opts);
  }
}

function findMenu(config, menuItemId) {
  return (config.menus ?? []).find((m) => m.id === menuItemId);
}

// ── Menu click dispatch ─────────────────────────────────────────────────────

async function effectiveRaise(menu) {
  // Per-menu override from the options page wins over the config default.
  // Missing entry → use whatever the menu's bundled `raise` says.
  const { raiseOverrides } = await chrome.storage.local.get(["raiseOverrides"]);
  const override = raiseOverrides?.[menu.id];
  return override !== undefined ? override : menu.raise;
}

async function handleMenuClick(menu, info, tab) {
  try {
    const payload = await gatherPayload(menu.payload, { tab, info });
    const raise   = await effectiveRaise(menu);
    if (raise !== undefined) payload.raise = raise;
    const response = await sendRequestToEmacs(menu.request, payload);
    badgeOk();
    const text = response?.message ?? `${menu.title}: done`;
    notify(text);
    return text;
  } catch (e) {
    log("menu dispatch failed:", e);
    badgeError(`${menu.title}: ${e.message}`);
    // Re-throw so popup-initiated clicks can show the failure inline.
    throw e;
  }
}

// ── Keyboard commands ────────────────────────────────────────────────────────

function findMenuByCommand(config, command) {
  // A menu's `command` is an object { name, description, suggested_key }.
  // The Chrome `commands.onCommand` event carries the command name string.
  return (config.menus ?? []).find((m) => m.command?.name === command);
}

async function handleCommand(command) {
  const config = cachedConfig ?? await refreshConfig();
  const menu   = findMenuByCommand(config, command);
  if (!menu) {
    notify(`No menu bound to keyboard command: ${command}`);
    return;
  }
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  if (!tab) {
    notify("No active tab.");
    return;
  }
  await handleMenuClick(menu, {}, tab);
}

// ── Wire up Chrome event listeners ───────────────────────────────────────────

chrome.runtime.onInstalled.addListener(() => {
  refreshConfig().then(ensureOffscreen).catch((e) => {
    log("onInstalled bootstrap failed:", e);
    notify(`Bootstrap failed: ${e.message}`);
  });
});

chrome.runtime.onStartup.addListener(() => {
  refreshConfig().then(ensureOffscreen).catch((e) => {
    log("onStartup bootstrap failed:", e);
    notify(`Bootstrap failed: ${e.message}`);
  });
});

chrome.storage.onChanged.addListener((changes) => {
  if (changes.menus || changes.handlers) {
    refreshConfig().catch((e) => {
      log("config refresh failed:", e);
      notify(`Config refresh failed: ${e.message}`);
    });
  }
});

chrome.contextMenus.onClicked.addListener(async (info, tab) => {
  const config = cachedConfig ?? await refreshConfig();
  const menu   = findMenu(config, info.menuItemId);
  if (!menu) {
    notify(`Unknown menu item: ${info.menuItemId}`);
    return;
  }
  await handleMenuClick(menu, info, tab);
});

chrome.commands.onCommand.addListener((command) => {
  handleCommand(command).catch((e) => {
    log("command failed:", e);
    badgeError(`${command}: ${e.message}`);
  });
});

// Drop the per-tab consent token when its tab goes away.  Tab ids are
// reused over time, so leaving stale entries would silently grant
// consent to a brand-new tab that happens to inherit the id.
chrome.tabs.onRemoved.addListener((tabId) => {
  forgetTabConsent(tabId).catch((e) => log("consent cleanup failed:", e));
});

// Re-check icon every time a tab becomes active, in case its consent
// expired (1h grant) while it was in the background.
chrome.tabs.onActivated.addListener(({ tabId }) => {
  refreshTabIcon(tabId);
});

// ── Messages from popup, content scripts, and offscreen doc ──────────────────

chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  if (!msg) return false;
  // Only handle messages addressed to us.
  if (msg.target !== undefined && msg.target !== "service-worker") return false;

  switch (msg.type) {
    case "WS_STATUS": {
      wsStatus = msg.status;
      log("ws status:", wsStatus);
      // Best-effort broadcast to popup; no-op if popup is closed.
      chrome.runtime.sendMessage({ target: "popup", type: "WS_STATUS", status: wsStatus })
        .catch(() => {});
      return false;
    }

    case "WS_STATUS_QUERY": {
      // Popup is asking; fetch from offscreen for ground truth.
      (async () => {
        try {
          const r = await offscreenSend({ type: "WS_STATUS_QUERY" });
          wsStatus = r.status;
          sendResponse({ status: wsStatus });
        } catch (e) {
          sendResponse({ status: "DISCONNECTED", error: e.message });
        }
      })();
      return true;
    }

    case "WS_RECONNECT": {
      (async () => {
        try {
          const r = await offscreenSend({ type: "WS_RECONNECT" });
          sendResponse(r);
        } catch (e) {
          sendResponse({ ok: false, error: e.message });
        }
      })();
      return true;
    }

    case "WS_REQUEST": {
      // Emacs is sending us a request; dispatch via the handlers[] config.
      (async () => {
        const config = cachedConfig ?? await refreshConfig();
        try {
          const payload = await dispatchEmacsRequest(msg.request, config.handlers ?? []);
          sendResponse(payload);
        } catch (e) {
          log("emacs request failed:", e);
          sendResponse({ status: "error", message: e.message });
        }
      })();
      return true;
    }

    case "SEND_REQUEST": {
      // From popup / content script: send a request to Emacs.
      (async () => {
        try {
          const payload = await sendRequestToEmacs(msg.name, msg.payload);
          sendResponse({ ok: true, payload });
        } catch (e) {
          sendResponse({ ok: false, error: e.message });
        }
      })();
      return true;
    }

    case "RELAY_TO_EMACS": {
      // From content.js postMessage relay.
      sendRequestToEmacs(msg.name, msg.payload)
        .then(() => sendResponse({ ok: true }))
        .catch((e) => sendResponse({ ok: false, error: e.message }));
      return true;
    }

    case "POPUP_MENU_CLICK": {
      // Popup wants to trigger a configured menu entry against a tab.
      (async () => {
        try {
          const config = cachedConfig ?? await refreshConfig();
          const menu   = findMenu(config, msg.menuId);
          if (!menu) throw new Error(`unknown menu: ${msg.menuId}`);
          const tab = await chrome.tabs.get(msg.tabId);
          const message = await handleMenuClick(menu, {}, tab);
          sendResponse({ ok: true, message });
        } catch (e) {
          sendResponse({ ok: false, error: e.message });
        }
      })();
      return true;
    }

    // ── Consent (per-tab) management for the popup ─────────────────────────

    case "CONSENT_GET": {
      // { tabId } → { state: "granted"|"absent", expiry: ms|null }
      getTabConsent(msg.tabId)
        .then((info) => sendResponse({ ok: true, ...info }))
        .catch((e) => sendResponse({ ok: false, error: e.message }));
      return true;
    }

    case "CONSENT_GRANT": {
      // { tabId, kind: "hour" | "tab" } → grant + return new state
      (async () => {
        try {
          const ms = CONSENT_DURATIONS[msg.kind];
          if (ms === undefined) throw new Error(`unknown consent kind: ${msg.kind}`);
          await rememberTabConsent(msg.tabId, ms);
          await refreshTabIcon(msg.tabId);
          const info = await getTabConsent(msg.tabId);
          sendResponse({ ok: true, ...info });
        } catch (e) {
          sendResponse({ ok: false, error: e.message });
        }
      })();
      return true;
    }

    case "CONSENT_REVOKE": {
      // { tabId } → drop the grant; future evals will re-prompt
      (async () => {
        await forgetTabConsent(msg.tabId);
        await refreshTabIcon(msg.tabId);
        sendResponse({ ok: true, state: "absent", expiry: null });
      })().catch((e) => sendResponse({ ok: false, error: e.message }));
      return true;
    }

    case "CONSENTED_TABS": {
      // Popup wants a list of tabs that currently have live consent.
      (async () => {
        try {
          const { consentByTab } = await chrome.storage.session.get(["consentByTab"]);
          const out = [];
          for (const [tabIdStr, entry] of Object.entries(consentByTab ?? {})) {
            const tabId  = Number(tabIdStr);
            const expiry = entry.expiry;
            if (expiry != null && expiry <= Date.now()) continue;  // skip expired
            let tab;
            try { tab = await chrome.tabs.get(tabId); } catch (e) { continue; }
            out.push({
              tabId,
              title:    tab.title ?? "",
              url:      tab.url ?? "",
              favIconUrl: tab.favIconUrl ?? "",
              windowId: tab.windowId,
              expiry,
            });
          }
          sendResponse({ ok: true, tabs: out });
        } catch (e) {
          sendResponse({ ok: false, error: e.message });
        }
      })();
      return true;
    }

    default:
      return false;
  }
});

// ── Boot ─────────────────────────────────────────────────────────────────────

(async () => {
  try {
    await refreshConfig();
    await ensureOffscreen();
    // SW restarts inherit chrome.storage.session.  Re-apply icons so
    // tabs with surviving consent still look red.
    await refreshAllTabIcons();
  } catch (e) {
    log("boot failed:", e);
    notify(`Chrome Server boot failed: ${e.message}`);
  }
})();
