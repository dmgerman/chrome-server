// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
// Assisted-by: Claude:claude-opus-4-7
//
// background.js (Chrome) — service-worker entry.
//
// All routing logic lives in src/core.js.  This file owns three
// Chrome-specific pieces:
//
//   1. The offscreen document.  MV3 service workers idle out after
//      ~30s and would kill any WebSocket they held, so the socket
//      lives in an offscreen page (`html/offscreen.html` →
//      `src/offscreen.js`).  We create it on boot and recreate it on
//      install/startup.
//
//   2. The transport object passed to core.  Each method round-trips a
//      `chrome.runtime.sendMessage` to the offscreen page.
//
//   3. Two extra `runtime.onMessage` cases that core does not handle
//      because they only exist in the SW-plus-offscreen architecture:
//      `WS_STATUS` (status broadcast coming up from offscreen) and
//      `WS_REQUEST` (a request from Emacs that needs to be dispatched
//      via `handlers[]`).

import {
  initRouter,
  setWsStatus,
  dispatchIncomingEmacsRequest,
} from "./core.js";

const OFFSCREEN_URL = "html/offscreen.html";

// Last status pushed up from the offscreen page.  Used as a fallback
// when the pull-through `transport.getStatus` cannot reach offscreen;
// updated by the WS_STATUS message handler below.
let cachedStatus = "DISCONNECTED";

function log(...args) { console.log("[bg]", ...args); }

// ── Offscreen lifecycle ─────────────────────────────────────────────────────

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
    // "WEB_RTC" is the closest documented reason for keeping a
    // long-lived socket alive.
    reasons: ["WEB_RTC"],
    justification: "Hold the WebSocket connection to the Emacs browsel.",
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

// ── Transport for core ──────────────────────────────────────────────────────

const transport = {
  async sendRequest(name, payload) {
    const response = await offscreenSend({ type: "SEND_REQUEST", name, payload });
    if (!response?.ok) {
      throw new Error(response?.error ?? "send failed");
    }
    return response.payload;
  },
  async reconnect() {
    return await offscreenSend({ type: "WS_RECONNECT" });
  },
  // Pull-through to offscreen so a respawned SW (cachedStatus reset
  // to "DISCONNECTED") cannot report a stale value to the popup.
  // cachedStatus stays as a fallback for the case where the offscreen
  // round-trip fails.
  async getStatus() {
    try {
      const r = await offscreenSend({ type: "WS_STATUS_QUERY" });
      if (r?.status) {
        cachedStatus = r.status;
        return r.status;
      }
    } catch (e) {
      log("status query to offscreen failed:", e?.message ?? e);
    }
    return cachedStatus;
  },
};

// ── Chrome-only runtime.onMessage cases (offscreen → SW) ────────────────────
//
// Registered BEFORE `initRouter` so these two types are observed before
// core's listener returns false on them.

chrome.runtime.onMessage.addListener((msg, _sender, sendResponse) => {
  if (!msg) return false;
  if (msg.target !== undefined && msg.target !== "service-worker") return false;

  switch (msg.type) {
    case "WS_STATUS": {
      cachedStatus = msg.status;
      setWsStatus(msg.status);
      return false;
    }

    case "WS_REQUEST": {
      Promise.resolve()
        .then(() => dispatchIncomingEmacsRequest(msg.request))
        .then(
          (payload) => sendResponse(payload),
          (e) => {
            log("emacs request failed:", e);
            sendResponse({ status: "error", message: e?.message ?? String(e) });
          },
        );
      return true;
    }

    case "WS_INCOMPATIBLE": {
      log("incompatible:", msg.message);
      chrome.notifications.create({
        type: "basic",
        iconUrl: chrome.runtime.getURL("icons/icon48.png"),
        title: "Browsel",
        message: `Version mismatch: ${msg.message}`,
      });
      chrome.action.setBadgeText({ text: "!" }).catch(() => {});
      chrome.action.setBadgeBackgroundColor({ color: "#c33" }).catch(() => {});
      return false;
    }

    case "GET_VERSION": {
      // Offscreen documents only expose a subset of chrome.runtime
      // (the messaging APIs); getManifest is not part of that subset.
      // The SW reads it on offscreen's behalf so the version that
      // travels in CLIENT_HELLO is the same one the manifest carries.
      sendResponse({ version: chrome.runtime.getManifest().version });
      return false;
    }

    default:
      return false;
  }
});

// ── Boot ────────────────────────────────────────────────────────────────────

(async () => {
  try {
    await ensureOffscreen();
    await initRouter(transport);
  } catch (e) {
    log("boot failed:", e);
  }
})();

chrome.runtime.onInstalled.addListener(() => {
  ensureOffscreen().catch((e) => log("onInstalled offscreen failed:", e));
});

chrome.runtime.onStartup.addListener(() => {
  ensureOffscreen().catch((e) => log("onStartup offscreen failed:", e));
});
