// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// handlers.js — dispatch Emacs-initiated requests via the handlers[] config.
//
// A handler entry looks like:
//   { name: "GET_ALL_TABS", api: "chrome.tabs.query", args: {} }
//   { name: "OPEN_TAB",     api: "chrome.tabs.create", "args-from": "payload" }
//   { name: "FOCUS_TAB",    api: "chrome.tabs.update", "args-shape": "focus-tab" }
//   { name: "EVAL_IN_ACTIVE_TAB", api: "chrome.userScripts.execute",
//       "args-shape": "user-script" }
//
// args        — static argument literal
// args-from   — "payload": pass the request payload through
// args-shape  — named adapter (see SHAPE_ADAPTERS) for irregular APIs
//
// API resolution is reflective: "chrome.tabs.query" is split on '.' and
// walked from `chrome`.  This is plain property access, not eval, so it is
// not blocked by MV3 CSP.  The set of reachable APIs is bounded by what the
// manifest declares in permissions.

import { ensureConsent, tabHasConsent } from "./consent.js";
import { evalAvailable, evalUnavailableMessage, evalInTab } from "./eval-impl.js";

// chrome.action.setIcon({path}) fails inside MV3 service workers
// ("Failed to fetch") regardless of path correctness.  We build ImageData
// from the bundled PNGs the same way background.js does.  Cache lives for
// the SW's lifetime; both icon variants get loaded lazily on first use.

const ICON_SIZES = [16, 48, 128];
const iconCache  = { normal: null, red: null };

async function loadIconImageData(isRed) {
  const out = {};
  for (const size of ICON_SIZES) {
    const file = isRed ? `icons/icon-red-${size}.png` : `icons/icon${size}.png`;
    const blob = await (await fetch(chrome.runtime.getURL(file))).blob();
    const bmp  = await createImageBitmap(blob);
    const canvas = new OffscreenCanvas(size, size);
    canvas.getContext("2d").drawImage(bmp, 0, 0, size, size);
    out[size] = canvas.getContext("2d").getImageData(0, 0, size, size);
  }
  return out;
}

async function syncTabIcon(tabId) {
  try {
    const granted = await tabHasConsent(tabId);
    const key = granted ? "red" : "normal";
    if (!iconCache[key]) iconCache[key] = await loadIconImageData(granted);
    await chrome.action.setIcon({ tabId, imageData: iconCache[key] });
  } catch {
    // Tab may have closed; harmless.
  }
}

const SHAPE_ADAPTERS = {
  // { id: 123 } -> chrome.tabs.update(123, { active: true })
  // optional { focusWindow: true } also focuses the parent window.
  async "focus-tab"(payload) {
    if (!payload || typeof payload.id !== "number") {
      throw new Error("focus-tab: payload.id (tab id) required");
    }
    await chrome.tabs.update(payload.id, { active: true });
    if (payload.focusWindow) {
      const tab = await chrome.tabs.get(payload.id);
      if (tab.windowId !== undefined) {
        await chrome.windows.update(tab.windowId, { focused: true });
      }
    }
    return { status: "ok" };
  },

  // { id: 123 } -> chrome.tabs.remove(123)
  async "tab-id"(payload) {
    if (!payload || typeof payload.id !== "number") {
      throw new Error("tab-id: payload.id required");
    }
    return await chrome.tabs.remove(payload.id);
  },

  // { code: "..." } -> runtime-specific eval primitive in ./eval-impl.js.
  // Defaults to the active tab and world: "MAIN" (sees the page's window).
  // Gated by ensureConsent: the first invocation on each tab displays an
  // in-page overlay asking the user to allow/deny.
  async "user-script"(payload) {
    if (!payload || typeof payload.code !== "string") {
      throw new Error("user-script: payload.code (string) required");
    }
    let tabId = payload.tabId;
    if (tabId === undefined) {
      const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
      if (!tab) throw new Error("no active tab for user-script execution");
      tabId = tab.id;
    }
    if (!evalAvailable()) {
      throw new Error(evalUnavailableMessage());
    }
    // Per-tab consent.  Throws on deny or 30s timeout.
    await ensureConsent(tabId, payload.code);
    // After ensureConsent the storage entry may have flipped from absent
    // to granted (user clicked Allow 1h / Allow this tab in the overlay).
    // Sync the toolbar icon so the tab looks red right away.
    await syncTabIcon(tabId);
    const result = await evalInTab({
      tabId,
      code:  payload.code,
      world: payload.world ?? "MAIN",
    });
    return { status: "ok", result };
  },
};

function resolveApi(path) {
  if (!path || typeof path !== "string") {
    throw new Error("handler missing `api` path");
  }
  if (!path.startsWith("chrome.")) {
    throw new Error(`api path must start with 'chrome.': ${path}`);
  }
  const parts = path.split(".");
  let cursor = self; // service worker global; chrome lives here
  for (const segment of parts) {
    if (cursor == null || typeof cursor !== "object") {
      throw new Error(`api path not reachable: ${path} (broke at '${segment}')`);
    }
    cursor = cursor[segment];
  }
  if (typeof cursor !== "function") {
    throw new Error(`api path does not resolve to a function: ${path}`);
  }
  return cursor;
}

function pickArgs(handler, payload) {
  if (handler["args-from"] === "payload") return payload ?? {};
  if (handler.args !== undefined)         return handler.args;
  return {};
}

export async function dispatchEmacsRequest(request, handlers) {
  const { name, payload } = request;
  const handler = (handlers ?? []).find((h) => h.name === name);
  if (!handler) {
    throw new Error(`no handler registered for request: ${name}`);
  }

  if (handler["args-shape"]) {
    const adapter = SHAPE_ADAPTERS[handler["args-shape"]];
    if (!adapter) {
      throw new Error(`unknown args-shape: ${handler["args-shape"]}`);
    }
    return await adapter(payload, handler);
  }

  const fn   = resolveApi(handler.api);
  const args = pickArgs(handler, payload);
  // chrome.* APIs in MV3 return promises directly.
  const result = await fn(args);
  return result === undefined ? { status: "ok" } : result;
}
