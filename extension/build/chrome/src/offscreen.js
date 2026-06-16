// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
// Assisted-by: Claude:claude-opus-4-7
//
// offscreen.js (Chrome) — host the WebSocket on behalf of the service
// worker.
//
// MV3 service workers idle out after ~30s and would kill any WebSocket
// they held.  An offscreen document is a hidden DOM page that does not
// idle, so it is a safe host for the long-lived socket.  All of the
// actual WebSocket logic lives in `src/ws-client.js`; this file is
// only the offscreen-side glue: it wires the shared client to
// chrome.runtime messages so the service worker can drive
// send/reconnect/status-query and so incoming Emacs requests can hop
// back up to the SW for handler dispatch.

import { startWebSocketClient } from "./ws-client.js";

function log(...args) { console.log("[offscreen]", ...args); }

// Ask the service worker to dispatch one Emacs-initiated request and
// resolve with the payload it returns.  The SW already has the
// merged handler config; doing the dispatch here would require
// duplicating that, and the SW is alive whenever a frame arrives
// (the WebSocket message wakes it).
function dispatchIncomingViaServiceWorker(request) {
  return new Promise((resolve, reject) => {
    chrome.runtime.sendMessage(
      { target: "service-worker", type: "WS_REQUEST", request },
      (response) => {
        if (chrome.runtime.lastError) {
          reject(new Error(chrome.runtime.lastError.message));
          return;
        }
        resolve(response ?? { status: "ok" });
      },
    );
  });
}

// Offscreen documents expose only the messaging subset of
// chrome.runtime — `getManifest` is not in that subset, so we ask the
// service worker for the extension's version.  Top-level await on an
// ES module delays the rest of this file until the SW responds, which
// is acceptable here because the SW's onMessage listener is registered
// synchronously at SW boot and is reachable as soon as this document
// has been created.
function fetchVersionFromServiceWorker() {
  return new Promise((resolve, reject) => {
    chrome.runtime.sendMessage(
      { target: "service-worker", type: "GET_VERSION" },
      (response) => {
        if (chrome.runtime.lastError) {
          reject(new Error(chrome.runtime.lastError.message));
          return;
        }
        if (!response?.version) {
          reject(new Error("service worker returned no version"));
          return;
        }
        resolve(response.version);
      },
    );
  });
}

const version = await fetchVersionFromServiceWorker();
log("starting ws client at version", version);

const client = startWebSocketClient({
  clientName: "chrome",
  version,
  onStatus: (status) => {
    chrome.runtime
      .sendMessage({ target: "service-worker", type: "WS_STATUS", status })
      .catch(() => {});
  },
  onIncompatible: (message) => {
    chrome.runtime
      .sendMessage({ target: "service-worker", type: "WS_INCOMPATIBLE", message })
      .catch(() => {});
  },
  onIncomingRequest: dispatchIncomingViaServiceWorker,
});

// Messages from the service worker.
chrome.runtime.onMessage.addListener((msg, _sender, sendResponse) => {
  if (!msg || msg.target !== "offscreen") return false;
  switch (msg.type) {
    case "WS_STATUS_QUERY":
      sendResponse({ status: client.getStatus() });
      return false;

    case "WS_RECONNECT":
      try {
        client.reconnect();
        sendResponse({ ok: true });
      } catch (e) {
        sendResponse({ ok: false, error: e?.message ?? String(e) });
      }
      return false;

    case "SEND_REQUEST":
      client.sendRequest(msg.name, msg.payload).then(
        (payload) => sendResponse({ ok: true, payload }),
        (e)       => sendResponse({ ok: false, error: e?.message ?? String(e) }),
      );
      return true;

    default:
      log("unknown message type:", msg.type);
      return false;
  }
});
