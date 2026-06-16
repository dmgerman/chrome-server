// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// offscreen.js — holds the WebSocket to the Emacs server.
//
// MV3 service workers idle out after ~30s and would kill a WebSocket held
// by the worker.  An offscreen document is a hidden DOM page that does not
// idle out; the service worker uses it as a long-lived host for the socket
// and talks to it over chrome.runtime messages.
//
// Wire protocol (spookfox-compatible):
//
//   Request  Emacs->browser: { id, name, payload }
//   Request  browser->Emacs: { id, name, payload }
//   Response (either way) : { requestId, payload }
//
// Frames go in both directions.  Requests from the service worker
// (`SEND_REQUEST`) are correlated to responses via a per-id callback map
// living in this offscreen page.

// Use 127.0.0.1 explicitly.  Chrome on macOS resolves `localhost` to ::1
// (IPv6) first, but the Emacs websocket-server binds to 127.0.0.1 (IPv4).
// Reaching it by name would yield ECONNREFUSED on the IPv6 attempt.
const WS_URL = "ws://127.0.0.1:9130";
const RECONNECT_INTERVAL_MS = 5000;
const REQUEST_TIMEOUT_MS = 5000;

let ws = null;
let reconnectTimer = null;
let status = "DISCONNECTED";
const pending = new Map(); // requestId -> { resolve, reject, timer }

function log(...args) {
  console.log("[offscreen]", ...args);
}

function setStatus(next) {
  if (status === next) return;
  status = next;
  chrome.runtime.sendMessage({ target: "service-worker", type: "WS_STATUS", status });
}

function clearReconnect() {
  if (reconnectTimer) {
    clearTimeout(reconnectTimer);
    reconnectTimer = null;
  }
}

function scheduleReconnect() {
  clearReconnect();
  reconnectTimer = setTimeout(connect, RECONNECT_INTERVAL_MS);
}

function connect() {
  clearReconnect();
  if (ws && (ws.readyState === WebSocket.OPEN || ws.readyState === WebSocket.CONNECTING)) {
    return;
  }
  setStatus("CONNECTING");
  log("connecting to", WS_URL);
  try {
    ws = new WebSocket(WS_URL);
  } catch (e) {
    log("WebSocket constructor failed:", e);
    setStatus("DISCONNECTED");
    scheduleReconnect();
    return;
  }
  ws.addEventListener("open", () => {
    log("connected");
    setStatus("CONNECTED");
  });
  ws.addEventListener("close", () => {
    log("closed");
    setStatus("DISCONNECTED");
    // Reject any outstanding requests so callers don't hang forever.
    for (const [id, p] of pending) {
      clearTimeout(p.timer);
      p.reject(new Error("WebSocket closed before response"));
      pending.delete(id);
    }
    scheduleReconnect();
  });
  ws.addEventListener("error", (e) => {
    log("error", e);
    // The 'close' event will follow; let it drive the reconnect.
  });
  ws.addEventListener("message", (event) => {
    handleFrame(event.data);
  });
}

function handleFrame(text) {
  log("recv", text);
  let msg;
  try {
    msg = JSON.parse(text);
  } catch (e) {
    log("bad frame", e, text);
    return;
  }
  if (msg.name) {
    // Emacs is asking us for something.
    chrome.runtime.sendMessage(
      { target: "service-worker", type: "WS_REQUEST", request: msg },
      (response) => {
        if (chrome.runtime.lastError) {
          send({ requestId: msg.id, payload: { status: "error", message: chrome.runtime.lastError.message } });
          return;
        }
        send({ requestId: msg.id, payload: response ?? { status: "ok" } });
      }
    );
    return;
  }
  if (msg.requestId) {
    const entry = pending.get(msg.requestId);
    if (!entry) return;
    clearTimeout(entry.timer);
    pending.delete(msg.requestId);
    entry.resolve(msg.payload);
    return;
  }
  log("unknown frame shape", msg);
}

function send(obj) {
  if (!ws || ws.readyState !== WebSocket.OPEN) {
    throw new Error("WebSocket not connected");
  }
  const text = JSON.stringify(obj);
  log("send", text);
  ws.send(text);
}

function sendRequest(name, payload) {
  return new Promise((resolve, reject) => {
    if (!ws || ws.readyState !== WebSocket.OPEN) {
      reject(new Error("not connected"));
      return;
    }
    const id = crypto.randomUUID();
    const timer = setTimeout(() => {
      if (pending.has(id)) {
        pending.delete(id);
        reject(new Error(`request ${name} timed out`));
      }
    }, REQUEST_TIMEOUT_MS);
    pending.set(id, { resolve, reject, timer });
    try {
      send({ id, name, payload: payload ?? null });
    } catch (e) {
      clearTimeout(timer);
      pending.delete(id);
      reject(e);
    }
  });
}

// Messages from the service worker.
chrome.runtime.onMessage.addListener((msg, sender, sendResponse) => {
  if (!msg || msg.target !== "offscreen") return false;
  switch (msg.type) {
    case "WS_STATUS_QUERY":
      sendResponse({ status });
      return false;
    case "WS_RECONNECT":
      if (ws) {
        try { ws.close(); } catch (e) {}
      }
      connect();
      sendResponse({ ok: true });
      return false;
    case "WS_SEND_RESPONSE":
      // service worker handled a request from Emacs and is sending back the
      // response payload.  msg.requestId is Emacs's id; msg.payload is the
      // handler return value.
      try {
        send({ requestId: msg.requestId, payload: msg.payload });
        sendResponse({ ok: true });
      } catch (e) {
        sendResponse({ ok: false, error: e.message });
      }
      return false;
    case "SEND_REQUEST":
      // service worker (popup, content script, context-menu click) wants to
      // send a request to Emacs.
      sendRequest(msg.name, msg.payload)
        .then((payload) => sendResponse({ ok: true, payload }))
        .catch((e) => sendResponse({ ok: false, error: e.message }));
      return true;
    default:
      return false;
  }
});

// Kick things off.
connect();
