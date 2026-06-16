// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// popup.js — connection status + config-driven action buttons.

const dot         = document.getElementById("dot");
const statusText  = document.getElementById("status-text");
const messageEl   = document.getElementById("message");
const customEl    = document.getElementById("custom-actions");
const optionsLink = document.getElementById("options-link");

function setStatus(status) {
  dot.className = "dot " + (status?.toLowerCase() ?? "disconnected");
  const label = {
    CONNECTED:    "Connected",
    CONNECTING:   "Connecting…",
    DISCONNECTED: "Disconnected",
  }[status] ?? "Unknown";
  statusText.textContent = label;
}

function setMessage(text, isError = false) {
  messageEl.textContent = text ?? "";
  messageEl.style.color = isError ? "#c33" : "#555";
}

function sendToBackground(message) {
  return new Promise((resolve) => {
    chrome.runtime.sendMessage(message, (response) => {
      if (chrome.runtime.lastError) {
        resolve({ ok: false, error: chrome.runtime.lastError.message });
        return;
      }
      resolve(response ?? {});
    });
  });
}

async function getActiveTab() {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  return tab;
}

// ── Reconnect / options ─────────────────────────────────────────────────────

document.getElementById("reconnect").addEventListener("click", async () => {
  setMessage("Reconnecting…");
  const r = await sendToBackground({ target: "service-worker", type: "WS_RECONNECT" });
  if (r?.ok) setMessage("Reconnect requested.");
  else       setMessage(`Reconnect failed: ${r?.error ?? "unknown"}`, true);
});

optionsLink.addEventListener("click", () => chrome.runtime.openOptionsPage());

// ── Consent panel (this tab) ────────────────────────────────────────────────

const stateEl   = document.getElementById("consent-state");
const buttonsEl = document.getElementById("consent-buttons");

function describeConsent({ state, expiry }) {
  if (state !== "granted") return { text: "Not granted", cls: "absent" };
  if (expiry == null) return { text: "Allowed until tab closes", cls: "granted" };
  const remaining = expiry - Date.now();
  if (remaining <= 0) return { text: "Not granted", cls: "absent" };
  const mins = Math.ceil(remaining / 60000);
  return {
    text: mins >= 60
      ? `Allowed (${(mins / 60).toFixed(1)} h left)`
      : `Allowed (${mins} min left)`,
    cls: "granted",
  };
}

function setConsentUI(tabId, info) {
  const { text, cls } = describeConsent(info);
  stateEl.textContent = text;
  stateEl.className   = "consent-state " + cls;
  buttonsEl.innerHTML = "";

  const mkBtn = (label, type, kind) => {
    const b = document.createElement("button");
    b.textContent = label;
    if (type === "revoke") b.className = "revoke";
    b.addEventListener("click", async () => {
      const message = type === "revoke"
        ? { target: "service-worker", type: "CONSENT_REVOKE", tabId }
        : { target: "service-worker", type: "CONSENT_GRANT",  tabId, kind };
      const r = await sendToBackground(message);
      if (r?.ok) {
        setConsentUI(tabId, r);
        renderConsentedTabs();
      } else {
        setMessage(`Consent: ${r?.error ?? "unknown"}`, true);
      }
    });
    return b;
  };

  if (info.state === "granted" &&
      (info.expiry == null || info.expiry > Date.now())) {
    buttonsEl.appendChild(mkBtn("Revoke", "revoke"));
  } else {
    buttonsEl.appendChild(mkBtn("1 hour",   "grant", "hour"));
    buttonsEl.appendChild(mkBtn("This tab", "grant", "tab"));
  }
}

async function renderConsent() {
  const tab = await getActiveTab();
  if (!tab) {
    stateEl.textContent = "(no active tab)";
    stateEl.className   = "consent-state absent";
    buttonsEl.innerHTML = "";
    return;
  }
  const r = await sendToBackground({
    target: "service-worker", type: "CONSENT_GET", tabId: tab.id,
  });
  if (r?.ok) setConsentUI(tab.id, r);
  else       setMessage(`Consent: ${r?.error ?? "unknown"}`, true);
}

// ── Consented tabs (jump list) ──────────────────────────────────────────────

const consentedTabsEl = document.getElementById("consented-tabs");

function shortRemaining(expiry) {
  if (expiry == null) return "until close";
  const ms = expiry - Date.now();
  if (ms <= 0) return "expired";
  const mins = Math.ceil(ms / 60000);
  return mins >= 60 ? `${(mins / 60).toFixed(1)} h left` : `${mins} min left`;
}

async function renderConsentedTabs() {
  const r = await sendToBackground({ target: "service-worker", type: "CONSENTED_TABS" });
  consentedTabsEl.innerHTML = "";
  if (!r?.ok || !r.tabs?.length) return;

  const header = document.createElement("div");
  header.className   = "header";
  header.textContent = `Tabs with permission (${r.tabs.length})`;
  consentedTabsEl.appendChild(header);

  for (const t of r.tabs) {
    const row = document.createElement("div");
    row.className = "ctab";

    if (t.favIconUrl) {
      const fav = document.createElement("img");
      fav.className = "fav";
      fav.src       = t.favIconUrl;
      fav.onerror   = () => fav.remove();
      row.appendChild(fav);
    }

    const meta = document.createElement("div");
    meta.className = "meta";
    const titleEl = document.createElement("div");
    titleEl.className   = "title";
    titleEl.textContent = t.title || t.url || `tab ${t.tabId}`;
    const subEl = document.createElement("div");
    subEl.className   = "sub";
    let host = "";
    try { host = new URL(t.url).host; } catch {}
    subEl.textContent = `${host} · ${shortRemaining(t.expiry)}`;
    meta.appendChild(titleEl);
    meta.appendChild(subEl);
    row.appendChild(meta);

    const revoke = document.createElement("button");
    revoke.className   = "revoke";
    revoke.textContent = "Revoke";
    revoke.addEventListener("click", async (e) => {
      e.stopPropagation();
      await sendToBackground({ target: "service-worker", type: "CONSENT_REVOKE", tabId: t.tabId });
      await renderConsentedTabs();
      await renderConsent();
    });
    row.appendChild(revoke);

    row.addEventListener("click", async () => {
      await chrome.tabs.update(t.tabId, { active: true });
      if (typeof t.windowId === "number") {
        try { await chrome.windows.update(t.windowId, { focused: true }); } catch {}
      }
      window.close();
    });

    consentedTabsEl.appendChild(row);
  }
}

// ── Config-driven action buttons ────────────────────────────────────────────
//
// Renders one button per `menus[]` entry from config.json (or the
// chrome.storage.local override).  Clicking a button forwards a
// POPUP_MENU_CLICK to the service worker so the full payload-gathering
// pipeline (gatherPayload + raise + handlers) runs identically to the
// right-click context-menu path.

async function loadMenus() {
  const stored = await chrome.storage.local.get(["menus"]);
  if (stored.menus) return stored.menus;
  try {
    const res = await fetch(chrome.runtime.getURL("config.json"));
    return (await res.json()).menus ?? [];
  } catch (e) {
    return [];
  }
}

async function renderActions() {
  const menus = await loadMenus();
  customEl.innerHTML = "";
  if (!menus.length) return;
  for (const m of menus) {
    const btn = document.createElement("button");
    btn.className   = "action";
    btn.textContent = m.title;
    if (m.command?.name) btn.dataset.command = m.command.name;
    btn.addEventListener("click", async () => {
      const tab = await getActiveTab();
      if (!tab) { setMessage("No active tab.", true); return; }
      // "Sent…" is optimistic — the request is on its way to Emacs.
      // Replaced by "Finished: <emacs reply>" once Emacs responds.
      setMessage("Sent…");
      const r = await sendToBackground({
        target:  "service-worker",
        type:    "POPUP_MENU_CLICK",
        menuId:  m.id,
        tabId:   tab.id,
      });
      if (r?.ok) setMessage(`Finished: ${r.message ?? "ok"}`);
      else       setMessage(`Error: ${r?.error ?? "unknown"}`, true);
    });
    customEl.appendChild(btn);
  }
}

// ── Shortcut hints ──────────────────────────────────────────────────────────
//
// Annotate each action button with its CURRENT keyboard shortcut.  Chrome
// returns whatever the user has actually bound at chrome://extensions/shortcuts
// (possibly different from the suggested key in the manifest); an empty
// `shortcut` means the user hasn't bound one yet.

async function commandShortcutMap() {
  if (!chrome.commands?.getAll) return {};
  try {
    const commands = await chrome.commands.getAll();
    return Object.fromEntries(
      commands.map((c) => [c.name, c.shortcut ?? ""]),
    );
  } catch (e) {
    return {};
  }
}

function annotateButton(btn, shortcut) {
  if (!shortcut) return;
  const span = document.createElement("span");
  span.className   = "shortcut";
  span.textContent = shortcut;
  btn.appendChild(span);
}

async function applyShortcutHints() {
  const map = await commandShortcutMap();
  document.querySelectorAll("[data-command]").forEach((btn) => {
    annotateButton(btn, map[btn.dataset.command]);
  });
}

// ── Live status updates from the service worker ─────────────────────────────

chrome.runtime.onMessage.addListener((msg) => {
  if (msg?.target !== "popup") return false;
  if (msg.type === "WS_STATUS") setStatus(msg.status);
  return false;
});

// On open, query the current status.
sendToBackground({ target: "service-worker", type: "WS_STATUS_QUERY" })
  .then((r) => setStatus(r?.status ?? "DISCONNECTED"));

renderActions().then(applyShortcutHints);
renderConsent();
renderConsentedTabs();
