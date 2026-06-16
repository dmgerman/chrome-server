// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// content.js — relay window.postMessage to the background service worker.
//
// Pages can trigger Emacs requests by posting a message like:
//
//   window.postMessage(
//     { source: "chrome-server", name: "EWW", payload: { url: "..." } },
//     "*"
//   );
//
// This mirrors the spookfox SPOOKFOX_RELAY_TO_EMACS hook so existing
// page-side integrations keep working.  Pages should only be considered
// allowed if they speak this protocol intentionally.

window.addEventListener("message", (event) => {
  if (event.source !== window) return;
  const data = event.data;
  if (!data || data.source !== "chrome-server") return;
  if (typeof data.name !== "string" || !data.name) return;
  chrome.runtime.sendMessage({
    target:  "service-worker",
    type:    "RELAY_TO_EMACS",
    name:    data.name,
    payload: data.payload ?? null,
  });
});
