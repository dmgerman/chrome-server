// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// background.js (Firefox) — placeholder.
//
// A real implementation will mirror the Chrome service worker
// (src/background.js in the Chrome target) but hold the WebSocket
// directly in this persistent background page rather than delegating
// to an offscreen document.  Until that work is done, the Firefox
// build produces a loadable extension that does not connect to Emacs.
console.warn("[chrome-server] Firefox background page is a placeholder; WebSocket not started.");
