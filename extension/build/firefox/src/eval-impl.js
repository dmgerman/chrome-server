// SPDX-License-Identifier: GPL-3.0-or-later
// Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
//
// eval-impl.js (Firefox) — placeholder.
//
// A real implementation will use browser.userScripts (the MV2/MV3
// hybrid API Firefox ships) to execute arbitrary JavaScript in a tab.
// Until then, this stub reports the primitive as unavailable so the
// `user-script` adapter returns a clear error instead of crashing.

export function evalAvailable() {
  return false;
}

export function evalUnavailableMessage() {
  return "JavaScript eval is not yet implemented for the Firefox target.";
}

export async function evalInTab(_options) {
  throw new Error(evalUnavailableMessage());
}
