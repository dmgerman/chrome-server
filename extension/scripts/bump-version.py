#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
# Assisted-by: Claude:claude-opus-4-7
#
# Bump the browsel version in browsel.el and config.json in lockstep.
#
# CLIENT_HELLO is strict on exact-match between the Emacs-side
# `browsel-version' constant and the extension's manifest version, so
# both have to move together.  This script does that from a single
# command; run it from the repo root or from extension/ — the paths it
# touches are resolved relative to the script's own location.
#
# Usage: scripts/bump-version.py X.Y[.Z]

import pathlib
import re
import sys


def main():
    if len(sys.argv) != 2:
        sys.exit("usage: bump-version.py X.Y[.Z]")
    version = sys.argv[1]
    if not re.fullmatch(r"\d+\.\d+(\.\d+)?", version):
        sys.exit(f"invalid version: {version!r}")

    extension_dir = pathlib.Path(__file__).resolve().parent.parent
    repo_root = extension_dir.parent
    cfg_path = extension_dir / "config.json"
    el_path = repo_root / "browsel.el"

    # Regex-substitute both files in place so unrelated formatting
    # (array layout in config.json, blank lines and comments in
    # browsel.el) is preserved verbatim.
    changed = []
    for path, pattern in [
        (el_path, re.compile(r'(\(defconst browsel-version ")[^"]*(")')),
        (cfg_path, re.compile(r'("version":\s*")[^"]*(")')),
    ]:
        text = path.read_text()
        if not pattern.search(text):
            sys.exit(f"version field not found in {path}")
        new = pattern.sub(rf"\g<1>{version}\g<2>", text, count=1)
        if new != text:
            path.write_text(new)
            changed.append(path.name)

    if changed:
        print(f"bumped {', '.join(changed)} -> {version}")
    else:
        print(f"already at {version}; nothing to do")


if __name__ == "__main__":
    main()
