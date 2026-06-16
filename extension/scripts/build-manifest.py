#!/usr/bin/env python3
# SPDX-License-Identifier: GPL-3.0-or-later
# Copyright (C) 2026 Daniel M. German <dmg@turingmachine.org>
# Assisted-by: Claude:claude-opus-4-7
"""Generate manifest.json from config.json.

config.json is the SINGLE SOURCE OF TRUTH for this extension:

  - config.extension       : shared MV3 manifest infrastructure (name,
                             version, host_permissions, action, icons,
                             options_ui, and the permissions common to
                             every target).
  - config.extensionTargets: per-target overlays.  Each entry deep-merges
                             on top of `config.extension` to produce the
                             manifest for that target.  Currently:
                                "chrome"  — service worker + offscreen
                                "firefox" — persistent background page
  - config.menus           : context-menu / keyboard-command bindings.
                             Each entry's optional `command` block is
                             collected into manifest.commands.
  - config.contentScripts  : declarative content_scripts (matches/js/run_at).
  - config.handlers        : runtime-only; ignored here.

Run as:
    build-manifest.py --target chrome  <output-path>
    build-manifest.py --target firefox <output-path>

Used by the Makefile.  The `<output-path>` argument is required; it
points to the manifest.json inside the target's build/ directory.
"""

import argparse
import copy
import json
import sys
from pathlib import Path

EXT_DIR     = Path(__file__).resolve().parent.parent
CONFIG_PATH = EXT_DIR / "config.json"


def die(msg: str) -> "NoReturn":
    sys.stderr.write(f"build-manifest: {msg}\n")
    sys.exit(1)


def load_config() -> dict:
    if not CONFIG_PATH.exists():
        die("missing config.json")
    try:
        return json.loads(CONFIG_PATH.read_text())
    except json.JSONDecodeError as e:
        die(f"config.json is not valid JSON: {e}")


def deep_merge(base: dict, overlay: dict) -> dict:
    """Return a new dict combining BASE with OVERLAY.

    Keys present only in BASE pass through unchanged.  Keys present in
    OVERLAY replace the corresponding BASE keys, except that:
      - Two dicts merge recursively.
      - Two lists concatenate (BASE entries first, then OVERLAY's),
        with duplicates removed while preserving order.
    """
    out = copy.deepcopy(base)
    for key, ov in overlay.items():
        if key in out and isinstance(out[key], dict) and isinstance(ov, dict):
            out[key] = deep_merge(out[key], ov)
        elif key in out and isinstance(out[key], list) and isinstance(ov, list):
            merged = list(out[key])
            for item in ov:
                if item not in merged:
                    merged.append(item)
            out[key] = merged
        else:
            out[key] = copy.deepcopy(ov)
    return out


def validate_content_scripts(scripts: list) -> None:
    if not isinstance(scripts, list):
        die("config.contentScripts must be an array")
    for i, s in enumerate(scripts):
        loc = f"contentScripts[{i}]"
        if not isinstance(s, dict):
            die(f"{loc} must be an object")
        if not isinstance(s.get("matches"), list) or not s["matches"]:
            die(f"{loc}.matches must be a non-empty array")
        if not isinstance(s.get("js"), list) or not s["js"]:
            die(f"{loc}.js must be a non-empty array")


def collect_commands(menus: list) -> dict:
    """Pull each menu's `command` block up into manifest.commands."""
    commands: dict = {}
    for i, m in enumerate(menus):
        cmd = m.get("command")
        if not cmd:
            continue
        if not isinstance(cmd, dict):
            die(f"menus[{i}].command must be an object")
        name = cmd.get("name")
        if not name:
            die(f"menus[{i}].command.name is required")
        if name in commands:
            die(f"duplicate command name across menus: {name!r}")
        entry = {k: v for k, v in cmd.items() if k != "name"}
        commands[name] = entry
    return commands


def main() -> None:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--target", required=True,
                        help="build target name (e.g. 'chrome' or 'firefox')")
    parser.add_argument("output", type=Path,
                        help="path to the generated manifest.json")
    args = parser.parse_args()

    cfg = load_config()

    if "extension" not in cfg or not isinstance(cfg["extension"], dict):
        die("config.json must contain an `extension` object")

    targets = cfg.get("extensionTargets") or {}
    if args.target not in targets:
        die(f"unknown target {args.target!r}; "
            f"known targets: {sorted(targets.keys())}")

    menus = cfg.get("menus", [])
    if not isinstance(menus, list):
        die("config.menus must be an array")

    scripts = cfg.get("contentScripts", [])
    validate_content_scripts(scripts)

    manifest = deep_merge(cfg["extension"], targets[args.target])

    commands = collect_commands(menus)
    if commands:
        manifest["commands"] = commands
    if scripts:
        manifest["content_scripts"] = scripts

    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(
        json.dumps(manifest, indent=2, ensure_ascii=False) + "\n"
    )
    try:
        display = args.output.relative_to(EXT_DIR)
    except ValueError:
        display = args.output
    print(
        f"wrote {display} "
        f"(target={args.target}, "
        f"{len(commands)} commands, {len(scripts)} content_scripts)"
    )


if __name__ == "__main__":
    main()
