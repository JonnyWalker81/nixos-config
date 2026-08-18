#!/usr/bin/env bash
#
# Wallpaper setup (compatibility shim).
#
# Historically this downloaded a single wallpaper. It now delegates to the
# wallpaper rotation engine so every caller (the wallpaper-setup systemd user
# service, Hyprland's exec-once, awesome's fallback) applies a random wallpaper
# from the curated dark rotation set. See scripts/wallpaper-rotate.sh.
#
# Callers only invoke this once per session, so they get a single random pick.
# Continuous rotation (every 15 min) is driven by the DWM autostart hook, which
# runs `wallpaper-rotate.sh loop`.

export PATH="/run/current-system/sw/bin:/etc/profiles/per-user/$USER/bin:/nix/var/nix/profiles/default/bin:$PATH"

exec "$(dirname "$(readlink -f "$0")")/wallpaper-rotate.sh" once
