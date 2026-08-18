#!/bin/sh
# DWM autostart script - mirrors XMonad myStartupHook
# Called by DWM's autostart patch from ~/.local/share/dwm/autostart.sh

# Ensure standard utilities are available in all session launch contexts.
PATH="/run/current-system/sw/bin:/etc/profiles/per-user/$USER/bin:/nix/var/nix/profiles/default/bin:$PATH"
export PATH

# Wallpaper rotation: pick a random dark wallpaper now, then rotate every 15 min.
# The loop inherits $DISPLAY from the DWM session (feh needs it). pkill first so a
# DWM restart doesn't stack duplicate rotation loops (mirrors the statusbar below).
pkill -f "wallpaper-rotate.sh" 2>/dev/null
/home/cipher/nixos-config/scripts/wallpaper-rotate.sh loop &

# Start greenclip clipboard daemon (only if installed)
command -v greenclip >/dev/null 2>&1 && greenclip daemon &

# Start emacs daemon
emacs --daemon &

# NOTE: picom is managed by home-manager's services.picom (systemd user service).
# Do NOT start picom here -- it causes a compositor conflict that crashes the
# X session (FATAL ERROR: Another composite manager is already running).

# Kill any existing status bar processes before starting a new one
pkill -f "dwm-statusbar.sh" 2>/dev/null
sleep 0.2

# Start the DWM status bar (xsetroot loop replicating xmobar info)
~/.local/share/dwm/dwm-statusbar.sh &
