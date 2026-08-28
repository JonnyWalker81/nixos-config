#!/usr/bin/env bash
# Launch the Omarchy polybar (the single top bar for DWM). Started from the DWM
# autostart. Do NOT start picom here — it is managed by services.picom and a
# second compositor crashes the X session.

export PATH="/run/current-system/sw/bin:/etc/profiles/per-user/$USER/bin:/nix/var/nix/profiles/default/bin:$PATH"

# Kill any running instance and wait for it to exit (avoids stacked bars on a
# DWM restart), then relaunch.
pkill -x polybar 2>/dev/null
while pgrep -x polybar >/dev/null; do sleep 0.2; done

polybar main >/tmp/polybar.log 2>&1 &
disown
