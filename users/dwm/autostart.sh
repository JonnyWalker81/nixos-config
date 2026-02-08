#!/bin/sh
# DWM autostart script - mirrors XMonad myStartupHook
# Called by DWM's autostart patch from ~/.local/share/dwm/autostart.sh

# Set wallpaper using feh (matching XMonad spawnOnce)
feh --no-fehbg --bg-fill /home/cipher/.local/share/wallpapers/nord.png &

# Start greenclip clipboard daemon
greenclip daemon &

# Start emacs daemon
emacs --daemon &

# Start picom compositor (matching XMonad picom service)
picom --daemon &

# Kill any existing status bar processes before starting a new one
pkill -f "dwm-statusbar.sh" 2>/dev/null
sleep 0.2

# Start the DWM status bar (xsetroot loop replicating xmobar info)
~/.local/share/dwm/dwm-statusbar.sh &
