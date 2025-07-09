#!/usr/bin/env bash

# Apply transparency to all existing windows based on focus state
# This script helps when picom doesn't apply transparency to existing windows

# Get the currently focused window
FOCUSED=$(xprop -root _NET_ACTIVE_WINDOW | cut -d ' ' -f 5)

# Apply transparency to all windows
for WID in $(xwininfo -root -tree | grep -E '0x[0-9a-f]+' | grep -v 'has no name' | awk '{print $1}'); do
    if [ "$WID" = "$FOCUSED" ]; then
        # Focused window - 95% opacity
        xprop -id "$WID" -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY 0xf2f2f2f2 2>/dev/null
    else
        # Unfocused window - 90% opacity
        xprop -id "$WID" -f _NET_WM_WINDOW_OPACITY 32c -set _NET_WM_WINDOW_OPACITY 0xe6e6e6e6 2>/dev/null
    fi
done

echo "Transparency applied to all windows"