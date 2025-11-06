#!/usr/bin/env bash
# Script to focus window under cursor without passing the click through
# This helps with VM applications where click-through causes issues

# Get the window under the cursor
WINDOW_INFO=$(hyprctl cursorpos -j)
X=$(echo "$WINDOW_INFO" | jq -r '.x')
Y=$(echo "$WINDOW_INFO" | jq -r '.y')

# Focus the window at cursor position
hyprctl dispatch focuswindow mouse

# Optional: You can add a small delay here if needed
# sleep 0.1

# For debugging
# echo "Focused window at position: $X, $Y"