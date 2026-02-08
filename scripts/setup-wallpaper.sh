#!/usr/bin/env bash

# Wallpaper setup script
# This script downloads wallpapers and sets them up for the desktop environment

WALLPAPER_DIR="$HOME/.local/share/wallpapers"
# WALLPAPER_URL="https://raw.githubusercontent.com/D3Ext/aesthetic-wallpapers/main/images/pastel-japanese-temple.png"
WALLPAPER_URL="https://raw.githubusercontent.com/basecamp/omakub/refs/heads/master/themes/nord/background.png"
# WALLPAPER_NAME="pastel-japanese-temple.png"
WALLPAPER_NAME="nord.png"
WALLPAPER_PATH="$WALLPAPER_DIR/$WALLPAPER_NAME"
CURRENT_WALLPAPER_LINK="$WALLPAPER_DIR/current"

# Create wallpaper directory if it doesn't exist
mkdir -p "$WALLPAPER_DIR"

# Download wallpaper if it doesn't exist
if [ ! -f "$WALLPAPER_PATH" ]; then
    echo "Downloading wallpaper..."
    curl -L "$WALLPAPER_URL" -o "$WALLPAPER_PATH"
    if [ $? -eq 0 ]; then
        echo "Wallpaper downloaded successfully to $WALLPAPER_PATH"
    else
        echo "Failed to download wallpaper"
        exit 1
    fi
else
    echo "Wallpaper already exists at $WALLPAPER_PATH"
fi

# Create symlink to current wallpaper
ln -sf "$WALLPAPER_PATH" "$CURRENT_WALLPAPER_LINK"

# Set wallpaper using swww for Wayland/Hyprland
if command -v swww &>/dev/null && [ -n "$WAYLAND_DISPLAY" ]; then
    # Initialize swww daemon if not running
    if ! pgrep -f "swww-daemon" > /dev/null; then
        swww init &
        sleep 1
    fi
    swww img "$WALLPAPER_PATH"
    echo "Wallpaper set with swww"
fi

# Set wallpaper using feh for X11 (xmonad, etc.)
if command -v feh &>/dev/null && [ -n "$DISPLAY" ]; then
    feh --bg-fill "$WALLPAPER_PATH"
    echo "Wallpaper set with feh"
fi

# Create .fehbg script for X11 persistence
cat >"$HOME/.fehbg" <<EOF
#!/bin/sh
feh --no-fehbg --bg-fill '$WALLPAPER_PATH'
EOF
chmod +x "$HOME/.fehbg"

echo "Wallpaper setup complete!"
