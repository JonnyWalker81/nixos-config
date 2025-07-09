#!/usr/bin/env bash
# Get current display profile for xmobar display

PROFILE_FILE="/tmp/.current-display-profile"
DEFAULT_PROFILE="Standard"

if [ -f "$PROFILE_FILE" ]; then
    PROFILE=$(cat "$PROFILE_FILE")
else
    # Detect current profile based on resolution and DPI
    CURRENT_RES=$(xrandr --current | grep -A 1 "connected primary" | tail -1 | awk '{print $1}')
    CURRENT_DPI=$(xrdb -query | grep -i "xft.dpi" | awk '{print $2}')
    
    if [ -z "$CURRENT_DPI" ]; then
        # Try to get DPI from xdpyinfo if available
        if command -v xdpyinfo >/dev/null 2>&1; then
            CURRENT_DPI=$(xdpyinfo | grep -i "dots per inch" | awk '{print $2}' | cut -d'x' -f1 2>/dev/null || echo "")
        fi
    fi
    
    # Determine profile based on resolution
    case "$CURRENT_RES" in
        "3840x2160"|"4096x2160"|"3816x2049")
            PROFILE="HiDPI"
            ;;
        "2560x1600"|"2880x1800")
            PROFILE="Retina"
            ;;
        "1920x1080"|"1920x1200")
            PROFILE="Standard"
            ;;
        "1366x768"|"1280x720"|"1280x800")
            PROFILE="Present"
            ;;
        "3440x1440"|"2560x1080")
            PROFILE="UltraWide"
            ;;
        *)
            PROFILE="Custom"
            ;;
    esac
fi

# Output format for xmobar: ProfileName (Resolution)
if command -v xrandr >/dev/null 2>&1; then
    RES=$(xrandr --current | grep -A 1 "connected primary" | tail -1 | awk '{print $1}')
    echo "$PROFILE"
else
    echo "$PROFILE"
fi