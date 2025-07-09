#!/usr/bin/env bash
# HiDPI Profile - For 4K/5K displays

echo "Configuring HiDPI display profile..."

# Set resolution if needed (Parallels usually handles this automatically)
if command -v xrandr >/dev/null 2>&1; then
    # Check available resolutions
    if xrandr | grep -q "4096x2160"; then
        xrandr --output Virtual-1 --mode 4096x2160 --rate 60.00 || true
    elif xrandr | grep -q "3840x2160"; then
        xrandr --output Virtual-1 --mode 3840x2160 --rate 60.00 || true
    elif xrandr | grep -q "3816x2049"; then
        xrandr --output Virtual-1 --mode 3816x2049 --rate 59.97 || true
    fi
fi

# Set X11 DPI to 220 (matching your config)
xrdb -merge <<EOF
Xft.dpi: 220
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.hintstyle: hintslight
Xft.lcdfilter: lcddefault
EOF

# Set cursor size
xrdb -merge <<EOF
Xcursor.theme: Adwaita
Xcursor.size: 48
EOF

# Environment variables for HiDPI
export GDK_SCALE=2
export GDK_DPI_SCALE=0.5
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_SCALE_FACTOR=2
export XCURSOR_SIZE=48
export _JAVA_OPTIONS="-Dsun.java2d.uiScale=2"

# Firefox-specific workaround for oversized dialogs
export MOZ_USE_XINPUT2=1
# Note: Firefox should be launched with custom scaling to avoid dialog issues
# Use 'firefox-hidpi' alias or set per-application scaling

# Update GTK settings
if [ -f ~/.config/gtk-3.0/settings.ini ]; then
    sed -i 's/gtk-cursor-theme-size=.*/gtk-cursor-theme-size=48/' ~/.config/gtk-3.0/settings.ini 2>/dev/null || true
fi

# Notify about the change
echo "HiDPI profile applied:"
echo "  - DPI: 220"
echo "  - GDK Scale: 2"
echo "  - QT Scale: 2"
echo "  - Cursor Size: 48"
echo "  - Java UI Scale: 2"

# Update profile name for status display
echo "HiDPI" > /tmp/.current-display-profile