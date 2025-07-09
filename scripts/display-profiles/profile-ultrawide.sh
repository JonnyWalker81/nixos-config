#!/usr/bin/env bash
# UltraWide Profile - For ultra-wide monitors

echo "Configuring UltraWide display profile..."

# Set ultra-wide resolution if available
if command -v xrandr >/dev/null 2>&1; then
    # Try ultra-wide resolutions
    if xrandr | grep -q "3440x1440"; then
        xrandr --output Virtual-1 --mode 3440x1440 --rate 60.00 || true
    elif xrandr | grep -q "2560x1080"; then
        xrandr --output Virtual-1 --mode 2560x1080 --rate 60.00 || true
    elif xrandr | grep -q "3840x1080"; then
        xrandr --output Virtual-1 --mode 3840x1080 --rate 60.00 || true
    fi
fi

# Set X11 DPI to 110 (good for most ultrawides)
xrdb -merge <<EOF
Xft.dpi: 110
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.hintstyle: hintslight
Xft.lcdfilter: lcddefault
EOF

# Set cursor size
xrdb -merge <<EOF
Xcursor.theme: Adwaita
Xcursor.size: 28
EOF

# Environment variables for UltraWide
export GDK_SCALE=1
export GDK_DPI_SCALE=1.1
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1.1
export XCURSOR_SIZE=28
export _JAVA_OPTIONS="-Dsun.java2d.uiScale=1.1"

# Update GTK settings
if [ -f ~/.config/gtk-3.0/settings.ini ]; then
    sed -i 's/gtk-cursor-theme-size=.*/gtk-cursor-theme-size=28/' ~/.config/gtk-3.0/settings.ini 2>/dev/null || true
fi

# Notify about the change
echo "UltraWide profile applied:"
echo "  - DPI: 110"
echo "  - GDK Scale: 1 (with 1.1 DPI scale)"
echo "  - QT Scale: 1.1"
echo "  - Cursor Size: 28"
echo "  - Java UI Scale: 1.1"
echo "  - Optimized for ultra-wide displays"

# Update profile name for status display
echo "UltraWide" > /tmp/.current-display-profile