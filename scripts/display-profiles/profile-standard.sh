#!/usr/bin/env bash
# Standard Profile - For 1080p/1440p displays

echo "Configuring Standard display profile..."

# Set resolution if needed
if command -v xrandr >/dev/null 2>&1; then
    # Try common standard resolutions
    if xrandr | grep -q "1920x1080"; then
        xrandr --output Virtual-1 --mode 1920x1080 --rate 60.00 || true
    elif xrandr | grep -q "1920x1200"; then
        xrandr --output Virtual-1 --mode 1920x1200 --rate 59.95 || true
    elif xrandr | grep -q "2560x1440"; then
        xrandr --output Virtual-1 --mode 2560x1440 --rate 60.00 || true
    fi
fi

# Set X11 DPI to standard 96
xrdb -merge <<EOF
Xft.dpi: 96
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.hintstyle: hintslight
Xft.lcdfilter: lcddefault
EOF

# Set cursor size
xrdb -merge <<EOF
Xcursor.theme: Adwaita
Xcursor.size: 24
EOF

# Environment variables for standard DPI
export GDK_SCALE=1
export GDK_DPI_SCALE=1
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1
export XCURSOR_SIZE=24
export _JAVA_OPTIONS="-Dsun.java2d.uiScale=1"

# Update GTK settings
if [ -f ~/.config/gtk-3.0/settings.ini ]; then
    sed -i 's/gtk-cursor-theme-size=.*/gtk-cursor-theme-size=24/' ~/.config/gtk-3.0/settings.ini 2>/dev/null || true
fi

# Notify about the change
echo "Standard profile applied:"
echo "  - DPI: 96"
echo "  - GDK Scale: 1"
echo "  - QT Scale: 1"
echo "  - Cursor Size: 24"
echo "  - Java UI Scale: 1"

# Update profile name for status display
echo "Standard" > /tmp/.current-display-profile