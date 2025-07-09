#!/usr/bin/env bash
# Retina Profile - For MacBook Retina displays (2560x1600, 2880x1800)

echo "Configuring Retina display profile..."

# Set resolution if needed
if command -v xrandr >/dev/null 2>&1; then
    # Try retina resolutions
    if xrandr | grep -q "2880x1800"; then
        xrandr --output Virtual-1 --mode 2880x1800 --rate 60.00 || true
    elif xrandr | grep -q "2560x1600"; then
        xrandr --output Virtual-1 --mode 2560x1600 --rate 59.99 || true
    fi
fi

# Set X11 DPI to 180 (good for retina)
xrdb -merge <<EOF
Xft.dpi: 180
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.hintstyle: hintslight
Xft.lcdfilter: lcddefault
EOF

# Set cursor size
xrdb -merge <<EOF
Xcursor.theme: Adwaita
Xcursor.size: 36
EOF

# Environment variables for Retina
export GDK_SCALE=2
export GDK_DPI_SCALE=0.75
export QT_AUTO_SCREEN_SCALE_FACTOR=1
export QT_SCALE_FACTOR=1.5
export XCURSOR_SIZE=36
export _JAVA_OPTIONS="-Dsun.java2d.uiScale=1.5"

# Update GTK settings
if [ -f ~/.config/gtk-3.0/settings.ini ]; then
    sed -i 's/gtk-cursor-theme-size=.*/gtk-cursor-theme-size=36/' ~/.config/gtk-3.0/settings.ini 2>/dev/null || true
fi

# Notify about the change
echo "Retina profile applied:"
echo "  - DPI: 180"
echo "  - GDK Scale: 2 (with 0.75 DPI scale)"
echo "  - QT Scale: 1.5"
echo "  - Cursor Size: 36"
echo "  - Java UI Scale: 1.5"

# Update profile name for status display
echo "Retina" > /tmp/.current-display-profile