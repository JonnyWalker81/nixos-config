#!/usr/bin/env bash
# Presentation Profile - Lower resolution for screen sharing/presentations

echo "Configuring Presentation display profile..."

# Set lower resolution for better screen sharing
if command -v xrandr >/dev/null 2>&1; then
    # Try presentation-friendly resolutions
    if xrandr | grep -q "1366x768"; then
        xrandr --output Virtual-1 --mode 1366x768 --rate 59.79 || true
    elif xrandr | grep -q "1280x800"; then
        xrandr --output Virtual-1 --mode 1280x800 --rate 59.81 || true
    elif xrandr | grep -q "1280x720"; then
        xrandr --output Virtual-1 --mode 1280x720 --rate 60.00 || true
    else
        # Fallback to 1024x768 if available
        xrandr --output Virtual-1 --mode 1024x768 --rate 60.00 || true
    fi
fi

# Set X11 DPI to slightly larger for readability
xrdb -merge <<EOF
Xft.dpi: 110
Xft.antialias: true
Xft.hinting: true
Xft.rgba: rgb
Xft.hintstyle: hintfull
Xft.lcdfilter: lcddefault
EOF

# Set cursor size
xrdb -merge <<EOF
Xcursor.theme: Adwaita
Xcursor.size: 32
EOF

# Environment variables for presentation mode
export GDK_SCALE=1
export GDK_DPI_SCALE=1.15
export QT_AUTO_SCREEN_SCALE_FACTOR=0
export QT_SCALE_FACTOR=1.15
export XCURSOR_SIZE=32
export _JAVA_OPTIONS="-Dsun.java2d.uiScale=1.15"

# Update GTK settings
if [ -f ~/.config/gtk-3.0/settings.ini ]; then
    sed -i 's/gtk-cursor-theme-size=.*/gtk-cursor-theme-size=32/' ~/.config/gtk-3.0/settings.ini 2>/dev/null || true
fi

# Notify about the change
echo "Presentation profile applied:"
echo "  - DPI: 110"
echo "  - GDK Scale: 1 (with 1.15 DPI scale)"
echo "  - QT Scale: 1.15"
echo "  - Cursor Size: 32"
echo "  - Java UI Scale: 1.15"
echo "  - Optimized for screen sharing"

# Update profile name for status display
echo "Present" > /tmp/.current-display-profile