#!/usr/bin/env bash
# Firefox HiDPI fix for file dialogs

# This script launches Firefox with adjusted scaling to prevent oversized dialogs
# while maintaining good appearance on HiDPI displays

# Set Firefox-specific environment variables
export MOZ_ENABLE_WAYLAND=0  # Force X11 for better dialog handling
export GDK_SCALE=1           # Don't double-scale GTK dialogs
export GDK_DPI_SCALE=1.8     # Use DPI scaling instead for better control
export MOZ_USE_XINPUT2=1     # Better input handling

# Launch Firefox with these settings
exec firefox "$@"