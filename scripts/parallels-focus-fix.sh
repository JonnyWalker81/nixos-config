#!/usr/bin/env bash
# Script to handle Parallels VM focus issues
# This script temporarily disables mouse input when the VM regains focus

# Check if we're in a Parallels VM
if ! lspci | grep -q "Parallels"; then
    echo "Not running in Parallels VM, exiting..."
    exit 0
fi

# Function to temporarily disable mouse clicks
disable_clicks() {
    # Get current follow_mouse setting
    CURRENT_FOLLOW=$(hyprctl getoption input:follow_mouse | grep -oP 'int: \K\d+')
    
    # Disable mouse following completely
    hyprctl keyword input:follow_mouse 0
    
    # Wait a short time for the focus event to settle
    sleep 0.3
    
    # Restore previous setting
    hyprctl keyword input:follow_mouse "$CURRENT_FOLLOW"
}

# Monitor for focus changes
if [ "$1" == "monitor" ]; then
    # This would run continuously to monitor focus changes
    # For now, we'll just provide the manual command
    echo "Run 'hyprctl dispatch exec /home/cipher/nixos-config/scripts/parallels-focus-fix.sh' when experiencing issues"
else
    disable_clicks
fi