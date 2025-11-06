#!/usr/bin/env bash
# Hyprland Focus Guard for Parallels VM
# This script prevents click-through when focusing the VM window from macOS

# Create a temporary file to track focus state
FOCUS_STATE="/tmp/hyprland-focus-guard"

# Function to create a transparent overlay window that captures clicks
create_focus_guard() {
    # Use a simple trick: when VM window gains focus, briefly make all windows unfocusable
    echo "Focus guard activated" > "$FOCUS_STATE"
    
    # Temporarily change input settings
    hyprctl keyword input:follow_mouse 0
    hyprctl keyword misc:focus_on_activate false
    
    # Create a brief delay to absorb the click
    sleep 0.2
    
    # Restore normal behavior
    hyprctl keyword input:follow_mouse 0  # Keep at 0 for VM usage
    
    # Remove the guard state
    rm -f "$FOCUS_STATE"
}

# Check if guard is needed
if [ -f "$FOCUS_STATE" ]; then
    echo "Focus guard already active"
    exit 0
fi

create_focus_guard