#!/usr/bin/env bash
# Hyprland VM Focus Helper
# This script helps prevent click-through when focusing VM from host

# Method 1: Create a fullscreen invisible layer that captures the click
create_click_catcher() {
    # Use wlr-layer-shell to create a temporary overlay
    # For now, we'll use a simpler approach with Hyprland commands
    
    # Disable all input temporarily
    hyprctl keyword input:kb_variant ""
    hyprctl keyword general:cursor_inactive_timeout 0.001
    
    # Wait for the click to be absorbed
    sleep 0.1
    
    # Re-enable input
    hyprctl keyword general:cursor_inactive_timeout 0
}

# Method 2: Use Hyprland's built-in features
hyprland_focus_fix() {
    # Get current workspace
    WORKSPACE=$(hyprctl activeworkspace -j | jq -r '.id')
    
    # Create a temporary rule that blocks input
    hyprctl keyword windowrulev2 "suppressevent activate, workspace:$WORKSPACE"
    
    # Wait briefly
    sleep 0.2
    
    # Remove the rule
    hyprctl keyword windowrulev2 "unset, workspace:$WORKSPACE"
}

# Method 3: Simple input toggle
simple_fix() {
    # Save current follow_mouse setting
    FOLLOW=$(hyprctl getoption input:follow_mouse -j | jq -r '.int')
    
    # Completely disable mouse input
    hyprctl keyword input:follow_mouse 0
    hyprctl keyword general:cursor_inactive_timeout 0.01
    
    # Brief pause to absorb the click
    sleep 0.15
    
    # Restore settings
    hyprctl keyword input:follow_mouse "$FOLLOW"
    hyprctl keyword general:cursor_inactive_timeout 0
}

# Run the simple fix by default
simple_fix