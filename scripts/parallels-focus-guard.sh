#!/usr/bin/env bash
# Parallels Focus Guard - Prevents beach balls when switching between VM and host
# This script monitors focus events and applies preventive measures

LOG_FILE="/tmp/parallels-focus-guard.log"
LAST_FOCUS_FILE="/tmp/parallels-last-focus"
BEACH_BALL_PREVENTION_DELAY="0.3"

echo "[$(date)] Starting Parallels Focus Guard" >> "$LOG_FILE"

# Function to prevent beach balls
prevent_beach_ball() {
    local event_type="$1"
    echo "[$(date)] Preventing beach ball for event: $event_type" >> "$LOG_FILE"
    
    # Method 1: Clear clipboard on focus change to prevent sync issues
    if command -v wl-copy &> /dev/null; then
        timeout 0.5 wl-paste 2>/dev/null | head -c 1024 | wl-copy 2>/dev/null || true
    fi
    
    # Method 2: Briefly disable/enable prlcp to reset state
    systemctl --user restart prlcp 2>/dev/null || true
    
    # Method 3: Force a small clipboard operation to "prime" the system
    echo -n "." | wl-copy 2>/dev/null || true
    sleep 0.1
    echo -n "" | wl-copy 2>/dev/null || true
}

# Function to check if window is Parallels-related
is_parallels_window() {
    local window_info="$1"
    if [[ "$window_info" =~ "Parallels" ]] || [[ "$window_info" =~ "prl" ]]; then
        return 0
    fi
    return 1
}

# Monitor Hyprland socket for focus events
if [ -n "$HYPRLAND_INSTANCE_SIGNATURE" ]; then
    socat -U - UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock 2>/dev/null | while read -r line; do
        case "$line" in
            *"activewindow>>"*)
                # Extract window class and title
                window_info="${line#activewindow>>}"
                current_time=$(date +%s.%N)
                
                # Read last focus time
                if [ -f "$LAST_FOCUS_FILE" ]; then
                    last_focus_time=$(cat "$LAST_FOCUS_FILE")
                    time_diff=$(echo "$current_time - $last_focus_time" | bc)
                    
                    # If focus changed very quickly (likely from macOS click), apply prevention
                    if (( $(echo "$time_diff < 0.5" | bc -l) )); then
                        echo "[$(date)] Rapid focus change detected ($time_diff seconds)" >> "$LOG_FILE"
                        prevent_beach_ball "rapid_focus_change"
                    fi
                fi
                
                # Update last focus time
                echo "$current_time" > "$LAST_FOCUS_FILE"
                
                # Check if this is a Parallels-related window
                if is_parallels_window "$window_info"; then
                    echo "[$(date)] Parallels window focused: $window_info" >> "$LOG_FILE"
                    sleep "$BEACH_BALL_PREVENTION_DELAY"
                    prevent_beach_ball "parallels_window"
                fi
                ;;
                
            *"workspace>>"*)
                # Workspace change might indicate switching from host
                echo "[$(date)] Workspace changed" >> "$LOG_FILE"
                sleep "$BEACH_BALL_PREVENTION_DELAY"
                prevent_beach_ball "workspace_change"
                ;;
        esac
    done
else
    echo "[$(date)] ERROR: Not running under Hyprland" >> "$LOG_FILE"
    exit 1
fi