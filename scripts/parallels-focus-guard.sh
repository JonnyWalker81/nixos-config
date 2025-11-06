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
        # Truncate clipboard to first 1KB to prevent large data issues
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
    echo "[$(date)] Monitoring Hyprland instance: $HYPRLAND_INSTANCE_SIGNATURE" >> "$LOG_FILE"
    
    # Check if socket exists
    SOCKET_PATH="/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock"
    if [ ! -S "$SOCKET_PATH" ]; then
        echo "[$(date)] ERROR: Hyprland socket not found at $SOCKET_PATH" >> "$LOG_FILE"
        # Try alternative socket path
        SOCKET_PATH="/tmp/hypr/.socket2.sock"
    fi
    
    if [ -S "$SOCKET_PATH" ]; then
        socat -U - UNIX-CONNECT:"$SOCKET_PATH" 2>/dev/null | while read -r line; do
            case "$line" in
                *"activewindow>>"*)
                    # Extract window class and title
                    window_info="${line#activewindow>>}"
                    current_time=$(date +%s.%N)
                    
                    # Read last focus time
                    if [ -f "$LAST_FOCUS_FILE" ]; then
                        last_focus_time=$(cat "$LAST_FOCUS_FILE")
                    else
                        last_focus_time=0
                    fi
                    
                    # Calculate time since last focus
                    time_diff=$(echo "$current_time - $last_focus_time" | bc 2>/dev/null || echo "999")
                    
                    # Only prevent beach ball if focus changed recently (potential VM switch)
                    if (( $(echo "$time_diff < 2" | bc -l 2>/dev/null || echo "0") )); then
                        echo "[$(date)] Rapid focus change detected (${time_diff}s)" >> "$LOG_FILE"
                        prevent_beach_ball "rapid_focus_change"
                    fi
                    
                    # Check if switching to/from Parallels window
                    if is_parallels_window "$window_info"; then
                        echo "[$(date)] Parallels window focused: $window_info" >> "$LOG_FILE"
                        sleep "$BEACH_BALL_PREVENTION_DELAY"
                        prevent_beach_ball "parallels_focus"
                    fi
                    
                    # Update last focus time
                    echo "$current_time" > "$LAST_FOCUS_FILE"
                    ;;
                    
                *"workspace>>"*)
                    # Workspace changed - might be switching virtual desktops
                    echo "[$(date)] Workspace change: $line" >> "$LOG_FILE"
                    prevent_beach_ball "workspace_change"
                    ;;
                    
                *"urgent>>"*)
                    # Urgent window event - might indicate focus steal
                    echo "[$(date)] Urgent event: $line" >> "$LOG_FILE"
                    prevent_beach_ball "urgent_event"
                    ;;
            esac
        done
    else
        echo "[$(date)] ERROR: No valid Hyprland socket found, falling back to clipboard monitoring only" >> "$LOG_FILE"
    fi
else
    echo "[$(date)] Not running under Hyprland, monitoring clipboard only" >> "$LOG_FILE"
fi

# Fallback: Monitor clipboard size as a safety measure
while true; do
    if command -v wl-paste &> /dev/null; then
        size=$(wl-paste 2>/dev/null | wc -c || echo 0)
        if [ "$size" -gt 262144 ]; then
            echo "[$(date)] Large clipboard detected ($size bytes), clearing..." >> "$LOG_FILE"
            echo -n "" | wl-copy 2>/dev/null || true
        fi
    fi
    sleep 5
done