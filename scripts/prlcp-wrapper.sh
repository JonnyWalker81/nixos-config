#!/usr/bin/env bash
# Wrapper script for prlcp with crash recovery and beach ball prevention

LOG_FILE="/tmp/prlcp-wrapper.log"
CRASH_COUNT_FILE="/tmp/prlcp-crash-count"
MAX_CRASHES=5
CRASH_WINDOW=60  # seconds
# Try to find prlcp in PATH first
PRLCP_BIN=$(which prlcp 2>/dev/null)

# Fallback to common location
if [ -z "$PRLCP_BIN" ] || [ ! -x "$PRLCP_BIN" ]; then
    PRLCP_BIN="/run/current-system/sw/bin/prlcp"
fi

# Last resort - search nix store
if [ ! -x "$PRLCP_BIN" ]; then
    PRLCP_BIN=$(find /nix/store -name prlcp -type f -executable 2>/dev/null | head -1)
fi

if [ ! -x "$PRLCP_BIN" ]; then
    echo "[$(date)] ERROR: Cannot find prlcp binary!" >> "$LOG_FILE"
    exit 1
fi

echo "[$(date)] Starting prlcp wrapper with binary: $PRLCP_BIN" >> "$LOG_FILE"

# Function to reset crash counter
reset_crash_counter() {
    echo "0:$(date +%s)" > "$CRASH_COUNT_FILE"
}

# Function to check crash rate
check_crash_rate() {
    if [ ! -f "$CRASH_COUNT_FILE" ]; then
        reset_crash_counter
        return 0
    fi
    
    local data=$(cat "$CRASH_COUNT_FILE")
    local count=${data%%:*}
    local timestamp=${data##*:}
    local current_time=$(date +%s)
    local time_diff=$((current_time - timestamp))
    
    if [ $time_diff -gt $CRASH_WINDOW ]; then
        # Reset counter if outside window
        reset_crash_counter
        return 0
    fi
    
    if [ $count -ge $MAX_CRASHES ]; then
        echo "[$(date)] CIRCUIT BREAKER: Too many crashes ($count in ${time_diff}s), exiting" >> "$LOG_FILE"
        
        # Clear clipboard as a safety measure
        if command -v wl-copy &> /dev/null; then
            echo -n "" | wl-copy 2>/dev/null || true
        fi
        if command -v xclip &> /dev/null; then
            echo -n "" | xclip -selection clipboard 2>/dev/null || true
        fi
        
        # Wait before allowing restart
        sleep 30
        reset_crash_counter
        return 1
    fi
    
    # Increment counter
    echo "$((count + 1)):$timestamp" > "$CRASH_COUNT_FILE"
    return 0
}

# Function to prepare environment
prepare_environment() {
    # Clear any large clipboard content before starting
    if command -v wl-paste &> /dev/null; then
        local size=$(wl-paste 2>/dev/null | wc -c || echo 0)
        if [ "$size" -gt 262144 ]; then
            echo "[$(date)] Clearing large clipboard content ($size bytes)" >> "$LOG_FILE"
            echo -n "" | wl-copy 2>/dev/null || true
        fi
    fi
    
    # Ensure X11 compatibility
    if [ -z "$DISPLAY" ]; then
        export DISPLAY=":0"
    fi
    
    # Force X11 backend
    export GDK_BACKEND=x11
    export QT_QPA_PLATFORM=xcb
    export LIBGL_ALWAYS_SOFTWARE=1
    
    # Clear Wayland to avoid conflicts
    unset WAYLAND_DISPLAY
    export XDG_SESSION_TYPE=x11
}

# Main wrapper loop
while true; do
    if ! check_crash_rate; then
        echo "[$(date)] Circuit breaker activated, waiting..." >> "$LOG_FILE"
        sleep 30
        continue
    fi
    
    prepare_environment
    
    echo "[$(date)] Starting prlcp..." >> "$LOG_FILE"
    
    # Run prlcp with timeout and capture exit code
    timeout 3600 "$PRLCP_BIN" 2>&1 | while read -r line; do
        echo "[$(date)] prlcp: $line" >> "$LOG_FILE"
    done
    
    EXIT_CODE=$?
    
    if [ $EXIT_CODE -eq 124 ]; then
        echo "[$(date)] prlcp timed out after 1 hour, restarting..." >> "$LOG_FILE"
        reset_crash_counter
    elif [ $EXIT_CODE -ne 0 ]; then
        echo "[$(date)] prlcp crashed with exit code $EXIT_CODE" >> "$LOG_FILE"
        
        # Clear clipboard on crash
        if command -v wl-copy &> /dev/null; then
            echo -n "" | wl-copy 2>/dev/null || true
        fi
        
        # Brief pause before restart
        sleep 2
    else
        echo "[$(date)] prlcp exited normally" >> "$LOG_FILE"
        reset_crash_counter
    fi
    
    # Always wait a bit before restarting
    sleep 1
done