#!/usr/bin/env bash
# Enhanced wrapper script for prlcp to prevent crashes and beach balls
# This script forces X11 mode and adds crash recovery

# Force X11 backend to avoid Wayland compatibility issues
export GDK_BACKEND=x11
export QT_QPA_PLATFORM=xcb
export WAYLAND_DISPLAY=""
export XDG_SESSION_TYPE=x11

# Disable hardware acceleration that might cause issues
export LIBGL_ALWAYS_SOFTWARE=1

# Plain text mode if enabled
if [ "${PARALLELS_CLIPBOARD_PLAIN_TEXT}" = "1" ]; then
    export PARALLELS_CLIPBOARD_FORMAT="text"
fi

# Log file for debugging
LOG_FILE="/tmp/prlcp-wrapper.log"
echo "[$(date)] Starting prlcp wrapper" >> "$LOG_FILE"

# Function to clear clipboard on crash
clear_clipboard() {
    echo "[$(date)] Clearing clipboard due to crash" >> "$LOG_FILE"
    if command -v wl-copy &> /dev/null; then
        echo -n "" | wl-copy 2>/dev/null || true
        echo -n "" | wl-copy --primary 2>/dev/null || true
    fi
    if command -v xclip &> /dev/null; then
        echo -n "" | xclip -selection clipboard 2>/dev/null || true
        echo -n "" | xclip -selection primary 2>/dev/null || true
    fi
}

# Trap signals to clean up on crash
trap clear_clipboard SIGTERM SIGINT SIGSEGV

# Circuit breaker - if we've crashed too many times recently, wait longer
CRASH_COUNT_FILE="/tmp/prlcp-crash-count"
CRASH_TIMESTAMP_FILE="/tmp/prlcp-crash-timestamp"

# Check crash frequency
if [ -f "$CRASH_TIMESTAMP_FILE" ]; then
    LAST_CRASH=$(cat "$CRASH_TIMESTAMP_FILE")
    CURRENT_TIME=$(date +%s)
    TIME_DIFF=$((CURRENT_TIME - LAST_CRASH))
    
    if [ $TIME_DIFF -lt 60 ]; then
        # Less than 60 seconds since last crash
        if [ -f "$CRASH_COUNT_FILE" ]; then
            CRASH_COUNT=$(cat "$CRASH_COUNT_FILE")
            CRASH_COUNT=$((CRASH_COUNT + 1))
        else
            CRASH_COUNT=1
        fi
        
        echo "$CRASH_COUNT" > "$CRASH_COUNT_FILE"
        
        if [ $CRASH_COUNT -gt 3 ]; then
            echo "[$(date)] Too many crashes, waiting 30 seconds..." >> "$LOG_FILE"
            sleep 30
            echo "0" > "$CRASH_COUNT_FILE"
        fi
    else
        # Reset crash count if it's been more than a minute
        echo "0" > "$CRASH_COUNT_FILE"
    fi
fi

# Record crash timestamp
date +%s > "$CRASH_TIMESTAMP_FILE"

# Find the actual prlcp binary
PRLCP_BIN=""

# First, try to find it through the Parallels package
if [ -x "/run/current-system/sw/bin/prlcp" ]; then
    PRLCP_BIN="/run/current-system/sw/bin/prlcp"
elif command -v prlcp &> /dev/null; then
    # Use which to find it in PATH
    PRLCP_BIN=$(which prlcp)
else
    # Search in nix store as last resort
    for path in /nix/store/*/bin/prlcp; do
        if [ -x "$path" ] && [ "$path" != "$0" ]; then
            PRLCP_BIN="$path"
            break
        fi
    done
fi

if [ -z "$PRLCP_BIN" ] || [ ! -x "$PRLCP_BIN" ]; then
    echo "[$(date)] ERROR: Could not find prlcp binary" >> "$LOG_FILE"
    # Try to provide more debugging info
    echo "[$(date)] PATH: $PATH" >> "$LOG_FILE"
    echo "[$(date)] Looking for prlcp in /nix/store:" >> "$LOG_FILE"
    ls -la /nix/store/*/bin/prlcp 2>&1 | head -5 >> "$LOG_FILE"
    exit 1
fi

echo "[$(date)] Executing: $PRLCP_BIN" >> "$LOG_FILE"

# Execute prlcp with timeout to prevent hanging
timeout 3600 "$PRLCP_BIN" "$@" 2>&1 | tee -a "$LOG_FILE"
EXIT_CODE=${PIPESTATUS[0]}

if [ $EXIT_CODE -eq 124 ]; then
    echo "[$(date)] prlcp timed out after 1 hour" >> "$LOG_FILE"
    clear_clipboard
elif [ $EXIT_CODE -ne 0 ]; then
    echo "[$(date)] prlcp exited with code $EXIT_CODE" >> "$LOG_FILE"
    clear_clipboard
fi

exit $EXIT_CODE