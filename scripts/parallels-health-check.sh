#!/usr/bin/env bash
# Parallels Health Check - Detects and fixes common issues that cause beach balls

set -euo pipefail

LOG_FILE="/tmp/parallels-health-check.log"
ISSUES_FOUND=0

log() {
    echo "[$(date)] $1" | tee -a "$LOG_FILE"
}

check_service() {
    local service="$1"
    if systemctl --user is-active "$service" &>/dev/null; then
        log "✓ $service is running"
        return 0
    else
        log "✗ $service is not running"
        ISSUES_FOUND=$((ISSUES_FOUND + 1))
        return 1
    fi
}

check_process_memory() {
    local process="$1"
    local max_mem_mb="$2"
    
    local pid=$(pgrep -f "$process" | head -1)
    if [ -n "$pid" ]; then
        local mem_kb=$(ps -o rss= -p "$pid" 2>/dev/null || echo 0)
        local mem_mb=$((mem_kb / 1024))
        
        if [ "$mem_mb" -gt "$max_mem_mb" ]; then
            log "✗ $process using too much memory: ${mem_mb}MB (max: ${max_mem_mb}MB)"
            ISSUES_FOUND=$((ISSUES_FOUND + 1))
            return 1
        else
            log "✓ $process memory usage OK: ${mem_mb}MB"
            return 0
        fi
    fi
    return 0
}

check_clipboard_size() {
    local size=0
    if command -v wl-paste &> /dev/null; then
        size=$(wl-paste 2>/dev/null | wc -c || echo 0)
    elif command -v xclip &> /dev/null; then
        size=$(xclip -selection clipboard -o 2>/dev/null | wc -c || echo 0)
    fi
    
    # Ensure size is not empty
    if [ -z "$size" ]; then
        size=0
    fi
    
    local size_kb=$((size / 1024))
    if [ "$size" -gt 262144 ]; then  # 256KB
        log "✗ Clipboard content too large: ${size_kb}KB"
        ISSUES_FOUND=$((ISSUES_FOUND + 1))
        return 1
    else
        log "✓ Clipboard size OK: ${size_kb}KB"
        return 0
    fi
}

fix_issues() {
    if [ "$ISSUES_FOUND" -eq 0 ]; then
        return 0
    fi
    
    log ""
    log "Attempting to fix $ISSUES_FOUND issues..."
    
    # Stop all services
    log "Stopping Parallels services..."
    systemctl --user stop prlcp 2>/dev/null || true
    systemctl --user stop parallels-clipboard-monitor 2>/dev/null || true
    systemctl --user stop parallels-focus-guard 2>/dev/null || true
    
    # Kill any hanging processes
    pkill -9 prlcp 2>/dev/null || true
    
    # Clear clipboard
    log "Clearing clipboard..."
    if command -v wl-copy &> /dev/null; then
        echo -n "" | wl-copy
        echo -n "" | wl-copy --primary
    fi
    if command -v xclip &> /dev/null; then
        echo -n "" | xclip -selection clipboard
        echo -n "" | xclip -selection primary
    fi
    
    # Wait a moment
    sleep 2
    
    # Restart services
    log "Restarting services..."
    systemctl --user start prlcp
    systemctl --user start parallels-clipboard-monitor
    systemctl --user start parallels-focus-guard 2>/dev/null || true
    
    log "Fix attempts completed"
}

# Main health check
log "=== Parallels Health Check Started ==="

# Check services
check_service "prlcp"
check_service "parallels-clipboard-monitor"
check_service "parallels-focus-guard" || true  # Optional service

# Check memory usage
check_process_memory "prlcp" 256

# Check clipboard size
check_clipboard_size

# Check for crash logs
if [ -f "/tmp/prlcp-crash-count" ]; then
    crash_count=$(cat "/tmp/prlcp-crash-count")
    if [ "$crash_count" -gt 0 ]; then
        log "✗ Recent prlcp crashes detected: $crash_count"
        ISSUES_FOUND=$((ISSUES_FOUND + 1))
    fi
fi

# Summary
log ""
if [ "$ISSUES_FOUND" -eq 0 ]; then
    log "✓ All checks passed - Parallels integration is healthy"
else
    log "✗ Found $ISSUES_FOUND issues"
    
    if [ "${1:-}" = "--fix" ]; then
        fix_issues
    else
        log "Run with --fix to attempt automatic fixes"
    fi
fi

exit "$ISSUES_FOUND"