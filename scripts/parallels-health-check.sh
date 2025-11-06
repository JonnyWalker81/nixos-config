#!/usr/bin/env bash
# Parallels clipboard health check and diagnostic script

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

echo "============================================"
echo "Parallels Clipboard Health Check"
echo "============================================"
echo ""

# Function to print status
print_status() {
    local status=$1
    local message=$2
    if [ "$status" = "OK" ]; then
        echo -e "${GREEN}[✓]${NC} $message"
    elif [ "$status" = "WARN" ]; then
        echo -e "${YELLOW}[!]${NC} $message"
    else
        echo -e "${RED}[✗]${NC} $message"
    fi
}

# Function to check service status
check_service() {
    local service=$1
    if systemctl --user is-active --quiet "$service"; then
        print_status "OK" "$service is running"
        return 0
    else
        print_status "FAIL" "$service is not running"
        return 1
    fi
}

# Function to check clipboard size
check_clipboard_size() {
    if command -v wl-paste &> /dev/null; then
        local size=$(wl-paste 2>/dev/null | wc -c || echo 0)
        if [ "$size" -gt 262144 ]; then
            print_status "WARN" "Clipboard size is large: $size bytes (may cause beach balls)"
            return 1
        else
            print_status "OK" "Clipboard size: $size bytes"
            return 0
        fi
    elif command -v xclip &> /dev/null; then
        local size=$(xclip -selection clipboard -o 2>/dev/null | wc -c || echo 0)
        if [ "$size" -gt 262144 ]; then
            print_status "WARN" "Clipboard size is large: $size bytes (may cause beach balls)"
            return 1
        else
            print_status "OK" "Clipboard size: $size bytes"
            return 0
        fi
    else
        print_status "WARN" "No clipboard tools found (wl-paste or xclip)"
        return 1
    fi
}

# Function to check prlcp crashes
check_prlcp_crashes() {
    local crash_count=$(journalctl --user -u prlcp --since "1 hour ago" 2>/dev/null | grep -c "Started Parallels")
    if [ "$crash_count" -gt 10 ]; then
        print_status "FAIL" "prlcp crashed $crash_count times in the last hour"
        return 1
    elif [ "$crash_count" -gt 5 ]; then
        print_status "WARN" "prlcp restarted $crash_count times in the last hour"
        return 1
    else
        print_status "OK" "prlcp restart count: $crash_count (last hour)"
        return 0
    fi
}

# Function to test clipboard functionality
test_clipboard() {
    local test_string="clipboard_test_$$"
    
    if command -v wl-copy &> /dev/null && command -v wl-paste &> /dev/null; then
        echo -n "$test_string" | wl-copy 2>/dev/null
        sleep 0.5
        local result=$(timeout 2 wl-paste 2>/dev/null)
        if [ "$result" = "$test_string" ]; then
            print_status "OK" "Clipboard test passed (Wayland)"
            echo -n "" | wl-copy 2>/dev/null  # Clear test
            return 0
        else
            print_status "FAIL" "Clipboard test failed (Wayland)"
            return 1
        fi
    elif command -v xclip &> /dev/null; then
        echo -n "$test_string" | xclip -selection clipboard 2>/dev/null
        sleep 0.5
        local result=$(timeout 2 xclip -selection clipboard -o 2>/dev/null)
        if [ "$result" = "$test_string" ]; then
            print_status "OK" "Clipboard test passed (X11)"
            echo -n "" | xclip -selection clipboard 2>/dev/null  # Clear test
            return 0
        else
            print_status "FAIL" "Clipboard test failed (X11)"
            return 1
        fi
    else
        print_status "SKIP" "Cannot test clipboard (no tools available)"
        return 1
    fi
}

# Function to check environment
check_environment() {
    echo ""
    echo "Environment Variables:"
    echo "----------------------"
    echo "DISPLAY: ${DISPLAY:-<not set>}"
    echo "WAYLAND_DISPLAY: ${WAYLAND_DISPLAY:-<not set>}"
    echo "XDG_SESSION_TYPE: ${XDG_SESSION_TYPE:-<not set>}"
    echo "HYPRLAND_INSTANCE_SIGNATURE: ${HYPRLAND_INSTANCE_SIGNATURE:-<not set>}"
    echo ""
}

# Function to auto-fix issues
auto_fix() {
    local fixed=false
    
    echo ""
    echo "Attempting automatic fixes..."
    echo "-----------------------------"
    
    # Clear large clipboard
    if ! check_clipboard_size > /dev/null 2>&1; then
        echo "Clearing large clipboard content..."
        if command -v wl-copy &> /dev/null; then
            echo -n "" | wl-copy 2>/dev/null
            echo -n "" | wl-copy --primary 2>/dev/null
        fi
        if command -v xclip &> /dev/null; then
            echo -n "" | xclip -selection clipboard 2>/dev/null
            echo -n "" | xclip -selection primary 2>/dev/null
        fi
        fixed=true
    fi
    
    # Restart crashed services
    if ! systemctl --user is-active --quiet prlcp; then
        echo "Restarting prlcp service..."
        systemctl --user restart prlcp 2>/dev/null
        fixed=true
    fi
    
    if [ "$fixed" = true ]; then
        print_status "OK" "Automatic fixes applied"
    else
        print_status "OK" "No fixes needed"
    fi
}

# Main health check
echo "Service Status:"
echo "---------------"
check_service "prlcp"
check_service "parallels-clipboard-monitor" 2>/dev/null || print_status "WARN" "Clipboard monitor not configured"
check_service "parallels-focus-guard" 2>/dev/null || print_status "WARN" "Focus guard not configured"

echo ""
echo "Clipboard Status:"
echo "-----------------"
check_clipboard_size
test_clipboard

echo ""
echo "System Health:"
echo "--------------"
check_prlcp_crashes

check_environment

# Check for recent errors
echo "Recent Errors (last 10 minutes):"
echo "---------------------------------"
errors=$(journalctl --user -u prlcp --since "10 minutes ago" 2>/dev/null | grep -i "error\|fail\|crash" | tail -5)
if [ -n "$errors" ]; then
    echo "$errors"
else
    print_status "OK" "No recent errors found"
fi

# Offer auto-fix if issues detected
if [ "$1" = "--fix" ] || [ "$1" = "-f" ]; then
    auto_fix
else
    echo ""
    echo "Run with --fix to attempt automatic repairs"
fi

echo ""
echo "============================================"
echo "Health check complete"
echo "============================================"