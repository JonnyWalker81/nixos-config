#!/usr/bin/env bash
# Main display profile switcher script

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROFILE_FILE="/tmp/.current-display-profile"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Function to display usage
usage() {
    echo "Usage: $0 [profile|list|auto|current]"
    echo ""
    echo "Profiles:"
    echo "  hidpi       - High DPI settings for 4K/5K displays"
    echo "  retina      - Retina display settings (2560x1600, 2880x1800)"
    echo "  standard    - Standard 1080p/1440p settings"
    echo "  present     - Presentation mode (lower resolution)"
    echo "  ultrawide   - Ultra-wide monitor support"
    echo ""
    echo "Commands:"
    echo "  list        - List available profiles"
    echo "  auto        - Auto-detect and apply best profile"
    echo "  current     - Show current profile"
    echo ""
    exit 1
}

# Function to check if running under X11
is_x11() {
    [ -n "$DISPLAY" ] && command -v xrandr >/dev/null 2>&1
}

# Function to check if running under Wayland
is_wayland() {
    [ -n "$WAYLAND_DISPLAY" ]
}

# Function to apply a profile
apply_profile() {
    local profile="$1"
    local profile_script="$SCRIPT_DIR/profile-${profile}.sh"
    
    if [ ! -f "$profile_script" ]; then
        echo -e "${RED}Error: Profile '$profile' not found!${NC}"
        exit 1
    fi
    
    echo -e "${BLUE}Applying display profile: ${YELLOW}$profile${NC}"
    
    # Source the profile
    source "$profile_script"
    
    # Save current profile
    echo "$profile" > "$PROFILE_FILE"
    
    echo -e "${GREEN}Display profile '$profile' applied successfully!${NC}"
    
    # Restart picom if running to apply any transparency changes
    if pgrep picom >/dev/null; then
        killall picom 2>/dev/null || true
        sleep 0.5
        picom &
    fi
}

# Function to list available profiles
list_profiles() {
    echo -e "${BLUE}Available display profiles:${NC}"
    for profile_file in "$SCRIPT_DIR"/profile-*.sh; do
        if [ -f "$profile_file" ]; then
            profile_name=$(basename "$profile_file" | sed 's/profile-//;s/.sh//')
            echo "  - $profile_name"
        fi
    done
}

# Function to show current profile
show_current() {
    if [ -f "$PROFILE_FILE" ]; then
        current=$(cat "$PROFILE_FILE")
        echo -e "${BLUE}Current profile: ${YELLOW}$current${NC}"
    else
        echo -e "${YELLOW}No profile set. Run with 'auto' to detect.${NC}"
    fi
    
    if is_x11; then
        echo -e "\n${BLUE}Current display configuration:${NC}"
        xrandr --current | grep -A 1 "connected"
        echo -e "\n${BLUE}Current DPI:${NC}"
        xrdb -query | grep -i dpi || echo "DPI not set in X resources"
    fi
}

# Function to auto-detect and apply best profile
auto_detect() {
    if ! is_x11; then
        echo -e "${YELLOW}Auto-detection only works in X11 sessions${NC}"
        exit 1
    fi
    
    # Get current resolution
    resolution=$(xrandr --current | grep -A 1 "connected primary" | tail -1 | awk '{print $1}')
    
    echo -e "${BLUE}Detected resolution: ${YELLOW}$resolution${NC}"
    
    case "$resolution" in
        "3840x2160"|"4096x2160"|"3816x2049"|"5120x2880")
            apply_profile "hidpi"
            ;;
        "2560x1600"|"2880x1800")
            apply_profile "retina"
            ;;
        "1920x1080"|"1920x1200"|"1920x1440")
            apply_profile "standard"
            ;;
        "1366x768"|"1280x720"|"1280x800"|"1024x768")
            apply_profile "present"
            ;;
        "3440x1440"|"2560x1080"|"3840x1080")
            apply_profile "ultrawide"
            ;;
        *)
            echo -e "${YELLOW}Unknown resolution: $resolution${NC}"
            echo -e "${YELLOW}Applying standard profile as fallback${NC}"
            apply_profile "standard"
            ;;
    esac
}

# Main script logic
case "${1:-}" in
    list)
        list_profiles
        ;;
    auto)
        auto_detect
        ;;
    current)
        show_current
        ;;
    hidpi|retina|standard|present|ultrawide)
        apply_profile "$1"
        ;;
    *)
        usage
        ;;
esac