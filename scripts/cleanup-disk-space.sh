#!/usr/bin/env bash
# Disk Space Cleanup Script for NixOS
# This script helps free up disk space by cleaning various caches and old data

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print colored output
print_status() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Function to show disk usage
show_disk_usage() {
    echo -e "\n${GREEN}=== Current Disk Usage ===${NC}"
    df -h | grep -E "^/dev|^Filesystem" | grep -v tmpfs
}

# Function to calculate and show freed space
calculate_freed_space() {
    local before=$1
    local after=$2
    local freed=$((before - after))
    local freed_gb=$(echo "scale=2; $freed / 1024 / 1024" | bc)
    echo -e "${GREEN}Freed: ${freed_gb} GB${NC}"
}

# Main cleanup function
main() {
    echo -e "${GREEN}=== NixOS Disk Space Cleanup Script ===${NC}"

    # Show initial disk usage
    show_disk_usage
    INITIAL_USED=$(df / | tail -1 | awk '{print $3}')

    # 1. Nix Store Cleanup
    echo -e "\n${GREEN}=== Cleaning Nix Store ===${NC}"

    if [[ $EUID -eq 0 ]]; then
        print_status "Running as root, cleaning system profiles..."
        nix-collect-garbage --delete-older-than 30d || print_warning "Failed to clean old generations"
        nix-collect-garbage -d || print_warning "Failed to run full garbage collection"
    else
        print_status "Running as user, cleaning user profiles..."
        nix-collect-garbage --delete-older-than 30d || print_warning "Failed to clean old generations"
        nix-collect-garbage -d || print_warning "Failed to run full garbage collection"

        print_warning "Run with sudo to also clean system profiles"
    fi

    # 2. Optimize Nix Store (optional, can be slow)
    read -p "Do you want to optimize the Nix store? This can take a while but may save additional space (y/N): " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        print_status "Optimizing Nix store (this may take several minutes)..."
        if [[ $EUID -eq 0 ]]; then
            nix-store --optimise || print_warning "Failed to optimize store"
        else
            sudo nix-store --optimise || print_warning "Failed to optimize store"
        fi
    fi

    # 3. Docker Cleanup (if Docker is installed)
    if command -v docker &>/dev/null; then
        echo -e "\n${GREEN}=== Cleaning Docker ===${NC}"
        print_status "Cleaning Docker system..."
        docker system prune -a --volumes -f || print_warning "Failed to clean Docker"
    else
        print_status "Docker not found, skipping Docker cleanup"
    fi

    # 4. Clear various caches
    echo -e "\n${GREEN}=== Clearing Cache Directories ===${NC}"

    # Mozilla/Firefox cache
    # if [[ -d "$HOME/.cache/mozilla" ]]; then
    #     print_status "Clearing Mozilla cache..."
    #     rm -rf "$HOME/.cache/mozilla"
    # fi

    # Chrome/Chromium cache
    # if [[ -d "$HOME/.cache/chromium" ]]; then
    #     print_status "Clearing Chromium cache..."
    #     rm -rf "$HOME/.cache/chromium"
    # fi

    # if [[ -d "$HOME/.cache/google-chrome" ]]; then
    #     print_status "Clearing Chrome cache..."
    #     rm -rf "$HOME/.cache/google-chrome"
    # fi

    # Development caches
    if command -v go &>/dev/null; then
        print_status "Clearing Go build cache..."
        go clean -cache || print_warning "Failed to clean Go cache"
    fi

    if [[ -d "$HOME/.cache/yarn" ]]; then
        print_status "Clearing Yarn cache..."
        rm -rf "$HOME/.cache/yarn"
    fi

    if [[ -d "$HOME/.cache/cypress" ]]; then
        print_status "Clearing Cypress cache..."
        rm -rf "$HOME/.cache/cypress"
    fi

    if command -v npm &>/dev/null; then
        print_status "Clearing npm cache..."
        npm cache clean --force || print_warning "Failed to clean npm cache"
    fi

    if command -v cargo &>/dev/null; then
        print_status "Clearing Cargo cache..."
        if command -v cargo-cache &>/dev/null; then
            cargo cache -a || print_warning "Failed to clean Cargo cache"
        else
            print_warning "cargo-cache not installed, cleaning manually..."
            rm -rf "$HOME/.cargo/registry/cache"
            rm -rf "$HOME/.cargo/registry/src"
            rm -rf "$HOME/.cargo/git/checkouts"
        fi
    fi

    # 5. Clear other common caches
    echo -e "\n${GREEN}=== Clearing Other Caches ===${NC}"

    # Thumbnail cache
    if [[ -d "$HOME/.cache/thumbnails" ]]; then
        print_status "Clearing thumbnail cache..."
        rm -rf "$HOME/.cache/thumbnails"
    fi

    # Pip cache
    if [[ -d "$HOME/.cache/pip" ]]; then
        print_status "Clearing pip cache..."
        rm -rf "$HOME/.cache/pip"
    fi

    # 6. Clear temporary files
    # echo -e "\n${GREEN}=== Clearing Temporary Files ===${NC}"

    # User temp files older than 7 days
    # if [[ -d "$HOME/.cache" ]]; then
    #     print_status "Removing old temporary files from ~/.cache..."
    #     find "$HOME/.cache" -type f -atime +7 -delete 2>/dev/null || true
    # fi

    # System temp (if running as root)
    # if [[ $EUID -eq 0 ]]; then
    #     print_status "Clearing old files from /tmp..."
    #     find /tmp -type f -atime +7 -delete 2>/dev/null || true
    #     find /var/tmp -type f -atime +7 -delete 2>/dev/null || true
    # fi

    # 7. Journal cleanup (if running as root)
    if [[ $EUID -eq 0 ]]; then
        echo -e "\n${GREEN}=== Cleaning System Logs ===${NC}"
        print_status "Cleaning journald logs older than 2 weeks..."
        journalctl --vacuum-time=2weeks || print_warning "Failed to clean journal"
    fi

    # Show final disk usage and calculate freed space
    echo -e "\n${GREEN}=== Cleanup Complete ===${NC}"
    show_disk_usage

    FINAL_USED=$(df / | tail -1 | awk '{print $3}')
    calculate_freed_space $INITIAL_USED $FINAL_USED

    echo -e "\n${GREEN}Additional cleanup suggestions:${NC}"
    echo "1. Review and delete old projects in ~/Repositories"
    echo "2. Check ~/Downloads for old files"
    echo "3. Review ~/.local/share for application data"
    echo "4. Consider using 'ncdu' to find large directories"
    echo "5. Run 'sudo nix-store --gc --print-roots' to see what's keeping packages"
}

# Run main function
main
