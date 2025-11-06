#!/usr/bin/env bash
# Script to fix Parallels clipboard hanging issues

set -e

echo "Fixing Parallels clipboard hanging issues..."

# Stop clipboard service
echo "Stopping prlcp service..."
systemctl --user stop prlcp || true

# Kill any hanging prlcp processes
echo "Killing any hanging prlcp processes..."
pkill -9 prlcp || true

# Clear clipboard to prevent large data issues
echo "Clearing clipboard..."
if command -v wl-copy &> /dev/null; then
    echo -n "" | wl-copy
    echo -n "" | wl-copy --primary
elif command -v xclip &> /dev/null; then
    echo -n "" | xclip -selection clipboard
    echo -n "" | xclip -selection primary
fi

# Wait a moment
sleep 2

# Restart the service
echo "Restarting prlcp service..."
systemctl --user start prlcp

echo "Parallels clipboard service restarted successfully."
echo ""
echo "If you continue to experience issues:"
echo "1. Consider disabling 'Preserve text formatting' in Parallels settings"
echo "2. Temporarily disable clipboard sharing when using problematic applications"
echo "3. Run this script when you experience hangs"