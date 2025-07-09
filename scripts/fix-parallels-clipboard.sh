#!/usr/bin/env bash

# Script to fix Parallels clipboard integration

echo "Fixing Parallels clipboard integration..."

# Reset failed services
echo "Resetting failed services..."
systemctl --user reset-failed prlcp.service 2>/dev/null || true
systemctl --user reset-failed prlcc.service 2>/dev/null || true

# Restart Parallels Tools daemon
echo "Restarting Parallels Tools daemon..."
sudo systemctl restart prltoolsd

# Give it a moment to initialize
sleep 2

# Try to start user services
echo "Starting user clipboard services..."
systemctl --user start prlcp.service 2>/dev/null || true
systemctl --user start prlcc.service 2>/dev/null || true

# Test clipboard
echo "Testing clipboard..."
echo "Parallels clipboard test" | xclip -selection clipboard 2>/dev/null

echo "Done! Try copying and pasting between VM and host."