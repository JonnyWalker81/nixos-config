#!/usr/bin/env bash
# Test clipboard functionality

echo "Testing clipboard functionality..."
echo ""

# Test Wayland clipboard
if command -v wl-copy &> /dev/null && command -v wl-paste &> /dev/null; then
    echo "Testing Wayland clipboard (wl-clipboard):"
    echo "test-wayland" | wl-copy
    result=$(wl-paste)
    if [ "$result" = "test-wayland" ]; then
        echo "✓ Wayland clipboard working"
    else
        echo "✗ Wayland clipboard NOT working"
    fi
else
    echo "✗ wl-clipboard not found"
fi

echo ""

# Test X11 clipboard
if command -v xclip &> /dev/null; then
    echo "Testing X11 clipboard (xclip):"
    echo "test-x11" | xclip -selection clipboard
    result=$(xclip -selection clipboard -o)
    if [ "$result" = "test-x11" ]; then
        echo "✓ X11 clipboard working"
    else
        echo "✗ X11 clipboard NOT working"
    fi
else
    echo "✗ xclip not found"
fi

echo ""

# Check Parallels clipboard service
echo "Checking Parallels clipboard service:"
if systemctl --user is-active prlcp &> /dev/null; then
    echo "✓ prlcp service is active"
else
    echo "✗ prlcp service is NOT active"
fi

echo ""
echo "Current clipboard content:"
if command -v wl-paste &> /dev/null; then
    echo "Wayland: $(wl-paste 2>/dev/null | head -c 50)"
elif command -v xclip &> /dev/null; then
    echo "X11: $(xclip -selection clipboard -o 2>/dev/null | head -c 50)"
fi