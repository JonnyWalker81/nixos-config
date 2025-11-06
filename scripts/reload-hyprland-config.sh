#!/usr/bin/env bash
# Script to reload Hyprland configuration after NixOS rebuild

echo "Reloading Hyprland configuration..."

# First, rebuild NixOS configuration
cd ~/nixos-config
sudo nixos-rebuild switch --flake .#vm-msi-nixos

# Then reload Hyprland
hyprctl reload

echo "Hyprland configuration reloaded!"
echo ""
echo "The click-through issue workaround:"
echo "1. follow_mouse = 2 - Windows get focus on hover but don't raise automatically"
echo "2. Use Alt+F to focus window under cursor"
echo "3. Use Ctrl+Click to focus without passing the click"
echo ""
echo "If you still experience click-through:"
echo "- Move mouse to the VM window (it will get focus)"
echo "- Press Alt+F to properly focus it"
echo "- Then click normally inside the VM"