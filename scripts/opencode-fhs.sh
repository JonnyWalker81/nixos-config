#!/usr/bin/env bash

# OpenCode wrapper using steam-run for FHS environment
# This allows running dynamically linked binaries on NixOS

OPENCODE_BIN="$HOME/.opencode/local/node_modules/opencode-linux-arm64/bin/opencode"

if [ ! -f "$OPENCODE_BIN" ]; then
    echo "OpenCode not found. Please run: /home/cipher/nixos-config/scripts/install-opencode.sh"
    exit 1
fi

# Check if steam-run is available
if command -v steam-run &> /dev/null; then
    exec steam-run "$OPENCODE_BIN" "$@"
else
    echo "steam-run not found. Install it with: nix-env -iA nixos.steam-run"
    exit 1
fi