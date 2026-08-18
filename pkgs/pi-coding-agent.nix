{
  lib,
  writeShellScriptBin,
  nodejs_22,
}:

writeShellScriptBin "pi" ''
  PI_HOME="''${PI_INSTALL_DIR:-$HOME/.pi-coding-agent}"
  PI_BIN="$PI_HOME/node_modules/.bin/pi"
  NPM="${nodejs_22}/bin/npm"
  export PATH="${nodejs_22}/bin:$HOME/.npm-global/bin:$PATH"

  # Redirect global npm installs to writable user directory (Nix store is read-only)
  export NPM_CONFIG_PREFIX="$HOME/.npm-global"
  mkdir -p "$HOME/.npm-global"

  # Handle --upgrade / --reinstall before normal execution
  if [ "''${1:-}" = "--upgrade" ] || [ "''${1:-}" = "--reinstall" ]; then
      echo "Removing $PI_HOME and reinstalling..."
      rm -rf "$PI_HOME"
      shift
  fi

  if [ ! -f "$PI_BIN" ]; then
      echo "pi coding agent not found. Installing from npm..."
      mkdir -p "$PI_HOME"
      echo '{"name":"pi-coding-agent-local","version":"1.0.0","private":true}' > "$PI_HOME/package.json"
      if ! (cd "$PI_HOME" && $NPM install @mariozechner/pi-coding-agent); then
          echo "Failed to install pi coding agent." >&2
          exit 1
      fi
      echo ""
  fi

  exec "$PI_BIN" "$@"
''
