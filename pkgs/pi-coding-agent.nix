{
  lib,
  writeShellScriptBin,
  nodejs_22,
}:

writeShellScriptBin "pi" ''
  PI_HOME="''${PI_INSTALL_DIR:-$HOME/.pi-coding-agent}"
  PI_BIN="$PI_HOME/node_modules/.bin/pi"
  NPM="${nodejs_22}/bin/npm"
  export PATH="${nodejs_22}/bin:$PATH"

  # Handle --upgrade / --reinstall before normal execution
  if [ "''${1:-}" = "--upgrade" ] || [ "''${1:-}" = "--reinstall" ]; then
      echo "Removing $PI_HOME and reinstalling..."
      rm -rf "$PI_HOME"
      shift
  fi

  if [ ! -f "$PI_BIN" ]; then
      echo "pi coding agent not found. Installing from npm..."
      mkdir -p "$PI_HOME"
      (cd "$PI_HOME" && $NPM init -y --silent >/dev/null 2>&1 && $NPM install @mariozechner/pi-coding-agent)
      if [ $? -ne 0 ]; then
          echo "Failed to install pi coding agent." >&2
          exit 1
      fi
      echo ""
  fi

  exec "$PI_BIN" "$@"
''
