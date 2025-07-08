{ lib
, buildFHSEnv
, writeShellScriptBin
, nodejs
}:

let
  opencodeFHS = buildFHSEnv {
    name = "opencode-fhs";
    targetPkgs = pkgs: with pkgs; [
      nodejs
      stdenv.cc.cc.lib
      zlib
      xorg.libX11
      xorg.libXext
      xorg.libXrender
      xorg.libXtst
      xorg.libXi
      fontconfig
      freetype
      libGL
    ];
    runScript = "bash";
  };

  opencode-wrapper = writeShellScriptBin "opencode" ''
    OPENCODE_BIN="$HOME/.opencode/local/node_modules/opencode-linux-arm64/bin/opencode"
    
    if [ ! -f "$OPENCODE_BIN" ]; then
        # Try to find it based on architecture
        case "$(uname -m)" in
            x86_64) PLATFORM="linux-x64" ;;
            aarch64) PLATFORM="linux-arm64" ;;
            *) echo "Unsupported architecture: $(uname -m)"; exit 1 ;;
        esac
        
        OPENCODE_BIN="$HOME/.opencode/local/node_modules/opencode-$PLATFORM/bin/opencode"
    fi
    
    if [ ! -f "$OPENCODE_BIN" ]; then
        echo "OpenCode not found. Please install it first:"
        echo "  mkdir -p ~/.opencode/local && cd ~/.opencode/local"
        echo "  npm init -y && npm install opencode-ai"
        exit 1
    fi
    
    exec ${opencodeFHS}/bin/opencode-fhs -c "$OPENCODE_BIN $*"
  '';

in opencode-wrapper