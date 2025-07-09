final: prev: {
  firefox-hidpi = prev.writeShellScriptBin "firefox-hidpi" ''
    # Firefox wrapper with HiDPI dialog fix
    # This prevents oversized file dialogs on HiDPI displays
    
    # Check if we're in a HiDPI profile
    PROFILE_FILE="/tmp/.current-display-profile"
    if [ -f "$PROFILE_FILE" ]; then
      PROFILE=$(cat "$PROFILE_FILE")
      case "$PROFILE" in
        "HiDPI"|"Retina")
          # Use adjusted scaling for HiDPI displays
          export GDK_SCALE=1
          export GDK_DPI_SCALE=1.8
          export MOZ_USE_XINPUT2=1
          ;;
        *)
          # Use default scaling for other profiles
          ;;
      esac
    fi
    
    exec ${prev.firefox}/bin/firefox "$@"
  '';
}