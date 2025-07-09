{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.parallels.display;
  
  # Parallels display optimization script
  parallelsDisplayScript = pkgs.writeShellScript "parallels-display-optimize" ''
    #!/usr/bin/env bash
    
    # Enable Parallels Tools video features if available
    if [ -x /usr/bin/prlcc ]; then
      # Set coherence mode settings for better display handling
      prlcc set --video-memory-size max 2>/dev/null || true
      prlcc set --3d-acceleration highest 2>/dev/null || true
      prlcc set --vertical-sync on 2>/dev/null || true
    fi
    
    # Ensure the virtual display is properly configured
    if command -v xrandr >/dev/null 2>&1 && [ -n "$DISPLAY" ]; then
      # Force detection of all available modes
      xrandr --output Virtual-1 --auto
      
      # If dynamic resolution is enabled, set up monitoring
      if [ "${toString cfg.dynamicResolution}" = "true" ]; then
        # Create a marker file for dynamic resolution
        touch /tmp/.parallels-dynamic-resolution
      fi
    fi
    
    # Optimize performance settings
    if [ -f /proc/sys/vm/swappiness ]; then
      echo 10 > /proc/sys/vm/swappiness
    fi
  '';
  
  # Resolution monitoring script for dynamic adjustments
  resolutionMonitor = pkgs.writeShellScript "parallels-resolution-monitor" ''
    #!/usr/bin/env bash
    
    LAST_RES=""
    PROFILE_SWITCHER="${../scripts/display-profiles/display-switcher.sh}"
    
    while true; do
      if command -v xrandr >/dev/null 2>&1 && [ -n "$DISPLAY" ]; then
        CURRENT_RES=$(xrandr --current | grep -A 1 "connected primary" | tail -1 | awk '{print $1}')
        
        if [ "$CURRENT_RES" != "$LAST_RES" ] && [ -n "$CURRENT_RES" ]; then
          echo "Resolution changed from $LAST_RES to $CURRENT_RES"
          
          # Auto-adjust profile if enabled
          if [ -f /tmp/.parallels-dynamic-resolution ]; then
            $PROFILE_SWITCHER auto
          fi
          
          LAST_RES="$CURRENT_RES"
        fi
      fi
      
      sleep 2
    done
  '';

in {
  options.hardware.parallels.display = {
    optimizations = mkOption {
      type = types.bool;
      default = config.hardware.parallels.enable or false;
      description = "Enable Parallels-specific display optimizations";
    };
    
    dynamicResolution = mkOption {
      type = types.bool;
      default = true;
      description = "Enable dynamic resolution support (auto-adjust when VM window resizes)";
    };
    
    supportedResolutions = mkOption {
      type = types.listOf types.str;
      default = [
        "640x480"
        "800x600"
        "1024x768"
        "1280x720"
        "1280x768"
        "1280x800"
        "1280x960"
        "1280x1024"
        "1360x768"
        "1366x768"
        "1400x1050"
        "1440x900"
        "1600x900"
        "1600x1200"
        "1680x1050"
        "1792x1344"
        "1856x1392"
        "1920x1080"
        "1920x1200"
        "1920x1440"
        "2048x1152"
        "2560x1080"
        "2560x1440"
        "2560x1600"
        "2880x1800"
        "3440x1440"
        "3816x2049"
        "3840x2160"
        "4096x2160"
        "5120x2880"
      ];
      description = "List of resolutions to ensure are available";
    };
    
    defaultResolution = mkOption {
      type = types.nullOr types.str;
      default = null;
      example = "1920x1080";
      description = "Default resolution to set on startup (null for auto)";
    };
    
    retinaModeHandling = mkOption {
      type = types.bool;
      default = true;
      description = "Enable special handling for Retina displays";
    };
  };
  
  config = mkIf cfg.optimizations {
    # Ensure Parallels Tools are enabled
    hardware.parallels.enable = mkDefault true;
    
    # Add custom modelines for non-standard Parallels resolutions
    services.xserver.monitorSection = mkIf (cfg.supportedResolutions != []) ''
      # Parallels custom resolutions
      ${concatMapStrings (res: 
        let
          parts = splitString "x" res;
          width = elemAt parts 0;
          height = elemAt parts 1;
        in ''
          # ${res} resolution support
          Modeline "${res}_60.00" optimized
        ''
      ) cfg.supportedResolutions}
    '';
    
    # Device section for Parallels video
    services.xserver.deviceSection = mkAfter ''
      # Parallels-specific options
      Option "AccelMethod" "glamor"
      Option "DRI" "3"
      Option "TearFree" "true"
    '';
    
    # Add display optimization to session commands
    services.xserver.displayManager.sessionCommands = mkAfter ''
      # Run Parallels display optimizations
      ${parallelsDisplayScript}
      
      # Set default resolution if specified
      ${optionalString (cfg.defaultResolution != null) ''
        if command -v xrandr >/dev/null 2>&1; then
          xrandr --output Virtual-1 --mode ${cfg.defaultResolution} 2>/dev/null || true
        fi
      ''}
    '';
    
    # Create systemd service for resolution monitoring
    systemd.user.services.parallels-resolution-monitor = mkIf cfg.dynamicResolution {
      description = "Monitor Parallels VM resolution changes";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "simple";
        ExecStart = "${resolutionMonitor}";
        Restart = "always";
        RestartSec = "5";
      };
    };
    
    # Environment variables for Parallels optimization
    environment.sessionVariables = mkIf cfg.optimizations {
      # Disable compositing for better performance in VM
      XCOMPOSITE_FORCE_MANUAL = "1";
      
      # Use software rendering if hardware acceleration issues occur
      LIBGL_ALWAYS_SOFTWARE = mkDefault "0";
      
      # Parallels-specific rendering hints
      __GL_SYNC_TO_VBLANK = "0";
    };
    
    # Kernel parameters for better VM performance
    boot.kernelParams = mkIf cfg.optimizations [
      "video=Virtual-1:${if cfg.defaultResolution != null then cfg.defaultResolution else "auto"}"
      "drm.debug=0"
      "drm.vblankoffdelay=1"
    ];
    
    # Add helper script for checking Parallels display status
    environment.systemPackages = [
      (pkgs.writeScriptBin "parallels-display-info" ''
        #!${pkgs.bash}/bin/bash
        echo "Parallels Display Information:"
        echo "=============================="
        
        if [ -x /usr/bin/prlcc ]; then
          echo "Parallels Tools Status:"
          prlcc status 2>/dev/null || echo "  Unable to query status"
          echo ""
        fi
        
        if command -v xrandr >/dev/null 2>&1 && [ -n "$DISPLAY" ]; then
          echo "Current Display Configuration:"
          xrandr --current | grep -A 1 "connected"
          echo ""
          echo "Available Resolutions:"
          xrandr | grep -E "^\s+[0-9]+x[0-9]+" | awk '{print $1}' | sort -u
        else
          echo "X11 not available"
        fi
        
        if [ -f /tmp/.current-display-profile ]; then
          echo ""
          echo "Current Display Profile: $(cat /tmp/.current-display-profile)"
        fi
        
        if [ -f /tmp/.parallels-dynamic-resolution ]; then
          echo "Dynamic Resolution: Enabled"
        else
          echo "Dynamic Resolution: Disabled"
        fi
      '')
    ];
  };
}