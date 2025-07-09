{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.xserver.displayProfiles;
  
  # Display profile types
  profileType = types.enum [ "hidpi" "retina" "standard" "present" "ultrawide" "auto" ];
  
  # DPI settings for each profile
  dpiSettings = {
    hidpi = 220;
    retina = 180;
    standard = 96;
    present = 110;
    ultrawide = 110;
  };
  
  # Cursor sizes for each profile
  cursorSizes = {
    hidpi = 48;
    retina = 36;
    standard = 24;
    present = 32;
    ultrawide = 28;
  };
  
  # Environment variables for each profile
  envVars = profile: {
    hidpi = {
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.5";
      QT_SCALE_FACTOR = "2";
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=2";
    };
    retina = {
      GDK_SCALE = "2";
      GDK_DPI_SCALE = "0.75";
      QT_SCALE_FACTOR = "1.5";
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1.5";
    };
    standard = {
      GDK_SCALE = "1";
      GDK_DPI_SCALE = "1";
      QT_SCALE_FACTOR = "1";
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1";
    };
    present = {
      GDK_SCALE = "1";
      GDK_DPI_SCALE = "1.15";
      QT_SCALE_FACTOR = "1.15";
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1.15";
    };
    ultrawide = {
      GDK_SCALE = "1";
      GDK_DPI_SCALE = "1.1";
      QT_SCALE_FACTOR = "1.1";
      _JAVA_OPTIONS = "-Dsun.java2d.uiScale=1.1";
    };
  }.${profile} or { };

in {
  options.services.xserver.displayProfiles = {
    enable = mkEnableOption "display profile management";
    
    defaultProfile = mkOption {
      type = profileType;
      default = "auto";
      description = "Default display profile to use";
    };
    
    autoDetect = mkOption {
      type = types.bool;
      default = true;
      description = "Automatically detect and apply best display profile on login";
    };
    
    profiles = mkOption {
      type = types.attrsOf (types.submodule {
        options = {
          dpi = mkOption {
            type = types.int;
            description = "DPI setting for this profile";
          };
          cursorSize = mkOption {
            type = types.int;
            description = "Cursor size for this profile";
          };
          environmentVariables = mkOption {
            type = types.attrsOf types.str;
            default = {};
            description = "Environment variables for this profile";
          };
        };
      });
      default = {};
      description = "Custom display profiles";
    };
  };
  
  config = mkIf (cfg.enable && config.services.xserver.enable) {
    # Set DPI based on profile if not auto
    services.xserver.dpi = mkIf (cfg.defaultProfile != "auto") 
      (dpiSettings.${cfg.defaultProfile} or 96);
    
    # Add display profile scripts to system packages
    environment.systemPackages = [
      (pkgs.writeScriptBin "display-profile" ''
        #!${pkgs.bash}/bin/bash
        exec ${../scripts/display-profiles/display-switcher.sh} "$@"
      '')
    ];
    
    # Session commands to set up display profile
    services.xserver.displayManager.sessionCommands = mkAfter ''
      # Display profile initialization
      if [ "${toString cfg.autoDetect}" = "1" ]; then
        ${../scripts/display-profiles/display-switcher.sh} auto
      elif [ "${cfg.defaultProfile}" != "auto" ]; then
        ${../scripts/display-profiles/display-switcher.sh} ${cfg.defaultProfile}
      fi
      
      # Set environment variables based on profile
      if [ -f /tmp/.current-display-profile ]; then
        PROFILE=$(cat /tmp/.current-display-profile | tr '[:upper:]' '[:lower:]')
        case "$PROFILE" in
          ${concatStringsSep "\n" (mapAttrsToList (name: vars: ''
            "${name}")
              ${concatStringsSep "\n" (mapAttrsToList (var: val: ''
                export ${var}="${val}"
              '') vars)}
              ;;
          '') (mapAttrs (n: v: envVars n) dpiSettings))}
        esac
      fi
    '';
    
    # X resources configuration
    services.xserver.displayManager.sessionCommands = mkAfter ''
      # Ensure Xft settings are applied
      ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
      Xft.antialias: true
      Xft.hinting: true
      Xft.rgba: rgb
      Xft.hintstyle: hintslight
      Xft.lcdfilter: lcddefault
      EOF
    '';
    
    # Create systemd service for display profile management
    systemd.user.services.display-profile-monitor = {
      description = "Monitor display changes and adjust profile";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      serviceConfig = {
        Type = "oneshot";
        RemainAfterExit = true;
        ExecStart = "${pkgs.bash}/bin/bash -c '${../scripts/display-profiles/display-switcher.sh} auto'";
      };
    };
  };
}