{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.parallels.clipboard;
  
  # Script to monitor and fix clipboard issues
  clipboardMonitor = pkgs.writeShellScript "parallels-clipboard-monitor" ''
    #!/usr/bin/env bash
    
    # Maximum clipboard size in bytes (1MB default)
    MAX_SIZE=''${PARALLELS_CLIPBOARD_MAX_SIZE:-1048576}
    
    # Function to get clipboard size
    get_clipboard_size() {
      if command -v wl-paste &> /dev/null; then
        wl-paste 2>/dev/null | wc -c || echo 0
      elif command -v xclip &> /dev/null; then
        xclip -selection clipboard -o 2>/dev/null | wc -c || echo 0
      else
        echo 0
      fi
    }
    
    # Function to clear clipboard if too large
    check_and_clear() {
      local size=$(get_clipboard_size)
      if [ "$size" -gt "$MAX_SIZE" ]; then
        echo "Clipboard content too large ($size bytes), clearing..."
        if command -v wl-copy &> /dev/null; then
          echo -n "" | wl-copy
          echo -n "" | wl-copy --primary
        elif command -v xclip &> /dev/null; then
          echo -n "" | xclip -selection clipboard
          echo -n "" | xclip -selection primary
        fi
      fi
    }
    
    # Monitor clipboard changes
    while true; do
      check_and_clear
      sleep 2
    done
  '';
  
  # Quick fix script
  quickFix = pkgs.writeShellScriptBin "fix-parallels-clipboard" ''
    #!/usr/bin/env bash
    echo "Restarting Parallels clipboard service..."
    
    # Stop the service
    systemctl --user stop prlcp 2>/dev/null || true
    
    # Kill any hanging processes
    pkill -9 prlcp 2>/dev/null || true
    
    # Clear clipboard
    if command -v wl-copy &> /dev/null; then
      echo -n "" | wl-copy
      echo -n "" | wl-copy --primary
    elif command -v xclip &> /dev/null; then
      echo -n "" | xclip -selection clipboard
      echo -n "" | xclip -selection primary
    fi
    
    # Wait and restart
    sleep 1
    systemctl --user start prlcp
    
    echo "Clipboard service restarted."
  '';

in {
  options.hardware.parallels.clipboard = {
    optimization = mkOption {
      type = types.bool;
      default = config.hardware.parallels.enable;
      description = "Enable Parallels clipboard optimization to prevent hangs";
    };
    
    maxSize = mkOption {
      type = types.int;
      default = 1048576; # 1MB
      description = "Maximum clipboard size in bytes before automatic clearing";
    };
    
    monitor = mkOption {
      type = types.bool;
      default = false;
      description = "Enable clipboard monitoring service to prevent large content issues";
    };
    
    plainTextOnly = mkOption {
      type = types.bool;
      default = false;
      description = "Force plain text only clipboard (recommended for stability)";
    };
  };
  
  config = mkIf (config.hardware.parallels.enable && cfg.optimization) {
    # Add the fix script to system packages
    environment.systemPackages = [ quickFix ];
    
    # Override prlcp service with optimization
    systemd.user.services.prlcp = mkForce {
      description = "Parallels CopyPaste Tool (Optimized)";
      wantedBy = [ "graphical-session.target" ];
      
      # Add environment variables for optimization
      environment = {
        PARALLELS_CLIPBOARD_MAX_SIZE = toString cfg.maxSize;
        # Disable rich text if configured
        PARALLELS_CLIPBOARD_PLAIN_TEXT = if cfg.plainTextOnly then "1" else "0";
      };
      
      serviceConfig = {
        ExecStart = "${config.hardware.parallels.package}/bin/prlcp";
        Restart = "always";
        RestartSec = 5;
        # Kill hanging processes after 10 seconds
        TimeoutStopSec = 10;
        KillMode = "mixed";
        KillSignal = "SIGTERM";
        # Nice level to prevent CPU hogging
        Nice = 10;
        # Memory limit to prevent runaway memory usage
        MemoryLimit = "256M";
        # CPU quota to prevent hanging from using too much CPU
        CPUQuota = "50%";
      };
      
      # Restart if it uses too much memory
      unitConfig = {
        ConditionMemory = ">128M";
        RestartKillSignal = "SIGKILL";
      };
    };
    
    # Optional clipboard monitor service
    systemd.user.services.parallels-clipboard-monitor = mkIf cfg.monitor {
      description = "Parallels Clipboard Monitor";
      wantedBy = [ "graphical-session.target" ];
      after = [ "prlcp.service" ];
      
      environment = {
        PARALLELS_CLIPBOARD_MAX_SIZE = toString cfg.maxSize;
      };
      
      serviceConfig = {
        ExecStart = "${clipboardMonitor}";
        Restart = "always";
        RestartSec = 10;
        Nice = 15;
      };
    };
    
    # Add shell alias for quick fix
    programs.bash.shellAliases.fix-clipboard = "fix-parallels-clipboard";
    programs.zsh.shellAliases.fix-clipboard = "fix-parallels-clipboard";
  };
}