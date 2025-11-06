{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.parallels.clipboard;
  
  # Script to monitor and fix clipboard issues
  clipboardMonitor = pkgs.writeShellScript "parallels-clipboard-monitor" ''
    #!/usr/bin/env bash
    
    # Maximum clipboard size in bytes (256KB)
    MAX_SIZE=''${PARALLELS_CLIPBOARD_MAX_SIZE:-262144}
    
    # Log file
    LOG_FILE="/tmp/parallels-clipboard-monitor.log"
    
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
    
    # Function to clear clipboard
    clear_clipboard() {
      echo "[$(date)] Clearing clipboard" >> "$LOG_FILE"
      if command -v wl-copy &> /dev/null; then
        echo -n "" | wl-copy 2>/dev/null || true
        echo -n "" | wl-copy --primary 2>/dev/null || true
      fi
      if command -v xclip &> /dev/null; then
        echo -n "" | xclip -selection clipboard 2>/dev/null || true
        echo -n "" | xclip -selection primary 2>/dev/null || true
      fi
    }
    
    # Function to check and clear clipboard if too large
    check_and_clear() {
      local size=$(get_clipboard_size)
      if [ "$size" -gt "$MAX_SIZE" ]; then
        echo "[$(date)] Clipboard too large ($size bytes), clearing..." >> "$LOG_FILE"
        clear_clipboard
      fi
    }
    
    # Monitor for focus events if running under Hyprland
    if [ -n "$HYPRLAND_INSTANCE_SIGNATURE" ] && command -v socat &> /dev/null; then
      # Monitor clipboard and focus events in parallel
      (
        # Clipboard size monitor
        while true; do
          check_and_clear
          sleep 1
        done
      ) &
      
      # Focus event monitor - clear clipboard on window focus changes
      socat -U - UNIX-CONNECT:/tmp/hypr/$HYPRLAND_INSTANCE_SIGNATURE/.socket2.sock 2>/dev/null | while read -r line; do
        case "$line" in
          *"activewindow>>"*)
            # Window focus changed - could be switching from macOS
            echo "[$(date)] Focus change detected: $line" >> "$LOG_FILE"
            # Small delay to let clipboard sync complete
            sleep 0.2
            check_and_clear
            ;;
        esac
      done
    else
      # Fallback: just monitor clipboard size
      while true; do
        check_and_clear
        sleep 1
      done
    fi
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
    
    # Restart the service
    systemctl --user start prlcp
    
    echo "Clipboard service restarted!"
  '';
in
{
  options.hardware.parallels.clipboard = {
    optimization = mkOption {
      type = types.bool;
      default = true;
      description = "Enable clipboard optimization for Parallels Tools";
    };
    
    maxSize = mkOption {
      type = types.int;
      default = 262144; # 256KB
      description = "Maximum clipboard size in bytes before automatic clearing";
    };
    
    monitor = mkOption {
      type = types.bool;
      default = true;
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
      after = [ "graphical-session.target" ];
      
      # Add environment variables for optimization
      environment = {
        PARALLELS_CLIPBOARD_MAX_SIZE = toString cfg.maxSize;
        # Disable rich text if configured
        PARALLELS_CLIPBOARD_PLAIN_TEXT = if cfg.plainTextOnly then "1" else "0";
        # Use X11 display
        DISPLAY = ":0";
        # Don't override Wayland, let it coexist
        # This allows prlcp to work with both X11 and Wayland
      };
      
      serviceConfig = {
        # Use prlcp directly with proper environment
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
        MemoryMax = "256M";
        # CPU quota to prevent hanging from using too much CPU
        CPUQuota = "50%";
        # Prevent rapid restarts
        StartLimitBurst = 3;
        StartLimitInterval = 60;
      };
      
      # Restart if it uses too much memory
      unitConfig = {
        ConditionMemory = ">128M";
      };
    };
    
    # Optional clipboard monitor service
    systemd.user.services.parallels-clipboard-monitor = mkIf cfg.monitor {
      description = "Parallels Clipboard Monitor";
      wantedBy = [ "graphical-session.target" ];
      after = [ "prlcp.service" ];
      
      environment = {
        PARALLELS_CLIPBOARD_MAX_SIZE = toString cfg.maxSize;
        HYPRLAND_INSTANCE_SIGNATURE = "\${HYPRLAND_INSTANCE_SIGNATURE}";
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
    
    # Focus guard service to prevent beach balls
    systemd.user.services.parallels-focus-guard = mkIf (config.programs.hyprland.enable or false) {
      description = "Parallels Focus Guard - Prevents beach balls when switching";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      environment = {
        HYPRLAND_INSTANCE_SIGNATURE = "\${HYPRLAND_INSTANCE_SIGNATURE}";
      };
      
      serviceConfig = let
        focusGuardScript = "/home/cipher/nixos-config/scripts/parallels-focus-guard.sh";
      in {
        ExecStart = focusGuardScript;
        Restart = "always";
        RestartSec = 5;
        Nice = 10;
      };
      
      # Only enable if the script exists
      unitConfig = {
        ConditionPathExists = "/home/cipher/nixos-config/scripts/parallels-focus-guard.sh";
      };
    };
  };
}