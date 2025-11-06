{ config, lib, pkgs, ... }:

with lib;

{
  config = mkIf config.hardware.parallels.enable {
    # Disable the problematic prlcp service
    systemd.user.services.prlcp = mkForce {
      enable = false;
      wantedBy = [];
    };
    
    # Disable the focus guard service that was destroying clipboard
    systemd.user.services.parallels-focus-guard = mkForce {
      enable = false;
      wantedBy = [];
    };
    
    # Add clipboard persistence service instead
    systemd.user.services.clipboard-persistence = {
      description = "Wayland Clipboard Persistence";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "clipboard-persistence" ''
          #!/usr/bin/env bash
          
          # Simple clipboard persistence for Wayland
          while true; do
            # Get current clipboard content
            content=$(${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null || true)
            
            if [ -n "$content" ]; then
              # Keep clipboard alive (use printf to avoid newline issues)
              printf "%s" "$content" | ${pkgs.wl-clipboard}/bin/wl-copy --foreground 2>/dev/null &
              COPY_PID=$!
              
              # Wait for clipboard to change
              while true; do
                sleep 1
                new_content=$(${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null || true)
                if [ "$new_content" != "$content" ]; then
                  # Kill old wl-copy and break to update
                  kill $COPY_PID 2>/dev/null || true
                  break
                fi
              done
            else
              sleep 1
            fi
          done
        ''}";
        Restart = "always";
        RestartSec = 5;
      };
    };
    
    # Add a manual clipboard sync command
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "sync-clipboard" ''
        #!/usr/bin/env bash
        echo "Manual clipboard sync (limited functionality without prlcp)"
        echo "Clipboard content will persist within the VM only"
        
        # Get current clipboard
        content=$(${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null || echo "")
        
        if [ -n "$content" ]; then
          echo "Current clipboard: $(echo "$content" | head -c 50)..."
          echo "Size: $(echo "$content" | wc -c) bytes"
        else
          echo "Clipboard is empty"
        fi
      '')
    ];
    
    # Add shell aliases
    programs.bash.shellAliases.fix-clipboard = "systemctl --user restart clipboard-persistence";
    programs.zsh.shellAliases.fix-clipboard = "systemctl --user restart clipboard-persistence";
  };
}