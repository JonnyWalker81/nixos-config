{ config, lib, pkgs, ... }:

with lib;

{
  config = mkIf config.hardware.parallels.enable {
    # Enable prlcp service with proper configuration for Wayland
    systemd.user.services.prlcp = mkForce {
      description = "Parallels CopyPaste Tool (Wayland Compatible)";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      environment = {
        # Set both X11 and Wayland displays
        DISPLAY = ":0";
        # Don't unset WAYLAND_DISPLAY - let prlcp detect and use it
        # This allows prlcp to work with both X11 and Wayland
        
        # Force software rendering to avoid GPU issues
        LIBGL_ALWAYS_SOFTWARE = "1";
        
        # Use native Wayland when available
        GDK_BACKEND = "wayland,x11";
        QT_QPA_PLATFORM = "wayland;xcb";
      };
      
      serviceConfig = {
        # Run prlcp directly
        ExecStart = "${config.hardware.parallels.package}/bin/prlcp";
        
        # Restart configuration
        Restart = "on-failure";
        RestartSec = 5;
        StartLimitBurst = 3;
        StartLimitIntervalSec = 60;
        
        # Timeout and resource limits
        TimeoutStopSec = 10;
        Nice = 10;
        MemoryMax = "256M";
        CPUQuota = "50%";
      };
    };
    
    # Disable the problematic focus guard
    systemd.user.services.parallels-focus-guard = mkForce {
      enable = false;
      wantedBy = [];
    };
    
    # Keep clipboard-persistence for basic functionality (essential for Wayland)
    systemd.user.services.clipboard-persistence = {
      description = "Wayland Clipboard Persistence";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "clipboard-persistence" ''
          #!/usr/bin/env bash
          
          # Simple clipboard persistence - essential for Wayland
          while true; do
            content=$(${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null || true)
            
            if [ -n "$content" ]; then
              printf "%s" "$content" | ${pkgs.wl-clipboard}/bin/wl-copy --foreground 2>/dev/null &
              COPY_PID=$!
              
              # Wait for clipboard to change
              while true; do
                sleep 1
                new_content=$(${pkgs.wl-clipboard}/bin/wl-paste 2>/dev/null || true)
                if [ "$new_content" != "$content" ]; then
                  kill $COPY_PID 2>/dev/null || true
                  break
                fi
              done
            else
              sleep 2
            fi
          done
        ''}";
        Restart = "always";
        RestartSec = 5;
      };
    };
    
    # Fix script for when things go wrong
    environment.systemPackages = [
      (pkgs.writeShellScriptBin "fix-parallels-clipboard" ''
        #!/usr/bin/env bash
        echo "Restarting Parallels clipboard services..."
        
        # Stop services
        systemctl --user stop prlcp 2>/dev/null || true
        systemctl --user stop clipboard-persistence 2>/dev/null || true
        
        # Kill any hanging processes
        pkill -9 prlcp 2>/dev/null || true
        pkill -f clipboard-persistence 2>/dev/null || true
        
        # Clear clipboard
        if command -v wl-copy &> /dev/null; then
          echo -n "" | wl-copy 2>/dev/null || true
        fi
        
        # Restart services
        systemctl --user start prlcp
        systemctl --user start clipboard-persistence
        
        echo "Clipboard services restarted!"
        echo "Testing clipboard..."
        
        # Test clipboard
        echo "Test from VM $(date +%s)" | wl-copy
        sleep 1
        result=$(wl-paste)
        echo "Local clipboard test: $result"
        echo ""
        echo "Now try copying from macOS and pasting in the VM, and vice versa."
      '')
      
      (pkgs.writeShellScriptBin "test-clipboard" ''
        #!/usr/bin/env bash
        echo "Testing clipboard functionality..."
        
        # Check service status
        echo "Service status:"
        systemctl --user is-active prlcp || echo "prlcp: inactive"
        systemctl --user is-active clipboard-persistence || echo "clipboard-persistence: inactive"
        
        # Check processes
        echo ""
        echo "Running processes:"
        pgrep -a prlcp || echo "No prlcp process found"
        
        # Test local clipboard
        echo ""
        echo "Testing local clipboard:"
        test_string="Test-$(date +%s)"
        echo "$test_string" | wl-copy
        sleep 0.5
        result=$(wl-paste)
        if [ "$result" = "$test_string" ]; then
          echo "✓ Local clipboard works"
        else
          echo "✗ Local clipboard failed"
        fi
        
        echo ""
        echo "To test VM-to-host sync:"
        echo "1. Copy this text: 'Hello from VM'"
        echo "Hello from VM" | wl-copy
        echo "2. Try pasting in macOS"
        echo ""
        echo "To test host-to-VM sync:"
        echo "1. Copy some text in macOS"
        echo "2. Run: wl-paste"
      '')
    ];
    
    # Add shell aliases
    programs.bash.shellAliases = {
      fix-clipboard = "fix-parallels-clipboard";
      test-clipboard = "test-clipboard";
    };
    programs.zsh.shellAliases = {
      fix-clipboard = "fix-parallels-clipboard";
      test-clipboard = "test-clipboard";
    };
  };
}