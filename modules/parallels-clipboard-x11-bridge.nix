{ config, lib, pkgs, ... }:

with lib;

{
  config = mkIf config.hardware.parallels.enable {

    # Enable prlcp service with X11 clipboard bridge
    systemd.user.services.prlcp = mkForce {
      description = "Parallels CopyPaste Tool (with X11 bridge)";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];

      environment = {
        # Set X11 display for prlcp (it only supports X11 API)
        DISPLAY = ":0";

        # wl-clipboard-x11 overrides: Make xclip/xsel point to wl-clipboard wrappers
        # This allows prlcp's X11 clipboard calls to work with Wayland clipboard
        PATH = lib.mkForce "${pkgs.wl-clipboard-x11}/bin:${config.hardware.parallels.package}/bin:/run/current-system/sw/bin";

        # Force software rendering to avoid GPU issues
        LIBGL_ALWAYS_SOFTWARE = "1";

        # Keep Wayland display available
        WAYLAND_DISPLAY = "wayland-1";
      };

      serviceConfig = {
        # Run prlcp - it will use X11 API but wl-clipboard-x11 bridges to Wayland
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

    # Keep clipboard-persistence for clipboard longevity
    systemd.user.services.clipboard-persistence = {
      description = "Wayland Clipboard Persistence";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];

      serviceConfig = {
        Type = "simple";
        ExecStart = "${pkgs.writeShellScript "clipboard-persistence" ''
          #!/usr/bin/env bash

          # Simple clipboard persistence - keeps clipboard content alive
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

    # Install wl-clipboard-x11 and utility scripts
    environment.systemPackages = with pkgs; [
      wl-clipboard-x11  # Provides xclip/xsel wrappers for Wayland
      xorg.xhost        # For X11 access control (if needed)
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

      (pkgs.writeShellScriptBin "test-clipboard-sync" ''
        #!/usr/bin/env bash
        echo "=== Parallels Clipboard Sync Test ==="
        echo ""

        # Check service status
        echo "Service status:"
        systemctl --user is-active prlcp && echo "✓ prlcp: active" || echo "✗ prlcp: inactive"
        systemctl --user is-active clipboard-persistence && echo "✓ clipboard-persistence: active" || echo "✗ clipboard-persistence: inactive"

        # Check processes
        echo ""
        echo "Running processes:"
        pgrep -a prlcp || echo "No prlcp process found"

        # Test local clipboard
        echo ""
        echo "Testing local Wayland clipboard:"
        test_string="Test-$(date +%s)"
        echo "$test_string" | wl-copy
        sleep 0.5
        result=$(wl-paste)
        if [ "$result" = "$test_string" ]; then
          echo "✓ Wayland clipboard works"
        else
          echo "✗ Wayland clipboard failed"
        fi

        # Test X11 bridge
        echo ""
        echo "Testing X11 clipboard bridge (via wl-clipboard-x11):"
        test_x11="X11-test-$(date +%s)"

        # Check if wl-clipboard-x11's xclip wrapper is available
        if command -v xclip &> /dev/null; then
          # Copy via X11 API (which wl-clipboard-x11 bridges to Wayland)
          echo "$test_x11" | xclip -selection clipboard 2>/dev/null || echo "xclip copy failed"
          sleep 0.5
          # Paste via Wayland API
          result_x11=$(wl-paste 2>/dev/null)
          if [ "$result_x11" = "$test_x11" ]; then
            echo "✓ X11 → Wayland bridge works"
          else
            echo "✗ X11 → Wayland bridge failed (got: '$result_x11')"
          fi
        else
          echo "⚠ xclip not found in PATH"
        fi

        echo ""
        echo "=== VM-to-host clipboard sync test ==="
        echo "1. Copy this text: 'Hello from VM'"
        echo "Hello from VM" | wl-copy
        echo "2. Try pasting in macOS (Cmd+V)"
        echo ""
        echo "=== Host-to-VM clipboard sync test ==="
        echo "1. Copy some text in macOS (Cmd+C)"
        echo "2. Run: wl-paste"
        echo ""
        echo "If sync doesn't work, check: journalctl --user -u prlcp -n 50"
      '')
    ];

    # Add shell aliases
    programs.bash.shellAliases = {
      fix-clipboard = "fix-parallels-clipboard";
      test-clipboard = "test-clipboard-sync";
    };
    programs.zsh.shellAliases = {
      fix-clipboard = "fix-parallels-clipboard";
      test-clipboard = "test-clipboard-sync";
    };
  };
}
