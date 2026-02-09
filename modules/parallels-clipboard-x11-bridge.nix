{
  config,
  lib,
  pkgs,
  ...
}:

with lib;

let
  prl-tools = config.hardware.parallels.package;
  xclipBin = "${pkgs.xclip}/bin/xclip";
in
{
  config = mkIf config.hardware.parallels.enable {

    # Override prlcp to run correctly in X11 sessions (DWM, XMonad, etc.)
    # prlcp is an X11 application that uses the X11 clipboard protocol natively.
    # It communicates with the Parallels host clipboard via the prltoolsd daemon
    # over virtio-vsock, and syncs that with the X11 CLIPBOARD selection.
    # No Wayland bridging is needed when running an X11 window manager.
    systemd.user.services.prlcp = mkForce {
      description = "Parallels CopyPaste Tool";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];

      environment = {
        DISPLAY = ":0";
        LIBGL_ALWAYS_SOFTWARE = "1";
      };

      unitConfig = {
        StartLimitIntervalSec = 60;
        StartLimitBurst = 5;
      };

      serviceConfig = {
        ExecStart = "${prl-tools}/bin/prlcp";
        Restart = "on-failure";
        RestartSec = 3;
        TimeoutStopSec = 10;
      };
    };

    # Disable the problematic focus guard
    systemd.user.services.parallels-focus-guard = mkForce {
      enable = false;
      wantedBy = [ ];
    };

    # Install utility scripts for clipboard diagnostics
    environment.systemPackages = with pkgs; [
      xclip
      xorg.xhost
      (pkgs.writeShellScriptBin "fix-parallels-clipboard" ''
        #!/usr/bin/env bash
        echo "Restarting Parallels clipboard service..."

        # Stop service and kill any hanging processes
        systemctl --user stop prlcp 2>/dev/null || true
        ${pkgs.procps}/bin/pkill -9 prlcp 2>/dev/null || true
        sleep 1

        # Clear X11 clipboard
        echo -n "" | ${xclipBin} -selection clipboard 2>/dev/null || true

        # Restart service
        systemctl --user start prlcp

        sleep 1
        echo "prlcp status: $(systemctl --user is-active prlcp)"
        echo ""

        # Test clipboard
        local_test="Test from VM $(${pkgs.coreutils}/bin/date +%s)"
        echo "$local_test" | ${xclipBin} -selection clipboard
        sleep 0.5
        result=$(${xclipBin} -selection clipboard -o 2>/dev/null)
        if [ "$result" = "$local_test" ]; then
          echo "Local X11 clipboard: OK"
        else
          echo "Local X11 clipboard: FAILED"
        fi
        echo ""
        echo "Now try copying from macOS and pasting in the VM, and vice versa."
      '')

      (pkgs.writeShellScriptBin "test-clipboard-sync" ''
        #!/usr/bin/env bash
        echo "=== Parallels Clipboard Sync Test ==="
        echo ""

        # Check service status
        echo "Service status:"
        systemctl --user is-active prlcp && echo "  prlcp: active" || echo "  prlcp: INACTIVE"
        echo ""

        # Check processes
        echo "Running processes:"
        ${pkgs.procps}/bin/pgrep -a prlcp || echo "  No prlcp process found"
        echo ""

        # Test local X11 clipboard
        echo "Testing X11 clipboard (CLIPBOARD selection):"
        test_string="clipboard-test-$(${pkgs.coreutils}/bin/date +%s)"
        echo "$test_string" | ${xclipBin} -selection clipboard
        sleep 0.5
        result=$(${xclipBin} -selection clipboard -o 2>/dev/null)
        if [ "$result" = "$test_string" ]; then
          echo "  X11 clipboard: OK"
        else
          echo "  X11 clipboard: FAILED (got: '$result')"
        fi

        echo ""
        echo "=== VM-to-host test ==="
        echo "  1. Text 'Hello from VM' has been placed on clipboard"
        echo "Hello from VM" | ${xclipBin} -selection clipboard
        echo "  2. Switch to macOS and press Cmd+V"
        echo ""
        echo "=== Host-to-VM test ==="
        echo "  1. Copy some text in macOS (Cmd+C)"
        echo "  2. Switch to VM and run: xclip -selection clipboard -o"
        echo ""
        echo "If sync fails, check: journalctl --user -u prlcp -n 50"
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
