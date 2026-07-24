{
  config,
  pkgs,
  lib,
  modulesPath,
  ...
}:
{
  imports = [
    # Parallels is qemu under the covers. This brings in important kernel
    # modules to get a lot of the stuff working.
    (modulesPath + "/profiles/qemu-guest.nix")

    ../hardware/vm-aarch64-prl.nix
    ../modules/parallels-guest.nix
    # ../modules/parallels-clipboard-working.nix  # Old: Wayland-only, no VM-host sync
    # ../modules/parallels-clipboard-hybrid.nix   # Old: Attempted Wayland with prlcp
    ../modules/parallels-clipboard-x11-bridge.nix # New: wl-clipboard-x11 bridge for prlcp
    ./vm-shared.nix
  ];

  # Time sync via chrony. Unlike ntpd (which panics and exits when the offset
  # exceeds its threshold — e.g. after a Parallels host suspend freezes the VM),
  # chrony step-corrects arbitrarily large offsets and keeps running. This
  # replaces the previous ntpd + timeapi.io web-fallback setup, whose data
  # source drifted ~5 min behind and pinned the clock to the wrong time.
  # Servers are inherited from networking.timeServers (pool.ntp.org, time.nist.gov).
  services.ntp.enable = false;
  services.chrony = {
    enable = true;
    # makestep <threshold> <limit>: step (not slew) the clock whenever the
    # offset exceeds 1s, on every update (-1 = no limit). Corrects the large
    # jumps after a VM freeze/unfreeze without the daemon dying.
    extraConfig = ''
      makestep 1.0 -1
    '';
    # Do NOT add `rtcsync`: enableRTCTrimming (default true) manages the RTC via
    # rtcfile/rtcautotrim, and the chrony module asserts against rtcsync.
  };
  # The official parallels guest support does not work currently.
  # https://github.com/NixOS/nixpkgs/pull/153665
  disabledModules = [ "virtualisation/parallels-guest.nix" ];
  hardware.parallels = {
    enable = true;
    autoMountShares = true; # Re-enabled with patched prlfsmountd (fixed /etc/fstab read-only issue)
    package = (
      pkgs.callPackage ../pkgs/parallels-tools/default.nix {
        kernel = config.boot.kernelPackages.kernel;
      }
    );
  };

  # Force the VM display to the highest available mode on X11 session start.
  # Parallels can occasionally come up in 1024x768 even when higher modes are exposed.
  services.xserver.displayManager.sessionCommands = lib.mkAfter ''
    if command -v xrandr >/dev/null 2>&1; then
      output=$(xrandr --current | awk '
        / connected primary/ { print $1; exit }
        / connected/ { print $1; exit }
      ')

      if [ -n "$output" ]; then
        best_mode=$(xrandr --current | awk -v out="$output" '
          $1 == out && / connected/ { in_output = 1; next }
          in_output && $0 !~ /^ / { in_output = 0 }
          in_output && $1 ~ /^[0-9]+x[0-9]+$/ {
            split($1, dim, "x")
            pixels = dim[1] * dim[2]
            if (pixels > max_pixels) {
              max_pixels = pixels
              best = $1
            }
          }
          END { if (best != "") print best }
        ')

        if [ -n "$best_mode" ]; then
          xrandr --output "$output" --mode "$best_mode" || true
        fi
      fi
    fi
  '';

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Lots of stuff that uses aarch64 that claims doesn't work, but actually works.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
