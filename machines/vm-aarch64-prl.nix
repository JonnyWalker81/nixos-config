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
    ../modules/parallels-clipboard-x11-bridge.nix  # New: wl-clipboard-x11 bridge for prlcp
    ./vm-shared.nix
  ];

  services.ntp.enable = true;
  # The official parallels guest support does not work currently.
  # https://github.com/NixOS/nixpkgs/pull/153665
  disabledModules = [ "virtualisation/parallels-guest.nix" ];
  hardware.parallels = {
    enable = true;
    autoMountShares = true;  # Re-enabled with patched prlfsmountd (fixed /etc/fstab read-only issue)
    # package = (config.boot.kernelPackages.callPackage ../pkgs/parallels-tools/default.nix { });
  };

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Lots of stuff that uses aarch64 that claims doesn't work, but actually works.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
