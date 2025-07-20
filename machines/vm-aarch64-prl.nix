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
    # ../modules/parallels-guest.nix
    ../modules/parallels-clipboard-fix.nix
    ./vm-shared.nix
  ];

  services.ntp.enable = true;
  # The official parallels guest support does not work currently.
  # https://github.com/NixOS/nixpkgs/pull/153665
  # disabledModules = [ "virtualisation/parallels-guest.nix" ];
  hardware.parallels = {
    enable = true;
    # package = (config.boot.kernelPackages.callPackage ../pkgs/parallels-tools/default.nix { });
    
    # Clipboard optimization to prevent hangs
    clipboard = {
      optimization = true;
      maxSize = 524288; # 512KB - smaller size to prevent hangs
      monitor = true; # Enable monitoring to clear large clipboard content
      plainTextOnly = false; # Set to true if you continue to have issues
    };
  };

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Lots of stuff that uses aarch64 that claims doesn't work, but actually works.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
