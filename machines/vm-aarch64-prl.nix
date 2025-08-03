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
    ../modules/parallels-periodic-restart.nix
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
      maxSize = 262144; # 256KB - reduced to prevent beach balls
      monitor = true; # Enable monitoring to clear large clipboard content
      plainTextOnly = true; # Force plain text to avoid format conversion issues
    };
    
    # Periodic restart to prevent service degradation
    periodicRestart = {
      enable = true;
      healthCheckInterval = "1h"; # Check every hour
      preventiveRestartTime = "03:00"; # Daily restart at 3 AM
      restartOnMemoryThreshold = 512; # Restart if memory > 512MB
      restartOnUptimeHours = 12; # Restart if running > 12 hours
      enablePreventiveRestart = true; # Enable daily restart
      enableNotifications = true; # Show desktop notifications
    };
  };

  # Interface is this on my M1
  networking.interfaces.enp0s5.useDHCP = true;

  # Lots of stuff that uses aarch64 that claims doesn't work, but actually works.
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
}
