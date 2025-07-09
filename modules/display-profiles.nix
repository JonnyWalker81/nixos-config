{ config, lib, pkgs, ... }:

{
  imports = [
    ./display-x11.nix
    ./parallels-display.nix
  ];

  # Enable display profile management
  services.xserver.displayProfiles = {
    enable = true;
    defaultProfile = "auto";
    autoDetect = true;
  };

  # Enable Parallels-specific display optimizations if running in Parallels
  hardware.parallels.display = {
    optimizations = config.hardware.parallels.enable or false;
    dynamicResolution = true;
    retinaModeHandling = true;
  };
}