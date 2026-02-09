{ config, lib, pkgs, ... }:

{
  options.desktop.xmonad.enable = lib.mkEnableOption "XMonad window manager";

  config = lib.mkIf config.desktop.xmonad.enable {
    services.xserver.windowManager.xmonad = {
      enable = true;
      enableContribAndExtras = true;

      extraPackages = hpkgs: [
        hpkgs.xmonad-contrib
        hpkgs.xmonad-extras
        hpkgs.xmonad
      ];
    };
  };
}
