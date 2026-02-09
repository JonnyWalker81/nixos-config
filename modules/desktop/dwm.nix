{ config, lib, pkgs, ... }:

{
  options.desktop.dwm.enable = lib.mkEnableOption "DWM window manager";

  config = lib.mkIf config.desktop.dwm.enable {
    services.xserver.windowManager.dwm.enable = true;
  };
}
