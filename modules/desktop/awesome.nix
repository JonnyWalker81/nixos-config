{ config, lib, pkgs, ... }:

{
  options.desktop.awesome.enable =
    lib.mkEnableOption "AwesomeWM window manager";

  config = lib.mkIf config.desktop.awesome.enable {
    services.xserver.windowManager.awesome = {
      enable = true;

      luaModules = with pkgs.luaPackages; [ luarocks luadbi-mysql ];
    };
  };
}
