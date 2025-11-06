{ config, lib, pkgs, ... }:

with lib;

{
  config = mkIf config.hardware.parallels.enable {
    # Override the default prlcp service to work properly with Wayland
    systemd.user.services.prlcp = mkForce {
      description = "Parallels Copy Paste Tool";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      environment = {
        DISPLAY = ":0";
        WAYLAND_DISPLAY = "wayland-1";
        XDG_SESSION_TYPE = "wayland";
      };
      
      serviceConfig = {
        ExecStart = "${config.hardware.parallels.package}/bin/prlcp";
        Restart = "always";
        RestartSec = 5;
      };
    };
  };
}