{ config, lib, pkgs, ... }:

with lib;

{
  config = mkIf config.hardware.parallels.enable {
    # Override the default prlcp service with working configuration
    systemd.user.services.prlcp = mkForce {
      description = "Parallels CopyPaste Tool";
      wantedBy = [ "graphical-session.target" ];
      after = [ "graphical-session.target" ];
      
      environment = {
        DISPLAY = ":0";
        # Don't force X11 backend - let it auto-detect
      };
      
      serviceConfig = {
        ExecStart = "${config.hardware.parallels.package}/bin/prlcp";
        Restart = "on-failure";
        RestartSec = 5;
        # Remove aggressive limits that might cause issues
        Nice = 5;
      };
    };
  };
}