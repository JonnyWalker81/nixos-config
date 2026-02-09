{ config, lib, pkgs, ... }:

{
  programs.home-manager.enable = true;

  # SSH Agent Service (Linux only - macOS uses native SSH agent)
  services.ssh-agent = lib.mkIf (!pkgs.stdenv.isDarwin) { enable = true; };

  # Systemd service to set SSH_AUTH_SOCK for all user services (Linux only)
  systemd.user.services.ssh-agent-env = lib.mkIf (!pkgs.stdenv.isDarwin) {
    Unit = {
      Description = "Set SSH_AUTH_SOCK environment variable for user services";
      After = [ "ssh-agent.service" ];
      Wants = [ "ssh-agent.service" ];
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart =
        "${pkgs.systemd}/bin/systemctl --user set-environment SSH_AUTH_SOCK=/run/user/1000/ssh-agent";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };
}
