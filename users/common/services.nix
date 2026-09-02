{ config, lib, pkgs, ... }:

{
  programs.home-manager.enable = true;

  # Root's nix-gc.service can never reach ~/.local/state/nix/profiles:
  # nix-collect-garbage's profilesDir() branches on isRootUser(), so a root
  # invocation only ever cleans /nix/var/nix/profiles (+ per-user/root). This
  # user-level timer covers the home-manager generations that root structurally
  # cannot see. Requires users.users.<user>.linger (set in machines/vm-shared.nix).
  #
  # Linux only: home-manager's Darwin branch of this module builds
  # ProgramArguments with lib.optional, collapsing the whole options string into
  # one argv element -- `nix-collect-garbage "--delete-older-than 14d"` fails
  # with "unrecognised flag" on every run. See machines/darwin-shared.nix.
  nix.gc = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
    automatic = true;
    # `frequency`, NOT `dates`: this repo pins home-manager release-25.05, where
    # the `dates` rename has not landed yet and would be an eval error.
    frequency = "daily";
    randomizedDelaySec = "45min";
    # Load-bearing: `options` defaults to null, which runs a bare
    # nix-collect-garbage that deletes zero generations.
    options = "--delete-older-than 14d";
  };

  # SSH Agent Service (Linux only - macOS uses native SSH agent)
  services.ssh-agent = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) { enable = true; };

  # Systemd service to set SSH_AUTH_SOCK for all user services (Linux only)
  systemd.user.services.ssh-agent-env = lib.mkIf (!pkgs.stdenv.hostPlatform.isDarwin) {
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
