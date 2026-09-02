# Shared configuration for all nix-darwin (macOS) machines.
# Counterpart to machines/vm-shared.nix, which serves the NixOS hosts.
{ config, lib, ... }:

{
  # users/cipher/darwin.nix and users/phantom/darwin.nix both set
  # `nix.enable = false` for Determinate Systems compatibility. nix-darwin
  # asserts `nix.gc.automatic -> nix.enable`, and failed assertions THROW, so
  # this MUST be gated: with mkIf the definitions vanish instead of breaking the
  # rebuild. Ungated, macbook-cipher fails with "nix.gc.automatic requires
  # nix.enable". This is therefore inert on every host that evaluates today, and
  # starts working the moment nix.enable is true somewhere.
  nix = lib.mkIf config.nix.enable {
    gc = {
      automatic = true;
      # launchd StartCalendarInterval -- NOT the NixOS systemd `dates` string.
      # nix.gc.dates, randomizedDelaySec and persistent are all
      # mkRemovedOptionModule on nix-darwin and are hard eval failures.
      interval = {
        Weekday = 1;
        Hour = 3;
        Minute = 15;
      }; # Mon 03:15
      # nix-darwin builds this into a shell string, so a space-separated value
      # is safe here (unlike home-manager's argv-list form).
      options = "--delete-older-than 14d";
    };

    # Preferred over nix.settings.auto-optimise-store on darwin: nix-darwin
    # asserts against that setting outside a narrow nix-version window.
    optimise = {
      automatic = true;
      interval = {
        Weekday = 1;
        Hour = 4;
        Minute = 15;
      }; # Mon 04:15
    };
  };
}
