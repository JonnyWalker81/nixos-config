# This function creates a NixOS or nix-darwin system configuration.
# It is the single unified builder for all system types.
#
# Partial application: import with { nixpkgs, overlays, inputs }
# Then call with: name { system, user, darwin?, wsl?, nixpkgs?, extraModules? }
{ nixpkgs, overlays, inputs, }:

name:
{ system, user, darwin ? false, wsl ? false, nixpkgsOverride ? null
, # Optional nixpkgs override (e.g. old-kernel for vm-aarch64)
extraModules ? [ ], # Additional modules (e.g. nix-homebrew for Darwin)
}:

let
  # True if this is a WSL system.
  isWSL = wsl;

  # Resolve which nixpkgs to use: per-system override or the default from closure.
  resolvedNixpkgs =
    if nixpkgsOverride != null then nixpkgsOverride else nixpkgs;

  # The config files for this system.
  machineConfig = ../machines/${name}.nix;

  # OS-specific config: nixos.nix for Linux, darwin.nix for macOS
  # All user configs live under users/${user}/ (unified directory per user).
  userOSConfig =
    if darwin then ../users/${user}/darwin.nix else ../users/${user}/nixos.nix;
  # home-manager.nix is shared across platforms (uses platform guards internally)
  userHMConfig = ../users/${user}/home-manager.nix;

  # NixOS vs nix-darwin functions
  systemFunc = if darwin then
    inputs.darwin.lib.darwinSystem
  else
    resolvedNixpkgs.lib.nixosSystem;
  home-manager = if darwin then
    inputs.home-manager.darwinModules
  else
    inputs.home-manager.nixosModules;
in systemFunc rec {
  inherit system;

  modules = [
    # Apply our overlays. Overlays are keyed by system type so we have
    # to go through and apply our system type. We do this first so
    # the overlays are available globally.
    {
      nixpkgs.overlays = overlays;
    }

    # Allow unfree packages.
    {
      nixpkgs.config.allowUnfree = true;
    }

    # Bring in WSL if this is a WSL build
    (if isWSL then inputs.nixos-wsl.nixosModules.wsl else { })

    machineConfig
    userOSConfig
    home-manager.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = if darwin then false else true;
      home-manager.backupFileExtension = "hm-backup";
      home-manager.users.${user} = import userHMConfig {
        isWSL = isWSL;
        isDarwin = darwin;
        inputs = inputs;
      };
    }

    # We expose some extra arguments so that our modules can parameterize
    # better based on these values.
    {
      config._module.args = {
        currentSystem = system;
        currentSystemName = name;
        currentSystemUser = user;
        isWSL = isWSL;
        inputs = inputs;
      };
    }
  ] ++ extraModules;
}
