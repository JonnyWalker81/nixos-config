# This function creates a NixOS system based on our VM setup for a
# particular architecture.
name:
{ nixpkgs, home-manager, system, user, overlays, darwin, inputs, ... }@args:

darwin.lib.darwinSystem rec {
  inherit system;

  modules = [
    # Apply our overlays. Overlays are keyed by system type so we have
    # to go through and apply our system type. We do this first so
    # the overlays are available globally.
    {
      nixpkgs.overlays = overlays;
      nixpkgs.config.allowUnfree = true;
    }

    # ../hardware/${name}.nix
    ../machines/vm-darwin.nix
    ../users/${user}-darwin/nixos.nix
    home-manager.darwinModules.home-manager
    {
      home-manager.useGlobalPkgs = true;
      home-manager.useUserPackages = true;
      home-manager.backupFileExtension = "hm-backup";
      home-manager.users.${user}.imports =
        [ ../users/${user}-darwin/home-manager.nix ];
      home-manager.extraSpecialArgs = { inherit inputs; };
    }

    {
      config._module.args = {
        currentSystemName = name;
        currentSystem = system;
      };
    }
  ];
}
