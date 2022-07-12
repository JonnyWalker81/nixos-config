{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-22.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-21.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Locks nixpkgs to an older version with an older Kernel that boots
    # on VMware Fusion Tech Preview. This can be swapped to nixpkgs when
    # the TP fixes the bug.
    nixpkgs-old-kernel.url =
      "github:nixos/nixpkgs/bacbfd713b4781a4a82c1f390f8fe21ae3b8b95b";
    # nix-doom-emacs.url = "github:he-la/nix-doom-emacs/develop";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    # nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # nix-doom-emacs.url = "github:vlaci/nix-doom-emacs/";
    zig.url = "github:arqv/zig-overlay";
  };

  # inputs = {
  #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
  #  nur = { url = "github:nix-community/NUR"; };
  #};

  outputs = { self, darwin, nixpkgs, home-manager, ... }@inputs:
    let
      mkVM = import ./lib/mkvm.nix;
      mkVMDarwin = import ./lib/mkvm-darwin.nix;

      # Overlays is the list of overlays we want to apply from flake inputs.
      overlays = [
        inputs.emacs-overlay.overlay
        (final: prev: {
          # Zig doesn't export an overlay so we do it here
          zig-master = inputs.zig.packages.${prev.system}.master.latest;

          # Go we always want the latest version
          go = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.go_1_18;

          # To get Kitty 0.24.x. Delete this once it hits release.
          kitty = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.kitty;

          xmobar = inputs.nixpkga-unstable.legacyPackages.${prev.system}.xmobar;
        })
        # (import (fetchGit { url = "https://github.com/jonaburg/picom"; }))
      ];
    in {
      nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" rec {
        inherit overlays home-manager;
        nixpkgs = inputs.nixpkgs-old-kernel;
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-intel = mkVM "vm-intel" rec {
        inherit overlays nixpkgs home-manager;
        system = "x86_64-linux";
        user = "jrothberg";
      };

      nixosConfigurations.vm-aarch64-prl = mkVM "vm-aarch64-prl" rec {
        inherit overlays nixpkgs home-manager;
        system = "aarch64-linux";
        user = "cipher";
      };

      darwinConfigurations.vm-intel = mkVMDarwin "vm-intel" rec {
        inherit overlays nixpkgs home-manager;
        system = "x86_64-darwin";
        user = "jrothber";
      };

    };

}
