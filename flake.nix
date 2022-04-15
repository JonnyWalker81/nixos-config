{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.url = "github:he-la/nix-doom-emacs/develop";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    # nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    nix-doom-emacs.url = "github:vlaci/nix-doom-emacs/";
    zig.url = "github:arqv/zig-overlay";
  };

  # inputs = {
  #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
  #  nur = { url = "github:nix-community/NUR"; };
  #};

  outputs = { self, nixpkgs, home-manager, nix-doom-emacs, ... }@inputs:
    let
      # picom_overlay = (self: super: {
      #   picom = super.picom.overrideAttrs (prev: {
      #     version = "git";
      #     src = super.fetchFromGitHub {
      #       owner = "jonaburg";
      #       repo = "picom";
      #       rev = "e3c19cd7d1108d114552267f302548c113278d45";
      #       sha256 = "VBnIzisg/7Xetd/AWVHlnaWXlxX+wqeYTpstO6+T5cE=";
      #     };
      #   });
      # });
      mkVM = import ./lib/mkvm.nix;

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
        inherit overlays nixpkgs home-manager nix-doom-emacs;
        system = "aarch64-linux";
        user = "cipher";
      };

    };

}
