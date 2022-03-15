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
  };

 # inputs = {
 #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
 #  nur = { url = "github:nix-community/NUR"; };
 #};

    outputs = { self, nixpkgs, nixpkgs-unstable, home-manager, nix-doom-emacs, ... }@inputs: let
    mkVM = import ./lib/mkvm.nix;

    # Overlays is the list of overlays we want to apply from flake inputs.
    overlays = [
      inputs.emacs-overlay.overlay
          ];
  in {
    nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" rec {
      inherit overlays nixpkgs nixpkgs-unstable home-manager nix-doom-emacs;
      system = "aarch64-linux";
      user   = "cipher";
    };

    };

 }

