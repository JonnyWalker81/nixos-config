{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

 # inputs = {
 #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
 #  nur = { url = "github:nix-community/NUR"; };
 #};

outputs = { self, nixpkgs, home-manager, ... }@inputs: let
    mkVM = import ./lib/mkvm.nix;

    # Overlays is the list of overlays we want to apply from flake inputs.
    overlays = [
          ];
  in {
    nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" rec {
      inherit overlays nixpkgs home-manager;
      system = "aarch64-linux";
      user   = "cipher";
    };

    };

 }

