{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/release-21.11";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {self,  home-manager, nixpkgs, ... }@inputs:
  let
   mkVM = import ./lib/mkvm.nix; 

   overlays = [
   ];
  in {
    nixosConfiguration.dev = mkVM "dev" rec {
      inherit overlays nixpkgs home-manager;
      system = "aarch64-linux";
      user = "cipher";
    };
  };
}

