{
  callPackage ? pkgs.callPackage,
  pkgs ? import <nixpkgs> { },
}:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.15.0";
  sha256 = "sha256-fZuKuBeQdxtchywGqJgrwltmlxko3MskT+CkHiRRdwo=";
}
