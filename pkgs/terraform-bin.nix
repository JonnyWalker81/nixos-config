{
  callPackage ? pkgs.callPackage,
  pkgs ? import <nixpkgs> { },
}:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.10.5";
  sha256 = "sha256-DKXWl3x8Rr+ku+AwAwuRHol88Mtyv/VSX7dsEPHDQJo=";
}
