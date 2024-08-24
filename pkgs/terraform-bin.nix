{ callPackage ? pkgs.callPackage, pkgs ? import <nixpkgs> { } }:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.9.3";
  sha256 = "GTziaar9XETzWc1zp1xcx6qrkk61w2AXhMGHNXWCjsc=";
}
