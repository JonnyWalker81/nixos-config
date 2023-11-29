{ callPackage ? pkgs.callPackage, pkgs ? import <nixpkgs> { } }:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.5.3";
  sha256 = "d2x4KBwbUX0eLZ54suYJALjvns1RxKXS/6aPZv6jXdI=";
}
