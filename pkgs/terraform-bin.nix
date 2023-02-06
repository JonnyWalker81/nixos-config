{ callPackage ? pkgs.callPackage, pkgs ? import <nixpkgs> { } }:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.3.4";
  sha256 = "ZTgca2Gy0amIkhmfZJpXZP9adyCApz1w+GYyReZALDk=";
}
