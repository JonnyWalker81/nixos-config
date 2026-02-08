{
  callPackage ? pkgs.callPackage,
  pkgs ? import <nixpkgs> { },
}:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.14.3";
  sha256 = "sha256-+1sAQHDl5DghSnTdyjIcIiRveZ5cvh7p31glKOlT+Mw=";
}
