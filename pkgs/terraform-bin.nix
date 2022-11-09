{ callPackage ? pkgs.callPackage, pkgs ? import <nixpkgs> { } }:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.1.9";
  sha256 = "6KCdH+WmjtdeX6vibGCa0Sp+RZAC3qZUPxCEmTuHomY=";
}
