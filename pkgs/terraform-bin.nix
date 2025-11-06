{
  callPackage ? pkgs.callPackage,
  pkgs ? import <nixpkgs> { },
}:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.13.0";
  sha256 = "sha256-41qQjkBlNu8bp7hT3A4P7RQWMOBLpLgATt7qGRmqw4U=";
}
