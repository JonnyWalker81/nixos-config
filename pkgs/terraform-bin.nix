{
  callPackage ? pkgs.callPackage,
  pkgs ? import <nixpkgs> { },
}:

callPackage (import ./hashicorp/generic.nix) {
  name = "terraform";
  version = "1.12.2";
  sha256 = "sha256-+KA0fcXmjm1gqfots2F2LnlD7QhKdz8oqYHZiM62/ck=";
}
