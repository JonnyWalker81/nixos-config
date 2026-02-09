{ isWSL, inputs, ... }:

{ config, lib, pkgs, ... }:
let
  common = import ../common.nix {
    inherit config lib pkgs isWSL inputs;
    system = pkgs.stdenv.hostPlatform.system;
  };
in {
  imports = [ common ];

  # programs.google-chrome-dev = { enable = true; };

  programs.git.userEmail = "jrothberg@bluebeam.com";
}
