{ config, lib, pkgs, ... }:

{
  imports = [ ../common.nix ];

  programs.google-chrome-dev = { enable = true; };

}
