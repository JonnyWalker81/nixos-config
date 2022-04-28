{ config, lib, pkgs, ... }:

{
  imports = [ ../common.nix ];

  programs.googlechome-dev = { enable = true; };

}
