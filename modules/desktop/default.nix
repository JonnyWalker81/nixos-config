{ config, lib, pkgs, ... }:

{
  imports = [ ./xmonad.nix ./dwm.nix ./awesome.nix ./hyprland.nix ];
}
