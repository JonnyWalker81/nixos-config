{ config, lib, pkgs, inputs, ... }:

{
  options.desktop.hyprland.enable =
    lib.mkEnableOption "Hyprland Wayland compositor";

  config = lib.mkIf config.desktop.hyprland.enable {
    programs.hyprland = {
      enable = true;
      package = inputs.hyprland.packages.${pkgs.system}.hyprland;
      xwayland.enable = true;
    };
  };
}
