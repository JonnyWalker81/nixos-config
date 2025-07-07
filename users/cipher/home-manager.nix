{ isWSL, inputs, ... }:

{
  config,
  lib,
  pkgs,
  ...
}:
let
  common = import ../common.nix {
    inherit
      config
      lib
      pkgs
      isWSL
      inputs
      ;
    system = pkgs.system;

  };
in
{

  imports = [ 
    common 
    ../hyprland.nix
  ];

  programs.git.userEmail = "jon@join.build";

  home.file.".config/rofi/config.rasi".text = ''
    // Write your configuration

    // String interpolation to get the store path
    @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
  '';

  home.file.".config/greenclip.toml" = {
    source = ../greenclip/greenclip.toml;
  };

  # Clipcat config files (service disabled to use custom configs)
  home.file.".config/clipcat/clipcatd.toml" = {
    source = ../clipcat/clipcatd.toml;
  };
  home.file.".config/clipcat/clipcatctl.toml" = {
    source = ../clipcat/clipcatctl.toml;
  };
  home.file.".config/clipcat/clipcat-menu.toml" = {
    source = ../clipcat/clipcat-menu.toml;
  };

  home.file.".config/picom/picom.conf" = { source = ../picom-yushi.conf; };

  home.file.".xmonad/xmonad.hs" = {
    source = ../xmonad/xmonad.hs;
  };

  home.file.".config/xmobar/.xmobarrc" = {
    source = ../xmobar/.xmobarrc;
  };
  
  home.file.".config/waybar/config" = {
    source = ../waybar/config;
  };
  
  home.file.".config/waybar/style.css" = {
    source = ../waybar/style.css;
  };

  home.file.".config/awesome/rc.lua" = {
    source = ../awesome/rc.lua;
  };

  home.file.".config/awesome/theme.lua" = {
    source = ../awesome/theme.lua;
  };

  home.file."scripts/wttr.sh" = {
    source = ../scripts/wttr.sh;
    executable = true;
  };

  programs.go = {
    enable = true;
    package = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.go;
  };

  services.picom = {
    enable = lib.mkForce false;  # Disabled - using custom config file instead
  };
  
  # Custom systemd user service for picom
  systemd.user.services.picom-custom = {
    Unit = {
      Description = "Picom compositor (jonaburg fork with animations)";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };
    Service = {
      ExecStart = "${pkgs.picom}/bin/picom --config %h/.config/picom/picom.conf";
      Restart = "on-failure";
      RestartSec = 3;
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
  
  services.clipcat = {
    enable = false;  # Disabled to avoid config conflicts, using manual config files
  };

  # Make cursor not tiny on HiDPI screens
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 32;
    # name = "Vanilla-DMZ";
    # package = pkgs.vanilla-dmz;
    # size = 64;
    x11.enable = true;
  };
}