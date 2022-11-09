{ config, lib, pkgs, ... }:

{
  imports = [ ../common.nix ];

  programs.google-chrome-dev = { enable = true; };

  programs.git.userEmail = "jrothberg@bluebeam.com";

  home.file.".config/rofi/config.rasi".text = ''
    // Write your configuration

    // String interpolation to get the store path
    @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
  '';

  home.file.".config/greenclip.toml" = {
    source = ../greenclip/greenclip.toml;
  };

  home.file.".config/clipcat/clipcatd.toml" = {
    source = ../clipcat/clipcatd.toml;
  };

  home.file.".config/clipcat/clipcatctl.toml" = {
    source = ../clipcat/clipcatctl.toml;
  };

  home.file.".config/clipcat/clipcat-menu.toml" = {
    source = ../clipcat/clipcat-menu.toml;
  };

  home.file.".config/picom/picom.conf" = { source = ../picom/picom.conf; };

  home.file.".xmonad/xmonad.hs" = { source = ../xmonad/xmonad.hs; };

  home.file.".config/xmobar/.xmobarrc" = { source = ../xmobar/.xmobarrc; };

  # Make cursor not tiny on HiDPI screens
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
    size = 32;
    # name = "Vanilla-DMZ";
    # package = pkgs.vanilla-dmz;
    # size = 64;
    x11.enable = true;
  };

}
