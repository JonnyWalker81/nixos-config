args@{ config, lib, pkgs, ... }: {

  imports = [ ../common.nix ];

  programs.git.userEmail = "jon@geneva.com";

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
