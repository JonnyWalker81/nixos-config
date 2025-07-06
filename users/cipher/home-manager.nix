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
# rustToolchain = pkgs.fenix.complete.withComponents [
#   "cargo"
#   "clippy"
#   "rust-src"
#   "rustc"
#   "rustfmt"
#   "rust-analyzer"
# ];
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

  home.file.".config/clipcat/clipcatd.toml" = {
    source = ../clipcat/clipcatd.toml;
  };

  home.file.".config/clipcat/clipcatctl.toml" = {
    source = ../clipcat/clipcatctl.toml;
  };

  home.file.".config/clipcat/clipcat-menu.toml" = {
    source = ../clipcat/clipcat-menu.toml;
  };

  # home.file.".config/picom/picom.conf" = { source = ../picom/picom.conf; };

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

  # programs.neovim = {
  #   enable = true;
  #   # package = pkgs.neovim-nightly;
  #   # package = pkgs.unstable.neovim;
  #   package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;

  #   viAlias = true;
  #   vimAlias = true;

  #   extraPackages = [
  #     # Formatters
  #     pkgs.nixfmt-rfc-style # Nix
  #     pkgs.prettierd # Multi-language
  #     pkgs.shfmt # Shell
  #     pkgs.stylua # Lua

  #     # LSP
  #     pkgs.lua-language-server
  #     # inputs.nightly-tools.packages.${pkgs.system}.nixd
  #     pkgs.nil
  #     pkgs.cargo
  #     pkgs.clippy
  #     # pkgs.rust-src
  #     pkgs.rustc
  #     pkgs.rustfmt
  #     pkgs.rust-analyzer
  #   ];
  # };

  programs.go = {
    enable = true;
    package = inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.go;
  };

  # services.picom = {
  # enable = true;
  # activeOpacity = 0.8;
  # inactiveOpacity = 0.5;
  # backend = "glx";
  # settings = {
  #   #   blur = false;
  #   #   blurExclude = [ "\n      class_g = 'slop'" "class_g *?= 'Firefox'" ];
  #   #   blur-method = "dual_kawase";
  #   #   blur-strength = 5;
  #   #
  #   #   # Radius
  #   corner-radius = 10;
  #   round-borders = 1;
  #   rounded-corners-exclude = [ "class_g = 'Custom-taffybar'" ];
  # };
  # extraOptions = ''
  #   corner-radius = 10;
  #   blur-method = "dual_kawase";
  #   blur-strength = "10";
  #   xinerama-shadow-crop = true;
  # '';
  # experimentalBackends = true;

  # shadowExclude = [ "bounding_shaped && !rounded_corners" ];

  # fade = true;
  # fadeDelta = 5;
  # vSync = true;
  # opacityRules = [
  #   "100:class_g = 'firefox' && window_type = 'utility'"
  #   "100:class_g   *?= 'Chromium-browser'"
  #   "100:class_g   *?= 'Firefox'"
  #   "100:class_g   *?= 'gitkraken'"
  #   "100:class_g   *?= 'emacs'"
  #   "100:class_g   ~=  'jetbrains'"
  #   "100:class_g   *?= 'slack'"
  # ];

  # package = pkgs.picom.overrideAttrs (o: {
  #   src = pkgs.fetchFromGitHub {
  #     repo = "picom";
  #     owner = "yshui";
  #     rev = "v12.5"; # Correct version
  #     # rev = "a456d438d0a681365d90a8adbf0941cede41aaa9";
  #     sha256 = "7ohtI890CutwprPEY5njqWou0fD6T9eu51EBSQ2/lWs=";
  #   };
  # });

  # };

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
