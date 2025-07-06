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

  services.picom = {
    enable = true;
    package = pkgs.picom-pijulius;
    
    # Performance optimized settings
    backend = "glx";
    vSync = true;
    
    # Transparency settings
    activeOpacity = 1.0;
    inactiveOpacity = 0.9;
    
    # Fade settings for performance
    fade = true;
    fadeDelta = 8;
    fadeSteps = [ 0.06 0.06 ];
    
    # Shadow disabled for performance
    shadow = false;
    
    # Window type settings
    wintypes = {
      normal = { fade = true; shadow = false; opacity = 1; };
      tooltip = { fade = true; shadow = false; opacity = 0.9; focus = true; full-shadow = false; };
      dock = { shadow = false; fade = false; opacity = 1; };
      dnd = { shadow = false; fade = false; opacity = 1; };
      popup_menu = { opacity = 0.95; fade = true; };
      dropdown_menu = { opacity = 0.95; fade = true; };
      dialog = { fade = true; shadow = false; opacity = 1; };
    };
    
    # Performance rules
    opacityRules = [
      "100:class_g = 'firefox'"
      "100:class_g = 'Firefox'"
      "100:class_g = 'Chromium'"
      "100:class_g = 'chromium'"
      "100:class_g = 'Google-chrome'"
      "100:class_g = 'code-oss'"
      "100:class_g = 'Code'"
      "100:class_g = 'discord'"
      "100:class_g = 'mpv'"
      "100:class_g = 'Vlc'"
      "100:fullscreen = 1"
      "95:class_g = 'URxvt' && !focused"
      "95:class_g = 'XTerm' && !focused"
      "95:class_g = 'Alacritty' && !focused"
      "95:class_g = 'kitty' && !focused"
      "95:class_g = 'ghostty' && !focused"
    ];
    
    settings = {
      # GLX backend optimizations (removed deprecated options)
      glx-copy-from-front = false;
      
      # Damage tracking - critical for performance
      use-damage = true;
      
      # Other performance settings
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      detect-transient = true;
      detect-client-leader = true;
      
      # Unredirect fullscreen windows for better performance
      unredir-if-possible = true;
      unredir-if-possible-delay = 0;
      unredir-if-possible-exclude = [
        "class_g = 'firefox'"
        "class_g = 'Firefox'"
      ];
      
      # Corner radius settings
      corner-radius = 10;
      round-borders = 1;
      rounded-corners-exclude = [
        "class_g = 'awesome'"
        "class_g = 'URxvt'"
        "class_g = 'XTerm'"
        "class_g = 'Alacritty'"
        "class_g = 'Polybar'"
        "class_g = 'code-oss'"
        "class_g = 'firefox'"
        "class_g = 'Thunderbird'"
        "class_g = 'xmobar'"
        "class_g = 'XMonad'"
      ];
      
      # Blur disabled for performance
      blur = {
        method = "none";
      };
      
      # Focus exclude to prevent unnecessary redraws
      focus-exclude = [
        "class_g = 'Cairo-clock'"
        "class_g = 'slop'"
      ];
      
      # Fade exclude
      fade-exclude = [
        "class_g = 'slop'"
      ];
      
      # Logging
      log-level = "warn";
      log-file = "/tmp/picom.log";
      
      # Pijulius animations
      transition-length = 300;
      transition-pow-x = 0.3;
      transition-pow-y = 0.3;
      transition-pow-w = 0.3;
      transition-pow-h = 0.3;
      size-transition = true;
      
      # Workspace switch animation (pijulius feature)
      workspace-switch-duration = 300;
    };
    
    # Extra options including transparent-clipping
    extraArgs = [ "--transparent-clipping" ];
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
