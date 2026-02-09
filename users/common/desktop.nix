{ config, lib, pkgs, ... }:

{
  programs.firefox = lib.mkIf (!pkgs.stdenv.isDarwin) {
    package = pkgs.firefox;
    enable = true;
    profiles = {
      default = {
        id = 0;
        name = "default";
        isDefault = true;
        settings = {
          # HiDPI/4K display scaling settings
          "layout.css.devPixelsPerPx" = "0.5";
          "browser.display.use_system_colors" = false;
          "browser.display.use_document_fonts" = 1;
          "font.size.variable.x-western" = 16;
          "font.size.fixed.x-western" = 13;
          "font.minimum-size.x-western" = 12;

          # Zoom settings
          "browser.zoom.full" = true;
          "zoom.minPercent" = 100;
          "zoom.maxPercent" = 500;
          "toolkit.zoomManager.zoomValues" = "0.5,0.75,1,1.25,1.5,1.75,2,2.5,3";

          # Better readability
          "gfx.webrender.enabled" = true;
          "layers.acceleration.force-enabled" = true;
          "layout.frame_rate" = 60;

          # Disable mouse button 4/5 navigation (fixes Parallels VM focus issue)
          # This prevents Firefox from interpreting focus clicks as back/forward navigation
          "mousebutton.4th.enabled" = false;
          "mousebutton.5th.enabled" = false;

          # Also disable swipe gestures to prevent accidental navigation
          "browser.gesture.swipe.left" = "";
          "browser.gesture.swipe.right" = "";
        };
        search = {
          force = true;
          default = "google";
          order = [ "google" "Searx" ];
          engines = {
            "Nix Packages" = {
              urls = [{
                template = "https://search.nixos.org/packages";
                params = [
                  {
                    name = "type";
                    value = "packages";
                  }
                  {
                    name = "query";
                    value = "{searchTerms}";
                  }
                ];
              }];
              icon =
                "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
            "NixOS Wiki" = {
              urls = [{
                template = "https://nixos.wiki/index.php?search={searchTerms}";
              }];
              icon = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@nw" ];
            };
            "Searx" = {
              urls = [{
                template = "https://searx.aicampground.com/?q={searchTerms}";
              }];
              icon = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@searx" ];
            };
            bing.metaData.hidden = true;
            "google".metaData.alias =
              "@g"; # builtin engines only support specifying one additional alias
          };
        };
      };
    };
  };

  services.picom = lib.mkIf (!pkgs.stdenv.isDarwin) {
    enable = true;

    # Backend
    backend = "glx";
    vSync = true;

    # Shadows - matching Hyprland's shadow settings
    shadow = true;
    shadowOffsets = [ (-8) (-8) ];
    shadowOpacity = 0.35;
    shadowExclude = [
      "name = 'Notification'"
      "class_g = 'xmobar'"
      "class_g = 'dwm'"
      "window_type = 'dock'"
      "window_type = 'desktop'"
    ];

    # Fading - smooth transitions like Hyprland
    fade = true;
    fadeSteps = [ 2.5e-2 2.5e-2 ];
    fadeDelta = 4;

    # Opacity - matching Hyprland's 0.85 transparency
    activeOpacity = 0.85;
    inactiveOpacity = 0.85;

    # Opacity rules - Firefox, Emacs, and browsers at 100%
    opacityRules = [
      "100:class_g = 'Firefox'"
      "100:class_g = 'firefox'"
      "100:class_g = 'Navigator'"
      "100:class_g = 'Chromium'"
      "100:class_g = 'Emacs'"
      "100:class_g = 'emacs'"
      "100:class_g = 'mpv'"
      "100:class_g = 'xmobar'"
      "100:class_g = 'dwm'"
      "100:_NET_WM_WINDOW_TYPE@[0]:a = '_NET_WM_WINDOW_TYPE_DOCK'"
    ];

    # Window type settings
    wintypes = {
      tooltip = {
        fade = true;
        shadow = false;
        opacity = 0.95;
        focus = true;
      };
      dock = { shadow = false; };
      dnd = { shadow = false; };
      popup_menu = {
        opacity = 0.98;
        shadow = true;
      };
      dropdown_menu = { opacity = 0.98; };
    };

    # Additional settings via extraArgs
    settings = {
      # Blur - matching Hyprland's blur
      blur-background = true;
      blur-method = "dual_kawase";
      blur-strength = 4;
      blur-deviation = 1.0;
      blur-background-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "class_g = 'xmobar'"
        "class_g = 'dwm'"
        "class_g = 'slop'"
      ];

      # Shadows
      shadow-radius = 8;
      shadow-color = "#1a1b26";

      # Corners - matching Hyprland's rounding=10
      corner-radius = 10;
      rounded-corners-exclude = [
        "window_type = 'dock'"
        "window_type = 'desktop'"
        "class_g = 'xmobar'"
        "class_g = 'dwm'"
        "class_g = 'Polybar'"
        "class_g = 'Dunst'"
      ];

      # General
      mark-wmwin-focused = true;
      mark-ovredir-focused = true;
      detect-rounded-corners = true;
      detect-client-opacity = true;
      detect-transient = true;
      detect-client-leader = true;
      use-ewmh-active-win = true;
      glx-copy-from-front = false;
      use-damage = true;
      xrender-sync-fence = true;
    };
  };
}
