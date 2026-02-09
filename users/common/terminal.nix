{ config, lib, pkgs, ... }:

{
  programs.alacritty = {
    enable = true;

    settings = {
      env.TERM = "xterm-256color";
      font = {
        size = 12.0;

        normal.family = "JetBrains Mono";
        bold.family = "JetBrains Mono Medium";
        italic.family = "JetBrains Mono Medium";
      };

      selection = { save_to_clipboard = true; };

      key_bindings = [
        {
          key = "C";
          mods = "Command";
          action = "Copy";
        }
        {
          key = "V";
          mods = "Command";
          action = "Paste";
        }
      ];
    };
  };

  # Terminal config symlinks
  home.file.".config/kitty/kitty.conf" = { source = ../kitty/kitty.conf; };

  home.file.".wezterm.lua" = { source = ../wezterm/wezterm.lua; };

  home.file.".config/ghostty/config" = {
    source = if pkgs.stdenv.isDarwin then
      ../ghostty/config-macos
    else
      ../ghostty/config-linux;
  };
}
