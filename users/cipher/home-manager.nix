args@{ config, lib, pkgs, nix-doom-emacs, ... }:
let
  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  mcfly = pkgs.mcfly;
  mcflyBin = mcfly + "/bin/mcfly";

in {
  home.file.".doom.d" = {
    source = ./doom.d;
    recursive = true;
  };

  home.file.".elisp" = {
    source = ./elisp;
    recursive = true;
  };

  home.file.".xmonad/xmonad.hs" = { source = ./xmonad/xmonad.hs; };

  home.file.".config/xmobar/.xmobarrc" = { source = ./xmobar/.xmobarrc; };

  home.file.".config/kitty/kitty.conf" = { source = ./kitty/kitty.conf; };

  home.file.".config/rofi/config.rasi" = { source = ./rofi/config.rasi; };

  home.file.".config/greenclip.toml" = { source = ./greenclip/greenclip.toml; };

  home.file.".config/clipcat/clipcatd.toml" = {
    source = ./clipcat/clipcatd.toml;
  };

  home.file.".config/clipcat/clipcatctl.toml" = {
    source = ./clipcat/clipcatctl.toml;
  };

  home.file.".config/clipcat/clipcat-menu.toml" = {
    source = ./clipcat/clipcat-menu.toml;
  };

  home.file."scripts" = {
    source = ./scripts;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = (epkgs: [ epkgs.vterm ]);
  };

  home.packages = [
    pkgs.fd
    pkgs.ripgrep
    pkgs.go_1_17
    pkgs.gopls
    pkgs.goimports
    pkgs.rustup
    pkgs.clang
    pkgs.just
    pkgs.docker-compose
    pkgs.awscli
    pkgs.postgresql_14
    pkgs.jq
    pkgs.exa
    pkgs.thefuck
    pkgs.feh
    pkgs.xplr
    pkgs.kitty
    pkgs.pcmanfm
    pkgs.rofi
    pkgs.font-awesome
    pkgs.powerline-fonts
    pkgs.powerline-symbols
    pkgs.clipcat
    pkgs.zoxide
    pkgs.fzf
    pkgs.openssl
    pkgs.lsof
    pkgs.gnupg
    pkgs.hunspell
    pkgs.bat
    pkgs.delta
    pkgs.acpi

    pkgs.haskellPackages.libmpd
    pkgs.haskellPackages.xmobar
    pkgs.haskellPackages.xmonad
    pkgs.haskellPackages.greenclip
    zoxide
  ];

  programs.mcfly = {
    enable = true;
    enableZshIntegration = false;
    enableFuzzySearch = true;
    keyScheme = "vim";
  };

  programs.git = {
    enable = true;
    userName = "Jonathan Rothberg";
    userEmail = "jon@geneva.com";
    extraConfig = {
      pull.rebase = true;
      init.defaultBranch = "main";
      color.ui = true;
      credential.helper = "store --file ~/.git-credentials";
    };

    delta = {
      enable = true;
      options = {
        syntax-theme = "1337";
        plus-color = "#32473d";
        minus-color = "#643632";
        features = "line-numbers";
        whitespace-error-style = "22 reverse";
      };
    };
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      ll = "exa -l";
      l = "exa -la";
      rebuild = "sudo nixos-rebuild switch --flake .#vm-aarch64";
      mf = "mcfly search";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      pbcopy = "xclip";
      pbpaste = "xclip -o";
    };

    enableAutosuggestions = true;
    sessionVariables = {
      LC_ALL = "en_US.utf8";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      GOPATH = "\${HOME}";
      PATH = "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin";
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";

      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
      TERM = "xterm-256color";
      EDITOR = "emacsclient -t -a ''"; # $EDITOR use Emacs in terminal
      VISUAL = "emacsclient -c -a emacs"; # $VISUAL use Emacs in GUI mode

    };

    # eval "$(${mcflyBin} init zsh)"
    initExtra = ''
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      eval "$(${zoxideBin} init zsh)"
      eval "$(${mcflyBin} init zsh)"
      eval "$(ssh-agent -s)"
    '';

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" "thefuck" ];
      theme = "robbyrussell";
    };

    history = {
      size = 100000;
      path = "${config.xdg.dataHome}/zsh/history";
    };
  };

  programs.neovim.enable = true;
  programs.neovim.viAlias = true;
  programs.neovim.vimAlias = true;

  programs.direnv = {
    enable = true;
    nix-direnv = { enable = true; };
  };

  programs.starship = {
    enable = true;
    # Configuration written to ~/.config/starship.toml
    settings = {
      add_newline = true;

      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };
    };
  };

  programs.home-manager.enable = true;

  programs.ssh = { enable = true; };

  programs.alacritty = {
    enable = true;

    settings = {
      env.TERM = "xterm-256color";

      key_bindings = [
        {
          key = "K";
          mods = "Command";
          chars = "ClearHistory";
        }
        {
          key = "V";
          mods = "Command";
          action = "Paste";
        }
        {
          key = "C";
          mods = "Command";
          action = "Copy";
        }
        {
          key = "Key0";
          mods = "Command";
          action = "ResetFontSize";
        }
        {
          key = "Equals";
          mods = "Command";
          action = "IncreaseFontSize";
        }
        {
          key = "Subtract";
          mods = "Command";
          action = "DecreaseFontSize";
        }
      ];
    };
  };
}
