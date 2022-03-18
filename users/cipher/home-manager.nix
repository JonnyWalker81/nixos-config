args@{ config, lib, pkgs, nix-doom-emacs, ... }: {
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
      credential.helper = "store";
    };
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      ll = "exa -l";
      l = "exa -la";
      rebuild = "sudo nixos-rebuild switch --flake .#vm-aarch64";
    };

    enableAutosuggestions = true;
    sessionVariables = {
      LC_ALL = "en_US.utf8";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      GOPATH = "\${HOME}";
      PATH = "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin";

      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
    };

    initExtra = ''
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
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
}
