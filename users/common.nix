args@{ config, lib, pkgs, ... }:
let
  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  mcfly = pkgs.mcfly;
  mcflyBin = mcfly + "/bin/mcfly";

  homeDir = builtins.getEnv "HOME";

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

  # home.file.".config/rofi/config.rasi" = { source = ./rofi/config.rasi; };

  home.file.".config/rofi/config.rasi".text = ''
    // Write your configuration

    // String interpolation to get the store path
    @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
  '';

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

  home.file.".config/picom/picom.conf" = { source = ./picom/picom.conf; };

  home.file."scripts" = {
    source = ./scripts;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsNativeComp;
    extraPackages = (epkgs: [ epkgs.vterm ]);
  };

  home.packages = [

  ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.fd
    pkgs.ripgrep
    pkgs.go
    pkgs.gopls
    pkgs.gotools
    pkgs.gotestsum
    pkgs.rustup
    pkgs.rust-analyzer
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
    pkgs.font-awesome_5
    pkgs.powerline-fonts
    pkgs.powerline-symbols
    pkgs.zoxide
    # pkgs.fzf
    pkgs.openssl
    pkgs.lsof
    pkgs.gnupg
    pkgs.hunspell
    pkgs.bat
    pkgs.delta
    pkgs.croc
    pkgs.zig-master
    pkgs.bottom
    pkgs.kubernetes-helm
    pkgs.waypoint
    pkgs.helix
    pkgs.unzip
    pkgs.k9s

    zoxide
    pkgs.diskonaut
    pkgs.sqlite
    pkgs.acpi
    pkgs.golangci-lint
    pkgs.kubernetes
    pkgs.pcmanfm
    pkgs.rofi
    pkgs.clipcat
    pkgs.vscode
    pkgs.haskellPackages.libmpd
    pkgs.haskellPackages.xmobar
    pkgs.haskellPackages.xmonad
    pkgs.haskellPackages.greenclip

  ];

  programs.go = {
    enable = true;
    # package = pkgs.go_1_17;
    goPath = "go";
    goPrivate = [ "github.com/geneva" "github.com/jonnywalker81" ];
  };

  # programs.git = {
  #   enable = true;
  #   userName = "Jonathan Rothberg";
  #   extraConfig = {
  #     pull.rebase = true;
  #     init.defaultBranch = "main";
  #     color.ui = true;
  #     credential.helper = "store --file ~/.git-credentials";
  #     url."git@github.com".insteadOf = "https://github.com";
  #   };

  #   aliases = {
  #     bump =
  #       "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
  #   };

  #   delta = {
  #     enable = true;
  #     options = {
  #       syntax-theme = "1337";
  #       plus-color = "#32473d";
  #       minus-color = "#643632";
  #       features = "line-numbers";
  #       whitespace-error-style = "22 reverse";
  #     };
  #   };
  # };

  # services.picom = {
  #   enable = true;
  #   # blur = true;

  # };
  # services.xserver.windowManager.xmonad = {

  #   #  i3.enable = true;
  #   enable = true;
  #   enableContribAndExtras = true;

  #   extraPackages = hpkgs: [
  #     hpkgs.xmonad-contrib
  #     hpkgs.xmonad-extras
  #     hpkgs.xmonad
  #   ];
  # };
  # programs.xsession.picom = { enable = true; };
  # pkgs.windowManagers.xmonad = {
  #   #  i3.enable = true;
  #   enable = true;
  #   enableContribAndExtras = true;

  #   extraPackages = hpkgs: [
  #     hpkgs.xmonad-contrib
  #     hpkgs.xmonad-extras
  #     hpkgs.xmonad
  #   ];
  # };

  programs.mcfly = {
    enable = true;
    enableZshIntegration = false;
    enableFuzzySearch = true;
    keyScheme = "vim";
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "Jonathan Rothberg";
    # userEmail = "jon@geneva.com";
    extraConfig = {
      pull.rebase = false;
      init.defaultBranch = "main";
      color.ui = true;
      core.askPass = ""; # needs to be empty to use terminal for ask pass
      # credential.helper = "store --file ~/.config/git-credentials";
      credential.helper = "store";
      push.default = "tracking";
      branch.autosetuprebase = "always";
      # url."git@github.com".insteadOf = "https://github.com";
    };

    aliases = {
      bump =
        "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
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
      rebuild =
        "sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild switch --flake .#vm-aarch64";
      rp = "sudo nixos-rebuild switch --flake .#vm-aarch64-prl";
      ri = "sudo nixos-rebuild switch --flake .#vm-intel";
      h = "mcfly search -f ''";
      f = "history | fzf --sort --exact";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -o";
    };

    # interactiveShellInit = lib.strings.concatStrings
    #   (lib.strings.intersperse "\n" [ (builtins.readFile ./config.zsh) ]);

    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    sessionVariables = {
      LC_ALL = "en_US.utf8";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      GOPATH = "\${HOME}";
      # GOPRIVATE = "github.com";
      # GOPROXY = "off";
      # PATH =
      #   "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin:~/Repositories/geneva/node_modules/.bin";
      PATH = "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin";
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";

      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
      TERM = "xterm-256color";
      EDITOR = "emacsclient -t -a ''"; # $EDITOR use Emacs in terminal
      VISUAL = "emacsclient -c -a emacs"; # $VISUAL use Emacs in GUI mode

      CLOUDFLARE_EMAIL = "jon@geneva.com";
    };

    # eval "$(${mcflyBin} init zsh)"
    initExtra = ''
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      eval "$(${zoxideBin} init zsh)"
      eval "$(${mcflyBin} init zsh)"
      eval "$(ssh-agent -s)"
      # bindkey "^R" mcfly-history-widget
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

  # programs.neovim = {
  #   enable = true;
  #   package = pkgs.neovim-nightly;

  #   viAlias = true;
  #   vimAlias = true;

  #   plugins = with pkgs; [
  #     customVim.vim-cue
  #     customVim.vim-fish
  #     customVim.vim-fugitive
  #     customVim.vim-glsl
  #     customVim.vim-misc
  #     customVim.vim-pgsql
  #     customVim.vim-tla
  #     customVim.vim-zig
  #     customVim.pigeon
  #     customVim.AfterColors

  #     customVim.vim-nord
  #     customVim.nvim-comment
  #     customVim.nvim-lspconfig
  #     customVim.nvim-plenary # required for telescope
  #     customVim.nvim-telescope
  #     customVim.nvim-treesitter
  #     customVim.nvim-treesitter-playground
  #     customVim.nvim-treesitter-textobjects

  #     vimPlugins.vim-airline
  #     vimPlugins.vim-airline-themes
  #     vimPlugins.vim-eunuch
  #     vimPlugins.vim-gitgutter

  #     vimPlugins.vim-markdown
  #     vimPlugins.vim-nix
  #     vimPlugins.typescript-vim
  #   ];

  #   extraConfig = (import ./vim-config.nix);
  # };

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

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "/tmp/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;

    hashKnownHosts = true;
    userKnownHostsFile = "~/.ssh/known_hosts";

    extraConfig = ''
      HostkeyAlgorithms +ssh-rsa
    '';

    matchBlocks = {
      github = {
        hostname = "github.com";
        identityFile = "~/.ssh/id_ed25519";
        forwardAgent = true;
        user = "jonnywalker81";
      };

      bluebeam = {
        hostname = "scm.bluebeam.com";
        port = 7999;
        identityFile = "~/.ssh/id_ed25519";
        forwardAgent = true;
        user = "git";
        extraOptions = {
          PubkeyAcceptedAlgorithms = "+ssh-rsa";
          HostkeyAlgorithms = "+ssh-rsa";
        };
      };
    };
  };

  programs.alacritty = {
    enable = true;

    settings = {
      env.TERM = "xterm-256color";
      font = {
        size = 12.0;
        use_thin_strokes = true;

        normal.family = "JetBrains Mono";
        bold.family = "JetBrains Mono Medium";
        italic.family = "JetBrains Mono Book Italic";
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

      # key_bindings = [
      #   {
      #     key = "K";
      #     mods = "Command";
      #     chars = "ClearHistory";
      #   }
      #   {
      #     key = "V";
      #     mods = "Command";
      #     action = "Paste";
      #   }
      #   {
      #     key = "C";
      #     mods = "Command";
      #     action = "Copy";
      #   }
      #   {
      #     key = "Key0";
      #     mods = "Command";
      #     action = "ResetFontSize";
      #   }
      #   {
      #     key = "Equals";
      #     mods = "Command";
      #     action = "IncreaseFontSize";
      #   }
      #   {
      #     key = "Subtract";
      #     mods = "Command";
      #     action = "DecreaseFontSize";
      #   }
      # ];
    };
  };
}
