args@{ config, lib, pkgs, ... }:
let
  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  mcfly = pkgs.mcfly;
  mcflyBin = mcfly + "/bin/mcfly";

  homeDir = builtins.getEnv "HOME";

  isDarwin = pkgs.stdenv.isDarwin;
  manpager = (pkgs.writeShellScriptBin "manpager" (if isDarwin then ''
    sh -c 'col -bx | bat -l man -p'
  '' else ''
    cat "$1" | col -bx | bat --language man --style plain
  ''));

in {
  home.stateVersion = "18.09";
  xdg.enable = true;

  home.file.".doom.d" = {
    source = ./doom.d;
    recursive = true;
  };

  home.file.".elisp" = {
    source = ./elisp;
    recursive = true;
  };

  home.file.".config/kitty/kitty.conf" = { source = ./kitty/kitty.conf; };

  # home.file.".config/rofi/config.rasi" = { source = ./rofi/config.rasi; };

  home.file."scripts" = {
    source = ./scripts;
    recursive = true;
  };

  home.file.".psqlrc".text = ''
     --  set PROMPT1 '[%m] '
     -- set PROMPT2 '[%m:trx] '
    -- select split_part(:'HOST','.',1) = 'komodo' as is_prod gset
    -- if :is_prod
    --    SET SESSION CHARACTERISTICS AS TRANSACTION READ ONLY;
    --    echo '**************************************************************************************************'
    --    echo 'Connected to the PRODUCTION DB. UPDATE WITH CAUTION.'
    --    echo 'Session READ ONLY by default. Change with: SET SESSION CHARACTERISTICS AS TRANSACTION READ WRITE;'
    --    echo '**************************************************************************************************'
    -- endif
    -- 	iming
  '';

  home.file.".config/gitui/key_bindings.ron" = {
    source = ./gitui/key_bindings.ron;
  };

  home.file.".config/nvim" = {
    source = ./nvim;
    recursive = true;
  };

  home.file.".config/dune/config" = {
    source = ./dune/config;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs-unstable;
    extraPackages = (epkgs: [ epkgs.vterm ]);
  };

  home.packages = [
    pkgs.jetbrains-mono
    pkgs.ripgrep
    pkgs.fd
    # pkgs.rustup
    # pkgs.rust-analyzer
    pkgs.thefuck
    pkgs.zoxide
    pkgs.bat
    pkgs.delta
    zoxide
    pkgs.jq
    pkgs.exa
    pkgs.k9s
    pkgs.procs
    pkgs.graphviz
    pkgs.fira-code
    pkgs.fira-code-symbols
    # pkgs.gcc_latest
    pkgs.llvm
    # pkgs.jetbrains.datagrip
    pkgs.gitui
    pkgs.bind
    # pkgs.firefox-bin
    # pkgs.firefox-esr-102-unwrapped
    pkgs.firefox-unwrapped
    pkgs.pgmanage
    pkgs.pgadmin4
    pkgs.pandoc
    pkgs.terraform-ls
    pkgs.tree-sitter
    pkgs.file
    pkgs.nil
    pkgs.nixpkgs-fmt
    pkgs.nixfmt
    pkgs.shfmt
    # pkgs.opam
    # pkgs.ocamlPackages.ocaml-lsp
    # pkgs.ocamlPackages.findlib
    # pkgs.tree-sitter.withPlugins
    # (p: [ p.tree-sitter-tsx p.tree-sitter-go ])
    # pkgs.tree-sitter-grammars.tree-sitter-c
    # pkgs.tree-sitter-grammars.tree-sitter-go
    # pkgs.tree-sitter-grammars.tree-sitter-tsx
    # pkgs.tree-sitter-grammars.tree-sitter-sql
    # pkgs.tree-sitter-grammars.tree-sitter-nix
    # pkgs.tree-sitter-grammars.tree-sitter-hcl
    # pkgs.tree-sitter-grammars.tree-sitter-css
    # pkgs.tree-sitter-grammars.tree-sitter-cpp
    # pkgs.tree-sitter-grammars.tree-sitter-yaml
    # pkgs.tree-sitter-grammars.tree-sitter-rust
    # pkgs.tree-sitter-grammars.tree-sitter-json
    # pkgs.tree-sitter-grammars.tree-sitter-html
    # pkgs.tree-sitter-grammars.tree-sitter-bash
    # pkgs.tree-sitter-grammars.tree-sitter-typescript
    # pkgs.tree-sitter-grammars.tree-sitter-javascript

  ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [
    pkgs.libreoffice
    pkgs.chromium
    pkgs.go_1_20
    # pkgs.gopls
    # pkgs.gotools
    # pkgs.gotestsum
    pkgs.rustup
    # pkgs.rust-analyzer
    pkgs.clang
    pkgs.just
    pkgs.docker-compose
    pkgs.awscli2
    pkgs.postgresql_14
    pkgs.feh
    pkgs.xplr
    pkgs.kitty
    pkgs.font-awesome_5
    pkgs.powerline-fonts
    pkgs.powerline-symbols
    pkgs.cascadia-code
    # pkgs.fzf
    pkgs.openssl
    pkgs.lsof
    pkgs.gnupg
    pkgs.hunspell
    pkgs.croc
    pkgs.zigpkgs.master
    pkgs.bottom
    pkgs.kubernetes-helm
    pkgs.waypoint
    # pkgs.helix
    pkgs.unzip
    # pkgs.sublime-merge
    pkgs.lapce
    pkgs.terraform
    pkgs.enchant

    pkgs.lazygit
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
    pkgs.nyxt
    pkgs.ssm-session-manager-plugin
    # pkgs.atuin
    pkgs.bun
    pkgs.wezterm
    pkgs.zellij
  ];

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "${manpager}/bin/manpager";
  };

  # programs.go_1_20 = {
  #   enable = true;
  #   # package = pkgs.go_1_20;
  #   goPath = "go";
  #   goPrivate = [ "github.com/joincad" "github.com/jonnywalker81" ];
  # };

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
    # fuzzySearchFactor = 3;
    keyScheme = "vim";
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.git = {
    enable = true;
    userName = "Jonathan Rothberg";
    extraConfig = {
      pull.rebase = false;
      init.defaultBranch = "main";
      color.ui = true;
      core.askPass = ""; # needs to be empty to use terminal for ask pass
      # credential.helper = "store --file ~/.config/git-credentials";
      # credential.helper = "store";
      credential.helper = "cache --timeout 72000";
      push.default = "tracking";
      # branch.autosetuprebase = "always";
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
      rdd = "sudo darwin-rebuild switch --flake .#vm-darwin";
      h = "mcfly search -f ''";
      f = "history | fzf --sort --exact | sh";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -o";

      cd = "z";
      ys = "yarn install && yarn start";
    };

    # interactiveShellInit = lib.strings.concatStrings
    #   (lib.strings.intersperse "\n" [ (builtins.readFile ./config.zsh) ]);

    enableAutosuggestions = true;
    enableSyntaxHighlighting = true;
    sessionVariables = {
      LC_ALL = "en_US.utf8";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      GOPATH = "\${HOME}";
      GOPRIVATE = "github.com/JoinCAD,github.com/JonnyWalker81";
      # GOPROXY = "off";
      # PATH =
      PATH = "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin";
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";

      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
      TERM = "xterm-256color";
      # EDITOR = "emacsclient -t -a ''"; # $EDITOR use Emacs in terminal
      EDITOR = "nvim"; # $EDITOR use Emacs in terminal
      # VISUAL = "emacsclient -c -a emacs"; # $VISUAL use Emacs in GUI mode
      VISUAL = "$EDITOR"; # $VISUAL use Emacs in GUI mode
    };

    # eval "$(${mcflyBin} init zsh)"
    initExtra = ''
      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      eval "$(${zoxideBin} init zsh)"
      # eval "$(${mcflyBin} init zsh)"
      eval "$(ssh-agent -s)"
      # bindkey "^R" mcfly-history-widget
      source ~/.bash_join_db

      [[ ! -r /home/cipher/.opam/opam-init/init.zsh ]] || source /home/cipher/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

      # eval "$(atuin init zsh)"

      if [[ -z "$ZELLIJ" ]]; then
        if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
          zellij attach -c
        else
          zellij
        fi

        if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
          exit
        fi
    fi

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
      directory.fish_style_pwd_dir_length =
        1; # turn on fish directory truncation
      directory.truncation_length = 2; # number of directories not to truncate
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

      # SSH over AWS Systems Manager Session Manager
      Host i-* mi-*
        ProxyCommand sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"
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
