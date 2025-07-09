{
  config,
  lib,
  pkgs,
  system,
  inputs,
  ...
}:

# { config, lib, pkgs, theme, ... }:
let
  # hyprland = import "./hyprland.nix";
  # theme = config.colorScheme.pallete;

  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  homeDir = builtins.getEnv "HOME";

  isDarwin = pkgs.stdenv.isDarwin;
  manpager = (
    pkgs.writeShellScriptBin "manpager" (
      if isDarwin then
        ''
          sh -c 'col -bx | bat -l man -p'
        ''
      else
        ''
          cat "$1" | col -bx | bat --language man --style plain
        ''
    )
  );

  neovimNightly = inputs.neovim-nightly-overlay.packages.${system}.default;

in
{
  home.stateVersion = "25.05";
  xdg.enable = true;

  home.file.".doom.d" = {
    source = ./doom.d;
    recursive = true;
  };

  home.file.".elisp" = {
    source = ./elisp;
    recursive = true;
  };

  home.file.".config/zls.json" = {
    source = ./zls.json;
  };

  home.file.".config/kitty/kitty.conf" = {
    source = ./kitty/kitty.conf;
  };

  home.file.".wezterm.lua" = {
    source = ./wezterm/wezterm.lua;
  };

  # home.file.".config/rofi/config.rasi" = { source = ./rofi/config.rasi; };

  home.file.".config/ghostty/config" = {
    source = ./ghostty/config;
  };

  home.file.".config/rainfrog/rainfrog_config.toml" = {
    source = ./rainfrog/rainfrog_config.toml;
  };

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

  # home.file.".config/nvim" = {
  #   # source = ./lazyvim;
  #   # source = ./lazy;
  #   source = ./nvim;
  #   recursive = true;
  # };

  home.file.".config/dune/config" = {
    source = ./dune/config;
    recursive = true;
  };

  home.file.".config/nyxt/config.lisp" = {
    source = ./nyxt;
    recursive = true;
  };

  programs.emacs = {
    enable = true;
    package = pkgs.emacs;
    extraPackages = (
      epkgs: [
        epkgs.vterm
        epkgs.jinx
      ]
    );
  };

  home.packages =
    [
      pkgs.jetbrains-mono
      pkgs.ripgrep
      pkgs.fd
      pkgs.monaspace
      # pkgs.rustup
      # pkgs.rust-analyzer
      pkgs.thefuck
      pkgs.zoxide
      pkgs.bat
      pkgs.delta
      zoxide
      pkgs.jq
      pkgs.eza
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
      # pkgs.firefox-devedition-unwrapped
      pkgs.pgmanage
      pkgs.pgadmin4
      pkgs.pandoc
      pkgs.terraform-ls
      pkgs.tree-sitter
      pkgs.file
      pkgs.nil
      pkgs.nixpkgs-fmt
      pkgs.nixfmt-rfc-style
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
      (pkgs.python3.withPackages (
        p: with p; [
          epc
          orjson
          sexpdata
          six
          setuptools
          paramiko
          rapidfuzz
        ]
      ))
      # pkgs.python3
      # pkgs.python311Packages.epc
      # pkgs.python311Packages.orjson
      # pkgs.python311Packages.sexpdata
      # pkgs.python311Packages.six
      # pkgs.python311Packages.setuptools
      # pkgs.python311Packages.paramiko
      # pkgs.python311Packages.rapidfuzz

      pkgs.difftastic
      pkgs.nixd
      pkgs.luaformatter
      pkgs.lua-language-server
      pkgs.stylua
      pkgs.nodePackages.sql-formatter
      pkgs.nodePackages.typescript-language-server
      pkgs.sqls
      pkgs.yazi
      pkgs.qemu

      # inputs.zen-browser.packages."${system}".default
      # inputs.Neve.packages.${pkgs.system}.default
      # inputs.nvix.packages.${pkgs.system}.full
      # neovimNightly
      # pkgs.nixvim

      inputs.nixvim.packages.${pkgs.system}.default
      pkgs.claude-code
      pkgs.opencode
    ]
    ++ lib.optionals (!pkgs.stdenv.isDarwin) [
      pkgs.libreoffice
      pkgs.chromium
      # pkgs.gopls
      # pkgs.gotools
      # pkgs.gotestsum
      pkgs.rustup
      # pkgs.rust-analyzer
      pkgs.clang
      pkgs.just
      pkgs.docker-compose
      # pkgs.awscli2
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
      pkgs.hunspellDicts.en-us
      pkgs.croc
      # pkgs.zigpkgs.master
      pkgs.bottom
      pkgs.kubernetes-helm
      # pkgs.waypoint
      # pkgs.helix
      pkgs.unzip
      # pkgs.sublime-merge
      pkgs.lapce
      pkgs.terraform
      pkgs.enchant
      pkgs.gh

      # pkgs.lazygit
      # pkgs.diskonaut
      pkgs.sqlite
      pkgs.acpi
      pkgs.golangci-lint
      pkgs.kubernetes
      pkgs.pcmanfm
      pkgs.nemo
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
      # pkgs.ghostty
      pkgs.zellij
      pkgs.gtk3
      pkgs.teller
      pkgs.warp-terminal
      pkgs.neofetch
      pkgs.pandoc

      # Wayland/Hyprland tools
      pkgs.waybar
      pkgs.wl-clipboard
      pkgs.cliphist
      pkgs.swww
      pkgs.wofi
      pkgs.grim
      pkgs.slurp
      pkgs.swaylock
      pkgs.swayidle
      pkgs.swaynotificationcenter
    ];

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "${manpager}/bin/manpager";
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";
  };

  home.shellAliases = {
    code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";
    fix-clipboard = "/home/cipher/nixos-config/scripts/fix-parallels-clipboard.sh";
    prl-clip-fix = "/home/cipher/nixos-config/scripts/fix-parallels-clipboard.sh";
    
    # Display profile shortcuts
    dp = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
    dp-hidpi = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
    dp-retina = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
    dp-standard = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
    dp-present = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
    dp-ultrawide = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
    dp-auto = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
    dp-current = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
    dp-list = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
    prl-display = "parallels-display-info";
    
    # Firefox HiDPI fix
    firefox-hidpi = "GDK_SCALE=1 GDK_DPI_SCALE=1.8 firefox";
  };

  programs.firefox = {
    package = pkgs.firefox;
    enable = true;
    profiles = {
      default = {
        id = 0;
        name = "default";
        isDefault = true;
        settings = {
          # "browser.startup.homepage" = "https://searx.aicampground.com";
          # "browser.search.defaultenginename" = "Searx";
          # "browser.search.order.1" = "Searx";
        };
        search = {
          force = true;
          default = "google";
          order = [
            "google"
            "Searx"
          ];
          engines = {
            "Nix Packages" = {
              urls = [
                {
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
                }
              ];
              icon = "''${pkgs.nixos-icons}/share/icons/hicolor/scalable/apps/nix-snowflake.svg";
              definedAliases = [ "@np" ];
            };
            "NixOS Wiki" = {
              urls = [ { template = "https://nixos.wiki/index.php?search={searchTerms}"; } ];
              icon = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@nw" ];
            };
            "Searx" = {
              urls = [ { template = "https://searx.aicampground.com/?q={searchTerms}"; } ];
              icon = "https://nixos.wiki/favicon.png";
              updateInterval = 24 * 60 * 60 * 1000; # every day
              definedAliases = [ "@searx" ];
            };
            bing.metaData.hidden = true;
            "google".metaData.alias = "@g"; # builtin engines only support specifying one additional alias
          };
        };
        # extensions = with pkgs.nur.repos.rycee.firefox-addons; [
        # ublock-origin
        # bitwarden
        # darkreader
        # tridactyl
        # ];
      };
    };
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

  services.picom = {
    enable = true;
    #   # blur = true;

  };
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
      credential.helper = "cache --timeout 36000";
      push.default = "current";
      # branch.autosetuprebase = "always";
      # url."git@github.com".insteadOf = "https://github.com";
    };

    aliases = {
      bump = "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
    };

    difftastic = {
      enable = true;
    };

    delta = {
      enable = false;
      options = {
        syntax-theme = "1337";
        plus-color = "#32473d";
        minus-color = "#643632";
        features = "line-numbers";
        whitespace-error-style = "22 reverse";
      };
    };
  };

  # programs.atuin = {
  #   enable = true;
  #   settings = {
  #     # Uncomment this to use your instance
  #     # sync_address = "https://majiy00-shell.fly.dev";
  #     keymap_mode = "vim-normal";
  #   };
  # };

  programs.zsh = {
    enable = true;
    shellAliases = {
      ll = "eza -l";
      l = "eza -lah";
      rebuild = "sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild switch --flake .#vm-aarch64";
      rp = "sudo nixos-rebuild switch --flake .#vm-aarch64-prl";
      ri = "sudo nixos-rebuild switch --flake .#vm-intel";
      rdd = "sudo darwin-rebuild switch --flake .#vm-darwin";
      f = "history | fzf --sort --exact | sh";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      pbcopy = "xclip -selection clipboard";
      pbpaste = "xclip -o";

      ap = ''export AWS_PROFILE=$(aws configure list-profiles | fzf --prompt "Choose active AWS profile:")'';
      sw = ''terraform workspace list | fzf --prompt "Choose workspace:" | xargs -r terraform workspace select'';
      ka = ''ps -aux | fzf | awk '{print $2}' | xargs -r kill -9'';

      cd = "z";
      ys = "yarn install && yarn start";
      ff = ''cd "$(find $(git rev-parse --show-toplevel 2>/dev/null || pwd) -mindepth 1 -type d | fzf)"'';

      claude = "/home/cipher/.claude/local/claude";

      # Display profile management
      dp = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
      dp-hidpi = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
      dp-retina = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
      dp-standard = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
      dp-present = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
      dp-ultrawide = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
      dp-auto = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
      dp-current = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
      dp-list = "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
      prl-display = "parallels-display-info";
      
      # Firefox HiDPI fix
      firefox-hidpi = "GDK_SCALE=1 GDK_DPI_SCALE=1.8 firefox";
      
      # SSH agent management
      ssh-cleanup = "/home/cipher/nixos-config/scripts/cleanup-ssh-agents.sh";
    };

    # interactiveShellInit = lib.strings.concatStrings
    #   (lib.strings.intersperse "\n" [ (builtins.readFile ./config.zsh) ]);

    # autosuggestion = { enable = true; };
    # enableAutosuggestion = true;
    # enableAutosuggestions = true;
    autosuggestion = {
      enable = true;
    };
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    sessionVariables = {
      LC_ALL = "en_US.utf8";
      LIBVIRT_DEFAULT_URI = "qemu:///system";
      GOPATH = "\${HOME}";
      GOPRIVATE = "github.com/JoinCAD,github.com/JonnyWalker81";
      # GOPROXY = "off";
      # PATH =
      PATH = "\${HOME}/bin:\${HOME}/.cargo/bin:\${PATH}";
      PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";

      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
      TERM = "xterm-256color";
      # EDITOR = "emacsclient -t -a ''"; # $EDITOR use Emacs in terminal
      EDITOR = "nvim"; # $EDITOR use Emacs in terminal
      # VISUAL = "emacsclient -c -a emacs"; # $VISUAL use Emacs in GUI mode
      VISUAL = "$EDITOR"; # $VISUAL use Emacs in GUI mode
      AWS_PAGER = "";
    };

    initContent = ''
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
        eval "$(${zoxideBin} init zsh)"
        
        # SSH key management - only load keys if SSH agent is available and keys aren't already loaded
        if [[ -n "$SSH_AUTH_SOCK" ]] && command -v ssh-add >/dev/null 2>&1; then
          # Check if keys are already loaded
          if ! ssh-add -l >/dev/null 2>&1; then
            # Load SSH keys if they exist
            for key in ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_github; do
              if [[ -f "$key" ]]; then
                ssh-add "$key" >/dev/null 2>&1 || true
              fi
            done
          fi
        fi
        
        source ~/.bash_join_db

        [[ ! -r /home/cipher/.opam/opam-init/init.zsh ]] || source /home/cipher/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

        # export ATUIN_NOBIND="true"
        # eval "$(atuin init zsh)"

        source <(fzf --zsh)

        # # bindkey '^z' atuin-search
        # bindkey -M emacs '^r' atuin-search
        # bindkey -M viins '^r' atuin-search
        # bindkey -M vicmd '^r' atuin-search


        # bind to the up key, which depends on terminal mode
        # bindkey '^[[A' atuin-up-search
        # bindkey '^[OA' atuin-up-search

      #   if [[ -z "$ZELLIJ" ]]; then
      #     if [[ "$ZELLIJ_AUTO_ATTACH" == "true" ]]; then
      #       zellij attach -c
      #     else
      #       zellij
      #     fi
      #
      #     if [[ "$ZELLIJ_AUTO_EXIT" == "true" ]]; then
      #       exit
      #     fi
      # fi

    '';

    oh-my-zsh = {
      enable = true;
      plugins = [
        "git"
        "thefuck"
      ];
      theme = "robbyrussell";
    };

    history = {
      size = 100000;
      # path = "${config.xdg.dataHome}/zsh/history";
    };
  };

  # programs.neovim = {
  #   enable = true;
  #   # package = pkgs.neovim-nightly;
  #   # package = pkgs.unstable.neovim;
  #   package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
  #
  #   viAlias = true;
  #   vimAlias = true;
  # };
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
    nix-direnv = {
      enable = true;
    };
  };

  programs.starship = {
    enable = true;
    # Configuration written to ~/.config/starship.toml
    settings = {
      directory.fish_style_pwd_dir_length = 1; # turn on fish directory truncation
      directory.truncation_length = 2; # number of directories not to truncate
      add_newline = true;

      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };
    };
  };

  programs.home-manager.enable = true;

  # wayland.windowManager.sway = {
  #   enable = true;
  #
  #   config = rec {
  #     modifier = "Mod4";
  #     terminal = "alacritty";
  #     output = {
  #       "Virtual-1" = {
  #         mode = "1920x1080@60Hz";
  #       };
  #     };
  #   };
  # };

  # wayland.windowManager.hyprland.settings = {
  #   "$mod" = "ALT";
  #   bind =
  #     [
  #       "$mod, F, exec, firefox"
  #       "$mod SHIFT, Return, exec, alacritty"
  #       ", Print, exec, grimblast copy area"
  #     ]
  #     ++ (
  #       # workspaces
  #       # binds $mod + [shift +] {1..10} to [move to] workspace {1..10}
  #       builtins.concatLists (builtins.genList (
  #           x: let
  #             ws = let
  #               c = (x + 1) / 10;
  #             in
  #               builtins.toString (x + 1 - (c * 10));
  #           in [
  #             "$mod, ${ws}, workspace, ${toString (x + 1)}"
  #             "$mod SHIFT, ${ws}, movetoworkspace, ${toString (x + 1)}"
  #           ]
  #         )
  #         10)
  #     );
  # };

  programs.ssh = {
    enable = true;

    controlMaster = "auto";
    controlPath = "/tmp/ssh-%u-%r@%h:%p";
    controlPersist = "1800";

    forwardAgent = true;
    serverAliveInterval = 60;
    addKeysToAgent = "yes";

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

      selection = {
        save_to_clipboard = true;
      };

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

  # SSH Agent Service
  services.ssh-agent = {
    enable = true;
  };
}
