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
  home.enableNixpkgsReleaseCheck = false; # Disable version mismatch warning
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
    package =
      if pkgs.stdenv.isDarwin then
        pkgs.emacs-unstable # Emacs 31.x with Cocoa GUI and native-comp support
      else
        pkgs.emacs;
    extraPackages = (
      epkgs: [
        epkgs.vterm
        epkgs.jinx
      ]
    );
    extraConfig = ''
      ;; Ensure SSH agent environment is available
      (setenv "SSH_AUTH_SOCK" "/run/user/1000/ssh-agent")

      ;; Copy SSH environment variables from systemd user environment
      (when (and (eq system-type 'linux)
                 (executable-find "systemctl"))
        (let ((ssh-auth-sock (shell-command-to-string "systemctl --user show-environment | grep SSH_AUTH_SOCK | cut -d'=' -f2-")))
          (when (and ssh-auth-sock (not (string-empty-p (string-trim ssh-auth-sock))))
            (setenv "SSH_AUTH_SOCK" (string-trim ssh-auth-sock)))))

      ;; Fix clipboard for Wayland
      (when (and (eq system-type 'linux)
                 (getenv "WAYLAND_DISPLAY"))
        ;; Use wl-copy and wl-paste for clipboard operations
        (setq wl-copy-process nil)
        (defun wl-copy (text)
          (setq wl-copy-process (make-process :name "wl-copy"
                                              :buffer nil
                                              :command '("wl-copy" "-f" "-n")
                                              :connection-type 'pipe))
          (process-send-string wl-copy-process text)
          (process-send-eof wl-copy-process))
        (defun wl-paste ()
          (if (and wl-copy-process (process-live-p wl-copy-process))
              nil ; should return nil if we're the current paste owner
            (shell-command-to-string "wl-paste -n | tr -d \r")))
        
        ;; Advise Emacs to use Wayland clipboard
        (setq interprogram-cut-function 'wl-copy)
        (setq interprogram-paste-function 'wl-paste)
        
        ;; Also set selection functions
        (setq select-enable-clipboard t)
        (setq select-enable-primary t))
    '';
  };

  home.packages =
    [
      pkgs.jetbrains-mono
      pkgs.ripgrep
      pkgs.fd
      pkgs.monaspace
      # pkgs.rustup
      # pkgs.rust-analyzer
      # pkgs.thefuck # Removed - incompatible with Python 3.12+, consider pay-respects
      pkgs.zoxide
      pkgs.bat
      pkgs.delta
      zoxide
      pkgs.jq
      pkgs.eza
      pkgs.fzf
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

      inputs.nixvim.packages.${pkgs.system}.default # Re-enabled after update
      # pkgs.neovim  # Using default neovim temporarily
      # pkgs.claude-code
      pkgs.opencode
      
      # DankMono font - available on both Linux and macOS
      pkgs.dankmono
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
      pkgs.ghostty # Now using nixpkgs version which supports macOS
      pkgs.zellij
      pkgs.gtk3
      pkgs.teller
      pkgs.warp-terminal
      pkgs.neofetch
      pkgs.pandoc

      # Wayland/Hyprland tools (Linux only)
      pkgs.waybar
      pkgs.wl-clipboard
      pkgs.xclip  # Also include xclip for X11 compatibility
      pkgs.cliphist
      pkgs.swww
      pkgs.wofi
      pkgs.grim
      pkgs.slurp
      pkgs.swaylock
      pkgs.swayidle
      pkgs.swaynotificationcenter
    ];

  home.sessionPath = [
    "$HOME/.claude/local"
    "$HOME/bin"
    "$HOME/.cargo/bin"
  ];

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    VISUAL = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "${manpager}/bin/manpager";
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";

    # Firefox/Mozilla HiDPI scaling
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    MOZ_DBUS_REMOTE = "1";
    
    # Development
    GOPATH = "\${HOME}";
    GOPRIVATE = "github.com/JoinCAD,github.com/JonnyWalker81";
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    LIBVIRT_DEFAULT_URI = "qemu:///system";
    AWS_PAGER = "";
    TERM = "xterm-256color";
  };

  home.shellAliases = {
    code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";
    fix-clipboard = "fix-parallels-clipboard";
    fix-clipboard-old = "/home/cipher/nixos-config/scripts/fix-parallels-clipboard.sh";
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
  };

  programs.firefox = lib.mkIf (!pkgs.stdenv.isDarwin) {
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

          # HiDPI/4K display scaling settings
          "layout.css.devPixelsPerPx" = "1.25";
          "browser.display.use_system_colors" = false;
          "browser.display.use_document_fonts" = 1;
          "font.size.variable.x-western" = 18;
          "font.size.fixed.x-western" = 14;
          "font.minimum-size.x-western" = 14;

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

  services.picom = lib.mkIf (!pkgs.stdenv.isDarwin) {
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
    shellAliases =
      {
        ll = "eza -l";
        l = "eza -lah";
        rebuild = "sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild switch --flake .#vm-aarch64";
        rp = "sudo nixos-rebuild switch --flake .#vm-aarch64-prl";
        ri = "sudo nixos-rebuild switch --flake .#vm-intel";
        rdd = "sudo darwin-rebuild switch --flake .#vm-darwin";
        f = "history | fzf --sort --exact | sh";
        bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

        ap = ''export AWS_PROFILE=$(aws configure list-profiles | fzf --prompt "Choose active AWS profile:")'';
        sw = ''terraform workspace list | fzf --prompt "Choose workspace:" | xargs -r terraform workspace select'';
        ka = ''ps -aux | fzf | awk '{print $2}' | xargs -r kill -9'';

        cd = "z";
        ys = "yarn install && yarn start";
        ff = ''cd "$(find $(git rev-parse --show-toplevel 2>/dev/null || pwd) -mindepth 1 -type d | fzf)"'';

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

        # SSH agent management
        ssh-cleanup = "/home/cipher/nixos-config/scripts/cleanup-ssh-agents.sh";
      }
      // lib.optionalAttrs (!pkgs.stdenv.isDarwin) {
        # Linux-specific aliases for clipboard
        # Use wl-copy/wl-paste for Wayland, with xclip fallback
        pbcopy = "wl-copy";
        pbpaste = "wl-paste";
        # X11 fallback aliases
        xpbcopy = "xclip -selection clipboard";
        xpbpaste = "xclip -o";
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
      # Only ZSH-specific variables here, general ones are in home.sessionVariables
      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
    };

    initContent = ''
        # Ensure claude is in PATH
        export PATH="$HOME/.claude/local:$PATH"
        
        source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
        eval "$(${zoxideBin} init zsh)"
        
        # Prepend home-manager bin to PATH for Darwin
        if [[ "$(uname)" == "Darwin" ]]; then
          export PATH="$HOME/.local/state/nix/profiles/home-manager/home-path/bin:$PATH"
          
          # macOS SSH agent setup - use the system SSH agent
          # The SSH agent on macOS is managed by launchd and the socket is dynamic
          # No need to set SSH_AUTH_SOCK as macOS handles it automatically
        fi
        
        # Import SSH environment from systemd user environment (Linux only)
        if [[ "$(uname)" == "Linux" ]] && command -v systemctl >/dev/null 2>&1; then
          export SSH_AUTH_SOCK="/run/user/1000/ssh-agent"
        fi
        
        # SSH key management - ensure SSH agent is available on macOS
        if [[ "$(uname)" == "Darwin" ]]; then
          # On macOS, the SSH agent is managed by launchd
          # Ensure we have a valid SSH_AUTH_SOCK
          if [[ -z "$SSH_AUTH_SOCK" ]] || [[ ! -S "$SSH_AUTH_SOCK" ]]; then
            # Try to get the SSH agent socket from launchctl
            export SSH_AUTH_SOCK=$(launchctl getenv SSH_AUTH_SOCK 2>/dev/null)
          fi
          
          # If we still don't have a valid socket, use the default macOS SSH agent
          if [[ -z "$SSH_AUTH_SOCK" ]] || [[ ! -S "$SSH_AUTH_SOCK" ]]; then
            export SSH_AUTH_SOCK=/private/tmp/com.apple.launchd.*/Listeners
          fi
          
          # Load SSH keys from Keychain if not already loaded
          if command -v ssh-add >/dev/null 2>&1; then
            # Check if any keys are loaded
            if ! ssh-add -l >/dev/null 2>&1; then
              # Load all SSH keys from keychain
              ssh-add --apple-load-keychain >/dev/null 2>&1 || true
            fi
          fi
        else
          # Linux SSH key management
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
        # "thefuck" # Removed - incompatible with Python 3.12+
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

  # SSH Agent Service (Linux only - macOS uses native SSH agent)
  services.ssh-agent = lib.mkIf (!pkgs.stdenv.isDarwin) {
    enable = true;
  };

  # Systemd service to set SSH_AUTH_SOCK for all user services (Linux only)
  systemd.user.services.ssh-agent-env = lib.mkIf (!pkgs.stdenv.isDarwin) {
    Unit = {
      Description = "Set SSH_AUTH_SOCK environment variable for user services";
      After = [ "ssh-agent.service" ];
      Wants = [ "ssh-agent.service" ];
    };
    Service = {
      Type = "oneshot";
      RemainAfterExit = true;
      ExecStart = "${pkgs.systemd}/bin/systemctl --user set-environment SSH_AUTH_SOCK=/run/user/1000/ssh-agent";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
}
