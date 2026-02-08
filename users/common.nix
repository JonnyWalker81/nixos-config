{ config, lib, pkgs, system, inputs, ... }:

let
  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  homeDir = builtins.getEnv "HOME";

  isDarwin = pkgs.stdenv.isDarwin;
  manpager = (pkgs.writeShellScriptBin "manpager" (if isDarwin then ''
    sh -c 'col -bx | bat -l man -p'
  '' else ''
    cat "$1" | col -bx | bat --language man --style plain
  ''));

  neovimNightly = inputs.neovim-nightly-overlay.packages.${system}.default;

in {
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false; # Disable version mismatch warning
  xdg.enable = true;

  home.file.".doom.d" = {
    source = ./doom.d;
    recursive = true;
  };

  # Symlink .emacs.d to .config/emacs for Doom Emacs
  home.file.".emacs.d" = {
    source = config.lib.file.mkOutOfStoreSymlink
      "${config.home.homeDirectory}/.config/emacs";
  };

  home.file.".elisp" = {
    source = ./elisp;
    recursive = true;
  };

  home.file.".config/zls.json" = { source = ./zls.json; };

  home.file.".config/kitty/kitty.conf" = { source = ./kitty/kitty.conf; };

  home.file.".wezterm.lua" = { source = ./wezterm/wezterm.lua; };

  home.file.".config/ghostty/config" = {
    source = if pkgs.stdenv.isDarwin then
      ./ghostty/config-macos
    else
      ./ghostty/config-linux;
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
    package = if pkgs.stdenv.isDarwin then
      pkgs.emacs-unstable # Emacs 31.x with Cocoa GUI and native-comp support
    else
      pkgs.emacs;
    extraPackages = (epkgs: [ epkgs.vterm epkgs.jinx ]);
    extraConfig = ''
      ;; Set up SSH agent environment based on system type
      (cond
       ;; macOS: Find the launchd SSH agent socket dynamically
       ((eq system-type 'darwin)
        (let ((sock (shell-command-to-string
                     "ls /private/tmp/com.apple.launchd.*/Listeners 2>/dev/null | head -1 | tr -d '\n'")))
          (when (and sock (not (string-empty-p sock)) (file-exists-p sock))
            (setenv "SSH_AUTH_SOCK" sock))))
       ;; Linux: Use systemd user environment or fallback to hardcoded path
       ((eq system-type 'gnu/linux)
        (if (executable-find "systemctl")
            (let ((ssh-auth-sock (shell-command-to-string "systemctl --user show-environment | grep SSH_AUTH_SOCK | cut -d'=' -f2-")))
              (when (and ssh-auth-sock (not (string-empty-p (string-trim ssh-auth-sock))))
                (setenv "SSH_AUTH_SOCK" (string-trim ssh-auth-sock))))
          (setenv "SSH_AUTH_SOCK" "/run/user/1000/ssh-agent"))))

    '';
  };

  home.packages = [
    # Programming fonts
    pkgs.cascadia-code
    pkgs.jetbrains-mono
    pkgs.victor-mono
    pkgs.input-fonts
    pkgs.iosevka
    pkgs.ripgrep
    pkgs.fd
    pkgs.monaspace
    pkgs.zoxide
    pkgs.bat
    pkgs.delta
    zoxide
    pkgs.jq
    pkgs.eza
    pkgs.fzf
    pkgs.maim # Screenshot tool
    pkgs.xclip # Clipboard support for screenshots
    pkgs.k9s
    pkgs.procs
    pkgs.graphviz
    pkgs.fira-code
    pkgs.fira-code-symbols
    pkgs.llvm
    pkgs.bind
    pkgs.pandoc
    pkgs.terraform-ls
    pkgs.tree-sitter
    pkgs.file
    pkgs.nil
    pkgs.nixpkgs-fmt
    pkgs.nixfmt
    pkgs.shfmt
    (pkgs.python3.withPackages
      (p: with p; [ epc orjson sexpdata six setuptools paramiko rapidfuzz ]))

    pkgs.difftastic
    pkgs.nixd
    pkgs.luaformatter
    pkgs.lua-language-server
    pkgs.stylua
    pkgs.nodePackages.sql-formatter
    pkgs.nodePackages.typescript-language-server
    pkgs.sqls
    pkgs.yazi

    pkgs.claude-code
    pkgs.opencode
  ] ++ lib.optionals (!pkgs.stdenv.isDarwin) [
    # Packages with wayland dependencies (Linux only)
    pkgs.qemu
    pkgs.pgmanage
    pkgs.pgadmin4
    inputs.nixvim.packages.${pkgs.system}.default # Has wayland clipboard dependencies
    pkgs.libreoffice
    pkgs.chromium
    pkgs.rustup
    pkgs.clang
    pkgs.just
    pkgs.docker-compose
    pkgs.postgresql_14
    pkgs.feh
    pkgs.xplr
    pkgs.kitty
    pkgs.font-awesome_5
    pkgs.powerline-fonts
    pkgs.powerline-symbols
    pkgs.openssl
    pkgs.lsof
    pkgs.gnupg
    pkgs.pinentry
    pkgs.hunspell
    pkgs.hunspellDicts.en-us
    pkgs.croc
    pkgs.bottom
    pkgs.kubernetes-helm
    pkgs.unzip
    pkgs.lapce
    pkgs.terraform
    pkgs.enchant
    pkgs.gh

    pkgs.sqlite
    pkgs.acpi
    pkgs.golangci-lint
    pkgs.kubernetes
    pkgs.pcmanfm
    pkgs.nemo
    pkgs.rofi
    pkgs.vscode
    pkgs.haskellPackages.libmpd
    pkgs.haskellPackages.xmobar
    pkgs.nyxt
    pkgs.ssm-session-manager-plugin
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
    pkgs.wl-clipboard # Essential for Wayland clipboard
    pkgs.xclip # Essential for X11 clipboard compatibility
    pkgs.swww
    pkgs.wofi
    pkgs.cliphist # Clipboard history manager
    pkgs.grim
    pkgs.slurp
    pkgs.swaylock
    pkgs.swayidle
    pkgs.swaynotificationcenter
  ];

  home.sessionPath =
    [ "$HOME/.claude/local" "$HOME/.local/bin" "$HOME/bin" "$HOME/.cargo/bin" ];

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    VISUAL = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "${manpager}/bin/manpager";

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
  } // lib.optionalAttrs (!isDarwin) {
    # Linux-only: Set SSH_AUTH_SOCK for systemd ssh-agent
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";
  };

  home.shellAliases = {
    code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";

    # Display profile shortcuts
    dp =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
    dp-hidpi =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
    dp-retina =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
    dp-standard =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
    dp-present =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
    dp-ultrawide =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
    dp-auto =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
    dp-current =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
    dp-list =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
    prl-display = "parallels-display-info";

    # SSH key management
    ssh-keys = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh";
    ssh-add-keys = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh add";
    ssh-status = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh status";
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
      core = {
        askPass = ""; # needs to be empty to use terminal for ask pass
        fsmonitor = true; # enables built-in fsmonitor daemon
        untrackedCache = true; # speeds up scanning untracked files
      };

      credential.helper = "cache --timeout 36000";
      push.default = "current";
    };

    aliases = {
      bump =
        "!git checkout $1; git pull origin $1; git rebase \${2:-'main'}; git push origin; git checkout \${2:-'main'}";
    };

    difftastic = { enable = true; };

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

  programs.zsh = {
    enable = true;
    shellAliases = {
      ll = "eza -l";
      l = "eza -lah";
      rebuild =
        "sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild switch --flake .#vm-aarch64";
      rp = "sudo nixos-rebuild switch --flake .#vm-aarch64-prl";
      ri = "sudo nixos-rebuild switch --flake .#vm-intel";
      rdd = "sudo darwin-rebuild switch --flake .#vm-darwin";
      f = "history | fzf --sort --exact | sh";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      ap = ''
        export AWS_PROFILE=$(aws configure list-profiles | fzf --prompt "Choose active AWS profile:")'';
      sw = ''
        terraform workspace list | fzf --prompt "Choose workspace:" | xargs -r terraform workspace select'';
      ka = "ps -aux | fzf | awk '{print $2}' | xargs -r kill -9";

      cd = "z";
      ys = "yarn install && yarn start";
      ff = ''
        cd "$(find $(git rev-parse --show-toplevel 2>/dev/null || pwd) -mindepth 1 -type d | fzf)"'';

      # Display profile management
      dp =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
      dp-hidpi =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
      dp-retina =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
      dp-standard =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
      dp-present =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
      dp-ultrawide =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
      dp-auto =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
      dp-current =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
      dp-list =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
      prl-display = "parallels-display-info";

      # SSH agent management
      ssh-cleanup = "/home/cipher/nixos-config/scripts/cleanup-ssh-agents.sh";

      # CodeRabbit CLI
      cr = "coderabbit";
    };

    autosuggestion = { enable = true; };
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

      # Ensure ~/.local/bin is in PATH for Linux (in case session vars aren't loaded properly)
      if [[ "$(uname)" == "Linux" ]]; then
        export PATH="$HOME/.local/bin:$PATH"
      fi

      # Import SSH environment from systemd user environment (Linux only)
      if [[ "$(uname)" == "Linux" ]] && command -v systemctl >/dev/null 2>&1; then
        export SSH_AUTH_SOCK="/run/user/1000/ssh-agent"
      fi

      # SSH key management - only load keys if SSH agent is available and keys aren't already loaded
      if [[ -n "$SSH_AUTH_SOCK" ]] && command -v ssh-add >/dev/null 2>&1; then
        # Check if keys are already loaded
        if ! ssh-add -l >/dev/null 2>&1; then
          # Load SSH keys if they exist
          if [[ "$(uname)" == "Darwin" ]]; then
            # On macOS, use --apple-use-keychain to store passphrase in keychain
            for key in ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_github; do
              if [[ -f "$key" ]]; then
                ssh-add --apple-use-keychain "$key" >/dev/null 2>&1 || true
              fi
            done
          else
            # On Linux, just add the keys normally
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

      source <(fzf --zsh)

      # Show system info on new terminal
      neofetch

    '';

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "robbyrussell";
    };

    history = { size = 100000; };
  };

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
    addKeysToAgent = "yes";

    hashKnownHosts = true;
    userKnownHostsFile = "~/.ssh/known_hosts";

    extraConfig = ''
      HostkeyAlgorithms +ssh-rsa

      # Prevent SSH from trying all available keys for every connection
      IdentitiesOnly yes

      # Increase timeout for SSH agent to reduce password prompts
      PasswordAuthentication no
      PubkeyAuthentication yes

      # SSH over AWS Systems Manager Session Manager
      Host i-* mi-*
        ProxyCommand sh -c "aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'"

      # Wildcard pattern for hosts that may use Jenkins buildfarm key
      Host *.jenkins.* *.buildfarm.* jenkins-* buildfarm-*
        IdentitiesOnly no
        AddKeysToAgent yes
        PasswordAuthentication no
        PubkeyAuthentication yes
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
    };
  };

  # SSH Agent Service (Linux only - macOS uses native SSH agent)
  services.ssh-agent = lib.mkIf (!pkgs.stdenv.isDarwin) { enable = true; };

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
      ExecStart =
        "${pkgs.systemd}/bin/systemctl --user set-environment SSH_AUTH_SOCK=/run/user/1000/ssh-agent";
    };
    Install = { WantedBy = [ "default.target" ]; };
  };
}
