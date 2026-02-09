{ isWSL, isDarwin, inputs, ... }:

{ config, lib, pkgs, ... }:
let
  isLinux = !isDarwin;

  common = import ../common {
    inherit config lib pkgs isWSL inputs;
    system = pkgs.stdenv.hostPlatform.system;

  };
in {

  imports = [ common ] ++ lib.optionals isLinux [ ../hyprland.nix ];

  # Disable the broken application/font linking on Darwin (home-manager bug with buildEnv)
  disabledModules = lib.optionals isDarwin [
    "targets/darwin/linkapps.nix"
    "targets/darwin/fonts.nix"
  ];

  programs.git.userEmail = "jon@join.build";

  # --- Linux-only configuration ---

  home.file = lib.mkIf isLinux {
    ".config/rofi/config.rasi".text = ''
      // Write your configuration

      // String interpolation to get the store path
      @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
    '';

    # picom config is now managed via services.picom in common.nix

    ".xmonad/xmonad.hs" = { source = ../xmonad/xmonad.hs; };

    ".config/xmobar/.xmobarrc" = { source = ../xmobar/.xmobarrc; };

    # DWM autostart and status bar scripts
    ".local/share/dwm/autostart.sh" = {
      source = ../dwm/autostart.sh;
      executable = true;
    };

    ".local/share/dwm/dwm-statusbar.sh" = {
      source = ../dwm/dwm-statusbar.sh;
      executable = true;
    };

    ".config/waybar/config" = { source = ../waybar/config; };

    ".config/waybar/style.css" = { source = ../waybar/style.css; };

    ".config/wofi/config" = { source = ../wofi/config; };

    ".config/wofi/style.css" = { source = ../wofi/style.css; };

    ".config/awesome/rc.lua" = { source = ../awesome/rc.lua; };

    ".config/awesome/theme.lua" = { source = ../awesome/theme.lua; };

    "scripts/wttr.sh" = {
      source = ../scripts/wttr.sh;
      executable = true;
    };
  };

  programs.go = lib.mkIf isLinux {
    enable = true;
    package =
      inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.go;
  };

  # picom is now managed via services.picom in common.nix with proper transparency settings

  # Wallpaper setup service (Linux only)
  systemd.user.services = lib.mkMerge [
    (lib.mkIf isLinux {
      wallpaper-setup = {
        Unit = {
          Description = "Setup desktop wallpaper";
          After = [ "graphical-session-pre.target" ];
          PartOf = [ "graphical-session.target" ];
        };
        Service = {
          Type = "oneshot";
          ExecStart =
            "${pkgs.bash}/bin/bash /home/cipher/nixos-config/scripts/setup-wallpaper.sh";
          RemainAfterExit = true;
        };
        Install = { WantedBy = [ "graphical-session.target" ]; };
      };
    })
    # macOS-specific services (disable Linux systemd services)
    (lib.mkIf isDarwin (lib.mkForce { }))
  ];

  # Make cursor not tiny on HiDPI screens (Linux only)
  home.pointerCursor = lib.mkIf isLinux {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 32;
    # name = "Vanilla-DMZ";
    # package = pkgs.vanilla-dmz;
    # size = 64;
    x11.enable = true;
  };

  # --- Darwin-only configuration ---

  # macOS-specific shell aliases
  home.shellAliases = lib.mkIf isDarwin {
    # Darwin rebuild shortcuts
    dm = "sudo darwin-rebuild switch --flake ~/nixos-config#macbook-cipher";
    dh = "home-manager switch --flake ~/nixos-config#macbook-cipher";

    # macOS-specific aliases
    brewup = "brew update && brew upgrade";
    finder = "open -a Finder";

    # Override Linux-specific aliases for macOS
    pbcopy = lib.mkForce "pbcopy"; # Use native macOS pbcopy
    pbpaste = lib.mkForce "pbpaste"; # Use native macOS pbpaste
  };

  # macOS-specific packages (in addition to common.nix)
  home.packages = lib.mkIf isDarwin (with pkgs; [
    # macOS-specific development tools
    rectangle # Window management

    # Neovim with nixvim configuration
    pkgs.nixvim

    # Native macOS alternatives to Linux tools
    # (common.nix handles the platform-agnostic tools)
  ]);

  # Override/disable Linux-specific configurations (only apply on Linux to avoid module evaluation on Darwin)
  programs.waybar = lib.mkIf (!isDarwin) (lib.mkForce { enable = false; });

  # Disable all Wayland/Hyprland configurations (only on Linux to avoid wayland package evaluation on Darwin)
  wayland.windowManager.hyprland =
    lib.mkIf (!isDarwin) { enable = lib.mkForce false; };

  # macOS-specific session variables
  home.sessionVariables = lib.mkIf isDarwin {
    # Override Linux-specific variables
    BROWSER = "open";

    # Remove the Linux SSH agent path override - let macOS handle it
    # SSH_AUTH_SOCK is managed by macOS launchd
  };

  # Ensure home-manager bin is in PATH before homebrew (Darwin)
  home.sessionPath = lib.mkIf isDarwin
    [ "$HOME/.local/state/nix/profiles/home-manager/home-path/bin" ];

  # macOS Terminal and shell optimization
  programs.alacritty.settings = lib.mkIf isDarwin {
    window = {
      decorations = "buttonless";
      option_as_alt = "Both";
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
      {
        key = "Q";
        mods = "Command";
        action = "Quit";
      }
      {
        key = "N";
        mods = "Command";
        action = "SpawnNewInstance";
      }
    ];
  };

  # Configure git for macOS
  programs.git.extraConfig = lib.mkIf isDarwin {
    # macOS-specific git settings
    credential.helper = lib.mkForce "osxkeychain";
  };

  # SSH configuration for macOS
  programs.ssh.extraConfig = lib.mkIf isDarwin ''
    # macOS-specific SSH settings
    UseKeychain yes
    AddKeysToAgent yes

    # Store SSH keys in macOS Keychain
    IdentityFile ~/.ssh/id_ed25519
    IdentityFile ~/.ssh/id_rsa
  '';

  # Ensure direnv works properly on macOS
  programs.direnv = lib.mkIf isDarwin {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  # Email configuration - DISABLED to prevent GPG password prompts
  # # Email configuration - mbsync for Gmail IMAP
  # home.file.".mbsyncrc".text = ''
  #   # Gmail IMAP configuration
  #   IMAPAccount gmail
  #   Host imap.gmail.com
  #   User jon@join.build
  #   PassCmd "pass email/gmail/app-password"
  #   SSLType IMAPS
  #   CertificateFile /etc/ssl/certs/ca-certificates.crt
  #
  #   IMAPStore gmail-remote
  #   Account gmail
  #
  #   MaildirStore gmail-local
  #   Path ~/mail/
  #   Inbox ~/mail/Inbox
  #   SubFolders Verbatim
  #
  #   Channel gmail
  #   Far :gmail-remote:
  #   Near :gmail-local:
  #   Patterns * ![Gmail]* "[Gmail]/Sent Mail" "[Gmail]/Starred" "[Gmail]/All Mail" "[Gmail]/Drafts" "[Gmail]/Trash"
  #   Create Both
  #   Expunge Both
  #   SyncState *
  # '';

  # # Email configuration - msmtp for SMTP
  # home.file.".msmtprc".text = ''
  #   defaults
  #   auth on
  #   tls on
  #   tls_trust_file /etc/ssl/certs/ca-certificates.crt
  #   logfile ~/.msmtp.log
  #
  #   account gmail
  #   host smtp.gmail.com
  #   port 587
  #   from jon@join.build
  #   user jon@join.build
  #   passwordeval "pass email/gmail/app-password"
  #
  #   account default : gmail
  # '';
  #
  # # Set permissions for msmtprc (msmtp requires 0600)
  # home.activation.msmtpPermissions = lib.hm.dag.entryAfter ["writeBoundary"] ''
  #   chmod 0600 ~/.msmtprc || true
  # '';

  # # Systemd service for mbsync
  # systemd.user.services.mbsync = {
  #   Unit = {
  #     Description = "mbsync synchronization";
  #     After = [ "network-online.target" ];
  #   };
  #   Service = {
  #     Type = "oneshot";
  #     ExecStart = "${pkgs.isync}/bin/mbsync -a";
  #     StandardOutput = "journal";
  #     StandardError = "journal";
  #   };
  # };

  # # Systemd timer for mbsync
  # systemd.user.timers.mbsync = {
  #   Unit = {
  #     Description = "mbsync timer";
  #   };
  #   Timer = {
  #     OnBootSec = "2m";
  #     OnUnitActiveSec = "5m";
  #     Unit = "mbsync.service";
  #   };
  #   Install = {
  #     WantedBy = [ "timers.target" ];
  #   };
  # };
}
