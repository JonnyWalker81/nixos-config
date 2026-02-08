{ isWSL, inputs, ... }:

{ config, lib, pkgs, ... }:
let
  common = import ../common.nix {
    inherit config lib pkgs isWSL inputs;
    system = pkgs.stdenv.hostPlatform.system;

  };
in {

  imports = [ common ../hyprland.nix ];

  programs.git.userEmail = "jon@join.build";

  home.file.".config/rofi/config.rasi".text = ''
    // Write your configuration

    // String interpolation to get the store path
    @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
  '';

  # picom config is now managed via services.picom in common.nix

  home.file.".xmonad/xmonad.hs" = { source = ../xmonad/xmonad.hs; };

  home.file.".config/xmobar/.xmobarrc" = { source = ../xmobar/.xmobarrc; };

  # DWM autostart and status bar scripts
  home.file.".local/share/dwm/autostart.sh" = {
    source = ../dwm/autostart.sh;
    executable = true;
  };

  home.file.".local/share/dwm/dwm-statusbar.sh" = {
    source = ../dwm/dwm-statusbar.sh;
    executable = true;
  };

  home.file.".config/waybar/config" = { source = ../waybar/config; };

  home.file.".config/waybar/style.css" = { source = ../waybar/style.css; };

  home.file.".config/wofi/config" = { source = ../wofi/config; };

  home.file.".config/wofi/style.css" = { source = ../wofi/style.css; };

  home.file.".config/awesome/rc.lua" = { source = ../awesome/rc.lua; };

  home.file.".config/awesome/theme.lua" = { source = ../awesome/theme.lua; };

  home.file."scripts/wttr.sh" = {
    source = ../scripts/wttr.sh;
    executable = true;
  };

  programs.go = {
    enable = true;
    package =
      inputs.nixpkgs-unstable.legacyPackages.${pkgs.stdenv.hostPlatform.system}.go;
  };

  # picom is now managed via services.picom in common.nix with proper transparency settings

  # Wallpaper setup service
  systemd.user.services.wallpaper-setup = {
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

  # Make cursor not tiny on HiDPI screens
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 32;
    # name = "Vanilla-DMZ";
    # package = pkgs.vanilla-dmz;
    # size = 64;
    x11.enable = true;
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

