{ isWSL, inputs, ... }:

{ config, lib, pkgs, ... }:
let
  common = import ../common.nix {
    inherit config lib pkgs isWSL inputs;
    system = pkgs.stdenv.hostPlatform.system;
  };
in {
  imports = [ common ];

  programs.google-chrome-dev = { enable = true; };

  programs.git.userEmail = "jrothberg@bluebeam.com";

  home.file.".config/rofi/config.rasi".text = ''
    // Write your configuration

    // String interpolation to get the store path
    @theme "${pkgs.rofi-unwrapped}/share/rofi/themes/glue_pro_blue.rasi"
  '';

  home.file.".config/greenclip.toml" = {
    source = ../greenclip/greenclip.toml;
  };

  home.file.".config/clipcat/clipcatd.toml" = {
    source = ../clipcat/clipcatd.toml;
  };

  home.file.".config/clipcat/clipcatctl.toml" = {
    source = ../clipcat/clipcatctl.toml;
  };

  home.file.".config/clipcat/clipcat-menu.toml" = {
    source = ../clipcat/clipcat-menu.toml;
  };

  # home.file.".config/picom/picom.conf" = { source = ../picom/picom.conf; };

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

  # Make cursor not tiny on HiDPI screens
  home.pointerCursor = {
    name = "Adwaita";
    package = pkgs.gnome.adwaita-icon-theme;
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
  #   User jrothberg@bluebeam.com
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
  # home.file.".msmtprc" = {
  #   text = ''
  #     defaults
  #     auth on
  #     tls on
  #     tls_trust_file /etc/ssl/certs/ca-certificates.crt
  #     logfile ~/.msmtp.log
  #
  #     account gmail
  #     host smtp.gmail.com
  #     port 587
  #     from jrothberg@bluebeam.com
  #     user jrothberg@bluebeam.com
  #     passwordeval "pass email/gmail/app-password"
  #
  #     account default : gmail
  #   '';
  #   mode = "0600"; # Secure permissions
  # };

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
