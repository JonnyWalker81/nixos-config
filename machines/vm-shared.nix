{
  config,
  pkgs,
  lib,
  currentSystem,
  currentSystemName,
  currentSystemUser,
  inputs,
  ...
}:

{
  imports = [ ../modules/desktop ];
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  services.journald.extraConfig = "SystemMaxUse=100M";
  nix = {
    package = pkgs.nixVersions.latest;

    settings = {
      # Moved here from nix.extraOptions: nix.settings is type-checked and merges
      # with other modules, whereas extraOptions is appended to nix.conf verbatim.
      experimental-features = [ "nix-command" "flakes" ];

      # Nix's default is false; this was previously turned on. With direnv
      # gcroots present, keep-outputs revives build-only closures (compilers,
      # rustc-bootstrap, fetched source tarballs) on top of every pinned dev
      # shell -- measured at 3.83 GiB on a single shell, and the reason 51,575
      # of 68,857 store entries were .drv files.
      keep-outputs = false;

      # keep-derivations is deliberately absent: `true` is already Nix's default,
      # so the old line was a no-op. It is the cheap half (~18 MB per shell) and
      # must NOT be inverted -- keep-derivations=false + keep-outputs=true is
      # exactly backwards.

      # Deduplicate (hardlink) paths as they enter the store. Currently saving
      # ~36.9 GiB. Reassess when nix moves past 2.31.3.
      auto-optimise-store = true;

      # Binary caches for faster builds
      substituters = [ "https://cache.nixos.org" ];
      trusted-public-keys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=" ];

      # Emergency valve for the real failure mode: when free space in the store
      # drops below min-free DURING A BUILD, Nix collects until max-free is
      # available. Build-time only -- complements the timer, does not replace it.
      min-free = 20 * 1024 * 1024 * 1024; # 20 GiB
      max-free = 60 * 1024 * 1024 * 1024; # 60 GiB
    };

    # Daily GC of generations older than 14 days. The timer already carries
    # Persistent=true, so triggers missed while the VM was suspended catch up.
    # Do NOT add --max-freed: removeOldGenerations() runs unconditionally before
    # collectGarbage(), so --max-freed can only make this free less.
    gc = {
      automatic = true;
      dates = "daily";
      randomizedDelaySec = "45min";
      options = "--delete-older-than 14d";
    };
  };

  # nix-collect-garbage has no age heuristic for indirect gcroots, and Nix only
  # auto-prunes dangling links under gcroots/auto -- never gcroots/per-user.
  # Measured: 633 store paths / 8.40 GiB pinned EXCLUSIVELY by direnv roots
  # (~16 GiB once the processes holding the shared half exit), across 16 .direnv
  # dirs untouched for over a year, plus 32 permanently-dangling legacy roots.
  # Unpin BEFORE collecting -- the ordering is load-bearing.
  systemd.services.nix-direnv-prune = {
    description = "Unpin dormant direnv dev-shell gcroots before nix-gc";
    before = [ "nix-gc.service" ];
    wantedBy = [ "nix-gc.service" ]; # Wants, not Requires: never block the GC
    serviceConfig.Type = "oneshot";
    path = with pkgs; [ coreutils findutils ];
    script = ''
      # Scan the whole home, not just Repositories -- dormant layout dirs turn
      # up under ~/scratch too. Do NOT add -L to find: `-L ... -xtype l` matches
      # LIVE symlinks and would defeat the -mtime gate entirely.
      home=${lib.escapeShellArg "/home/${currentSystemUser}"}
      if [ -d "$home" ]; then
        find "$home" -xdev -maxdepth 6 -type d -name .direnv -mtime +365 \
          -prune -print -exec rm -rf {} + || true
      fi
      # Every root under gcroots/per-user points into a .direnv path, so this
      # cannot collateral-damage a profile or channel root.
      find /nix/var/nix/gcroots/per-user -maxdepth 2 -xtype l -print -delete || true
    '';
  };

  # A systemd *user* timer (the home-manager nix.gc in users/common/services.nix)
  # only fires while the user has a running systemd instance. Linger was already
  # on at runtime but set imperatively; declaring it makes that reproducible.
  users.users.${currentSystemUser}.linger = true;

  # We expect to run the VM on hidpi machines.
  hardware.graphics = {
    enable = true;

    # For VM environments, ensure software rendering fallback
    extraPackages = with pkgs; [
      mesa
      libvdpau-va-gl
      vaapiVdpau
    ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.systemd-boot.configurationLimit = 20;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
  nixpkgs.config.input-fonts.acceptLicense = true;

  boot.loader.systemd-boot.consoleMode = "0";

  # Define your hostname.
  networking.hostName = "cipher";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  networking.timeServers = [
    "pool.ntp.org"
    "time.nist.gov"
  ];
  services.timesyncd.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  # Don't require password for sudo
  security.sudo.wheelNeedsPassword = false;

  # Virtualization settings
  virtualisation.docker.enable = true;

  # VM performance optimizations
  boot.kernel.sysctl = {
    "vm.swappiness" = 1;
    "vm.dirty_background_ratio" = 5;
    "vm.dirty_ratio" = 10;
    "vm.vfs_cache_pressure" = 50;
  };

  # Memory optimization
  zramSwap.enable = true;
  zramSwap.memoryPercent = 25;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    inputMethod = {
      type = "fcitx5";
      enable = true;
      fcitx5.addons = with pkgs; [
        fcitx5-mozc
        fcitx5-gtk
        fcitx5-chinese-addons
      ];
    };
  };

  # Window manager toggles -- set to true/false to control which WMs are built.
  # All four enabled for backward compatibility; disable unused ones to speed up rebuilds.
  desktop.xmonad.enable = true;
  desktop.dwm.enable = true;
  desktop.awesome.enable = true;
  desktop.hyprland.enable = true;

  # setup windowing environment
  services = {
    displayManager = {
      defaultSession = "none+dwm";
      sddm = {
        enable = true;
        wayland.enable = true;
        theme = "breeze";
      };
    };

    xserver = {
      enable = true;
      xkb.layout = "us";
      dpi = 220;

      desktopManager = {
        xterm.enable = false;
        wallpaper.mode = "fill";
      };

      displayManager = {
        sessionCommands = ''
          ${pkgs.xorg.xset}/bin/xset r rate 300 60 || true
        '';
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;

  # Manage fonts. We pull these from a secret directory since most of these
  # fonts require a purchase.
  fonts = {
    fontDir.enable = true;

    packages =
      with pkgs;
      [
        fira-code
        fira-code-symbols
        jetbrains-mono
      ]
      ++ builtins.filter lib.attrsets.isDerivation (builtins.attrValues pkgs.nerd-fonts);
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages =
    with pkgs;
    [
      asciidoc
      cachix
      gnumake
      killall
      niv
      rxvt-unicode-unwrapped
      vimHugeX
      dmenu
      picom-pijulius

      (writeShellScriptBin "xrandr-auto" ''
        # Use the new display profile system for auto-detection
        ${../scripts/display-profiles/display-switcher.sh} auto
      '')
    ]
    ++ lib.optionals (currentSystemName == "vm-aarch64") [

      # This is needed for the vmware user tools clipboard to work.
      # You can test if you don't need this by deleting this and seeing
      # if the clipboard sill works.
      gtkmm3

      # VMware on M1 doesn't support automatic resizing yet and on
      # my big monitor it doesn't detect the resolution either so we just
      # manualy create the resolution and switch to it with this script.
      # This script could be better but its hopefully temporary so just force it.
    ];

  environment.sessionVariables = { };

  # Enable the OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.settings.PasswordAuthentication = true;
  services.openssh.settings.PermitRootLogin = "no";

  # Disable the firewall since we're in a VM and we want to make it
  # easy to visit stuff in here. We only use NAT networking anyways.
  networking.firewall.enable = false;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "22.11"; # Did you read the comment?
}
