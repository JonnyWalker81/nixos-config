{ config, pkgs, currentSystem, currentSystemName, inputs, ... }:

{
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  services.journald.extraConfig = "SystemMaxUse=100M";
  nix = {
    package = pkgs.nixVersions.latest;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';

    settings = {
      substituters = [ "https://mitchellh-nixos-config.cachix.org" ];
      trusted-public-keys = [
        "mitchellh-nixos-config.cachix.org-1:bjEbXJyLrL1HZZHBbO4QALnI5faYZppzkU4D2s0G8RQ="
      ];
    };
  };

  # We expect to run the VM on hidpi machines.
  hardware.graphics = {
    enable = true;

    # For VM environments, ensure software rendering fallback
    extraPackages = with pkgs; [ mesa libvdpau-va-gl vaapiVdpau ];
  };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
  nixpkgs.config.input-fonts.acceptLicense = true;

  boot.loader.systemd-boot.consoleMode = "0";

  # Define your hostname.
  networking.hostName = "cipher";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  networking.timeServers = [ "pool.ntp.org" "time.nist.gov" ];
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

  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    xwayland.enable = true;
  };

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
          ${pkgs.xorg.xset}/bin/xset r rate 1000 1000
        '';
      };

      windowManager.xmonad = {
        enable = true;
        enableContribAndExtras = true;

        extraPackages = hpkgs: [
          hpkgs.xmonad-contrib
          hpkgs.xmonad-extras
          hpkgs.xmonad
        ];
      };

      windowManager.dwm.enable = true;

      windowManager.awesome = {
        enable = true;

        luaModules = with pkgs.luaPackages; [ luarocks luadbi-mysql ];
      };
    };
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;

  # Manage fonts. We pull these from a secret directory since most of these
  # fonts require a purchase.
  fonts = {
    fontDir.enable = true;

    packages = with pkgs;
      [ fira-code fira-code-symbols jetbrains-mono ]
      ++ builtins.filter lib.attrsets.isDerivation
      (builtins.attrValues pkgs.nerd-fonts);
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      asciidoc
      cachix
      gnumake
      killall
      niv
      rxvt-unicode-unwrapped
      vimHugeX
      nixfmt

      dmenu
      picom-pijulius

      (writeShellScriptBin "xrandr-auto" ''
        # Use the new display profile system for auto-detection
        ${../scripts/display-profiles/display-switcher.sh} auto
      '')
    ] ++ lib.optionals (currentSystemName == "vm-aarch64") [

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
