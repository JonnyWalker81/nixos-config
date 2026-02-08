{ config, pkgs, currentSystem, currentSystemName, inputs, ... }:

{
  # We require 5.14+ for VMware Fusion on M1.
  # boot.kernelPackages = pkgs.linuxPackages_5_15;

  # Be careful updating this.
  # boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelPackages = pkgs.linuxPackages_6_6;
  # boot.kernelPackages = pkgs.linuxPackages_5_18;
  services.journald.extraConfig = "SystemMaxUse=100M";
  # use unstable nix so we can access flakes
  nix = {
    # package = pkgs.nixUnstable;
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

    # settings = {
    #
    #   substituters = [ "https://hyprland.cachix.org" ];
    #   trusted-public-keys = [ "hyprland.cachix.org-1:a7pgxzMz7+chwVL3/pzj6jIBMioiJM7ypFP8PwtkuGc=" ];
    # };
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
  # boot.loader.systemd-boot.configurationLimit = 42;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;
  nixpkgs.config.input-fonts.acceptLicense = true;

  boot.loader.systemd-boot.consoleMode = "0";

  # Define your hostname.
  networking.hostName = "cipher";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # networking.timeServers = config.networking.timeServers.default ++ [ "ntp.example.com" ];
  networking.timeServers = [ "pool.ntp.org" "time.nist.gov" ];
  services.timesyncd.enable = true;

  # services.ntp.enable = true;

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

  # Select internationalisation properties.
  # i18n.defaultLocale = "en_US.UTF-8";

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

  # services.picom = {
  # enable = false;
  #   activeOpacity = 0.8;
  #   inactiveOpacity = 0.5;
  #   backend = "xr_glx_hybrid";
  #   # settings = {
  #   #   blur = false;
  #   #   blurExclude = [ "\n      class_g = 'slop'" "class_g *?= 'Firefox'" ];
  #   #   blur-method = "dual_kawase";
  #   #   blur-strength = 5;
  #   #
  #   #   # Radius
  #   #   corner-radius = 10;
  #   #   round-borders = 1;
  #   #   rounded-corners-exclude = [ "class_g = 'Custom-taffybar'" ];
  #   # };
  #   # extraOptions = ''
  #   #   corner-radius = 10;
  #   #   blur-method = "dual_kawase";
  #   #   blur-strength = "10";
  #   #   xinerama-shadow-crop = true;
  #   # '';
  #   # experimentalBackends = true;
  #
  #   # shadowExclude = [ "bounding_shaped && !rounded_corners" ];
  #
  #   # fade = true;
  #   # fadeDelta = 5;
  #   # vSync = true;
  #   # opacityRules = [
  #   #   "100:class_g = 'firefox' && window_type = 'utility'"
  #   #   "100:class_g   *?= 'Chromium-browser'"
  #   #   "100:class_g   *?= 'Firefox'"
  #   #   "100:class_g   *?= 'gitkraken'"
  #   #   "100:class_g   *?= 'emacs'"
  #   #   "100:class_g   ~=  'jetbrains'"
  #   #   "100:class_g   *?= 'slack'"
  #   # ];
  #
  #   package = pkgs.picom.overrideAttrs (o: {
  #     src = pkgs.fetchFromGitHub {
  #       repo = "picom";
  #       owner = "ibhagwan";
  #       rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
  #       sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
  #     };
  #   });
  # };

  programs.hyprland = {
    enable = true;
    package = inputs.hyprland.packages.${pkgs.system}.hyprland;
    xwayland.enable = true;
  };

  # programs.sway.enable = true;
  # security.polkit.enable = true;
  # hardware.opengl.enable = true;

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
        # lightdm.enable = true;
        # defaultSession = "none+awesome";
        # defaultSession = "none+xmonad";
        # ${pkgs.xorg.xrandr}/bin/xrandr -s '1920x1080'
        # ${pkgs.xorg.xset}/bin/xset r rate 1000 1000
        sessionCommands = ''
            # ${pkgs.xorg.xset}/bin/xset r rate 200 50
          ${pkgs.xorg.xset}/bin/xset r rate 1000 1000
        '';
        # sddm.enable = true;
        # sddm.enableHidpi = true;
        # defaultSession = "none+i3";
        # lightdm.enable = true;

        # AARCH64: For now, on Apple Silicon, we must manually set the
        # display resolution. This is a known issue with VMware Fusion.

        # sessionCommands = ''
        # ${pkgs.xorg.xrdb}/bin/xrdb -merge <<EOF
        #  xft.dpi: 192
        #  Xcursor.theme: Adwaita
        #  Xcursor.size: 64
        # EOF
        # '';
        # sessionCommands = ''
        #   ${pkgs.xorg.xset}/bin/xset r rate 200 40
        #   ${pkgs.xorg.xrandr}/bin/xrandr -s '2880x1800'
        # '';
      };

      # windowManager = {
      #   i3.enable = true;
      # };

      windowManager.xmonad = {
        #  i3.enable = true;
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
        # i3.enable = true;
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
      # firefox
      # neovim
      asciidoc
      cachix
      gnumake
      killall
      niv
      rxvt-unicode-unwrapped
      vimHugeX
      # nixfmt-classic
      nixfmt
      # linuxKernel.packages.linux_6_6.prl-tools  # Managed by parallels-guest module

      # gitAndTools.gitFull

      dmenu
      picom-pijulius
      # xorg.xrandr
      # xorg.xprop
      # haskellPackages.libmpd
      # haskellPackages.xmobar
      # haskellPackages.xmonad
      # haskellPackages.greenclip

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
      # (writeShellScriptBin "xrandr-4k" ''
      #   xrandr --newmode "4096x2160_60.00"  1768.50  6016 6544 7216 8416  3384 3387 3392 3503 -hsync +vsync
      #   xrandr --addmode Virtual-1 4096x2160_60.00
      #   xrandr -s 4096x2160_60.00
      # '')
      # (writeShellScriptBin "xrandr-mbp" ''
      #   xrandr -s 4096x2160
      #   xrandr --output Virtual-1 --mode 4096x2160
      # '')
    ];

  # xrandr -s 2880x1800
  environment.sessionVariables = {
    # GDK_SCALE = "2";
    # WINIT_HIDPI_FACTOR = "1";
    # TERM = "xterm-256color";
    # DISPLAY = ":0";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  # programs.mtr.enable = true;
  # programs.gnupg.agent = {
  #   enable = true;
  #   enableSSHSupport = true;
  # };

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
