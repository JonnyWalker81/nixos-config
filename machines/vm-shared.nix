{ config, pkgs, currentSystem, currentSystemName, ... }:

{
  # We require 5.14+ for VMware Fusion on M1.
  # boot.kernelPackages = pkgs.linuxPackages_5_15;

  # Be careful updating this.
  boot.kernelPackages = pkgs.linuxPackages_latest;
  # boot.kernelPackages = pkgs.linuxPackages_5_18;

  # use unstable nix so we can access flakes
  nix = {
    package = pkgs.nixUnstable;
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    '';
  };

  # We expect to run the VM on hidpi machines.
  hardware.opengl = { enable = true; };

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.loader.systemd-boot.configurationLimit = 42;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;

  # Define your hostname.
  networking.hostName = "cipher";

  # Set your time zone.
  time.timeZone = "America/Los_Angeles";

  # networking.timeServers = config.networking.timeServers.default ++ [ "ntp.example.com" ];
  services.ntp.enable = true;

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;

  # Don't require password for sudo
  security.sudo.wheelNeedsPassword = false;

  # Virtualization settings
  virtualisation.docker.enable = true;

  # Select internationalisation properties.
  i18n.defaultLocale = "en_US.UTF-8";

  services.picom = { enable = true; };

  # setup windowing environment
  services.xserver = {
    enable = true;
    layout = "us";
    dpi = 220;

    #desktopManager = {
    #  xterm.enable = false;
    #  wallpaper.mode = "scale";
    #};

    # resolutions = [
    #   #  # { x = 2560; y = 1600;}
    #   # { x = 2880; y = 1800;}
    #   (if currentSystem == "aarch64-linux" then {
    #     x = 3840;
    #     y = 2160;
    #   } else {
    #     x = 2560;
    #     y = 1600;
    #   })
    # ];

    desktopManager = {
      xterm.enable = false;
      wallpaper.mode = "fill";
    };

    displayManager = {
      sessionCommands = ''
        ${pkgs.xorg.xset}/bin/xset -r rate 300 100
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
  };

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.mutableUsers = false;

  # Manage fonts. We pull these from a secret directory since most of these
  # fonts require a purchase.
  fonts = {
    fontDir.enable = true;

    fonts = with pkgs; [ fira-code fira-code-symbols nerdfonts jetbrains-mono ];
  };

  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = with pkgs;
    [
      # firefox
      neovim
      gnumake
      killall
      niv
      rxvt_unicode
      xsel
      xclip
      vimHugeX
      nixfmt

      # gitAndTools.gitFull

      dmenu
      xorg.xrandr
      # haskellPackages.libmpd
      # haskellPackages.xmobar
      # haskellPackages.xmonad
      # haskellPackages.greenclip

      (writeShellScriptBin "xrandr-auto" ''
        xrandr --output Virtual-1 --mode  4096x2160
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
    TERM = "xterm-256color";
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
