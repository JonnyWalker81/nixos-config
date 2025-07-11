{ pkgs, ... }:

{
  # User configuration for phantom on macOS
  users.users.phantom = {
    home = "/Users/phantom";
    shell = pkgs.zsh;
  };

  # Enable zsh system-wide
  programs.zsh.enable = true;

  # macOS-specific system settings
  system = {
    # Set macOS system preferences
    defaults = {
      # Dock settings
      dock = {
        autohide = true;
        show-recents = false;
        launchanim = true;
        mouse-over-hilite-stack = true;
        orientation = "bottom";
        tilesize = 48;
      };

      # Finder settings
      finder = {
        AppleShowAllExtensions = true;
        QuitMenuItem = true;
        FXEnableExtensionChangeWarning = false;
      };

      # Global system settings
      NSGlobalDomain = {
        AppleShowAllExtensions = true;
        ApplePressAndHoldEnabled = false;
        
        # Key repeat settings (faster)
        KeyRepeat = 1;
        InitialKeyRepeat = 10;
        
        # Mouse and trackpad
        "com.apple.mouse.tapBehavior" = 1;
        "com.apple.trackpad.enableSecondaryClick" = true;
      };
    };

    # Keyboard settings
    keyboard = {
      enableKeyMapping = true;
      remapCapsLockToEscape = true;
    };
  };

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;

  # Import overlays (handled by mkvm-darwin.nix at the flake level)
  # nixpkgs.overlays = import ../../lib/overlays.nix;
}