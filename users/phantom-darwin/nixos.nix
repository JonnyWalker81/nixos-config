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
    # Required for nix-darwin
    stateVersion = 6;
    
    # Set the primary user for system defaults
    primaryUser = "phantom";
    
    # Set macOS system preferences
    defaults = {
      # Dock settings
      dock = {
        autohide = false;
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

  # Disable nix-darwin's Nix management for Determinate Systems compatibility
  nix.enable = false;

  # Allow unfree packages
  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;

  # Import overlays (handled by mkvm-darwin.nix at the flake level)
  # nixpkgs.overlays = import ../../lib/overlays.nix;
  
  # System packages that should be available and linked to /Applications
  environment.systemPackages = with pkgs; [
    # Add Emacs to system packages so it appears in /Applications/Nix Apps
    (if stdenv.isDarwin then emacs-unstable else emacs)
    # Add Ghostty to system packages so it appears in /Applications/Nix Apps
    ghostty
  ];
  
  # Fonts configuration for macOS
  fonts = {
    packages = with pkgs; [
      jetbrains-mono
      monaspace
      fira-code
      fira-code-symbols
      # (nerdfonts.override { fonts = [ "JetBrainsMono" ]; })
    ];
  };
}