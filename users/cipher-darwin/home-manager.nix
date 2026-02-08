{ config, lib, pkgs, ... }:

{
  # Import common configuration but with Darwin-specific conditionals
  imports = [ ../common.nix ];

  # Disable the broken application/font linking (home-manager bug with buildEnv)
  disabledModules = [
    "targets/darwin/linkapps.nix"
    "targets/darwin/fonts.nix"
  ];

  # User-specific git configuration for cipher
  programs.git.userEmail = "jon@join.build";

  # macOS-specific shell aliases
  home.shellAliases = {
    # Darwin rebuild shortcuts
    dm = "sudo darwin-rebuild switch --flake ~/nixos-config#macbook-cipher";
    dh = "home-manager switch --flake ~/nixos-config#macbook-cipher";

    # macOS-specific aliases
    brewup = "brew update && brew upgrade";
    finder = "open -a Finder";

    # Override Linux-specific aliases for macOS
    pbcopy = lib.mkForce "pbcopy";  # Use native macOS pbcopy
    pbpaste = lib.mkForce "pbpaste"; # Use native macOS pbpaste
  };

  # macOS-specific packages (in addition to common.nix)
  home.packages = with pkgs; [
    # macOS-specific development tools
    rectangle  # Window management

    # Neovim with nixvim configuration
    pkgs.nixvim

    # Native macOS alternatives to Linux tools
    # (common.nix handles the platform-agnostic tools)
  ];

  # Override/disable Linux-specific configurations (only apply on Linux to avoid module evaluation on Darwin)
  programs.waybar = lib.mkIf (!pkgs.stdenv.isDarwin) (lib.mkForce { enable = false; });

  # Disable all Wayland/Hyprland configurations (only on Linux to avoid wayland package evaluation on Darwin)
  wayland.windowManager.hyprland = lib.mkIf (!pkgs.stdenv.isDarwin) {
    enable = lib.mkForce false;
  };

  # macOS-specific session variables
  home.sessionVariables = {
    # Override Linux-specific variables
    BROWSER = "open";

    # Remove the Linux SSH agent path override - let macOS handle it
    # SSH_AUTH_SOCK is managed by macOS launchd
  };

  # Ensure home-manager bin is in PATH before homebrew
  home.sessionPath = [
    "$HOME/.local/state/nix/profiles/home-manager/home-path/bin"
  ];

  # macOS-specific services (disable Linux systemd services)
  systemd.user.services = lib.mkForce {};

  # macOS Terminal and shell optimization
  programs.alacritty.settings = lib.mkIf pkgs.stdenv.isDarwin {
    window = {
      decorations = "buttonless";
      option_as_alt = "Both";
    };

    key_bindings = [
      { key = "C"; mods = "Command"; action = "Copy"; }
      { key = "V"; mods = "Command"; action = "Paste"; }
      { key = "Q"; mods = "Command"; action = "Quit"; }
      { key = "N"; mods = "Command"; action = "SpawnNewInstance"; }
    ];
  };

  # Configure git for macOS
  programs.git = {
    extraConfig = {
      # macOS-specific git settings
      credential.helper = lib.mkForce "osxkeychain";
    };
  };

  # SSH configuration for macOS
  programs.ssh = {
    extraConfig = lib.mkIf pkgs.stdenv.isDarwin ''
      # macOS-specific SSH settings
      UseKeychain yes
      AddKeysToAgent yes

      # Store SSH keys in macOS Keychain
      IdentityFile ~/.ssh/id_ed25519
      IdentityFile ~/.ssh/id_rsa
    '';
  };

  # Ensure direnv works properly on macOS
  programs.direnv = {
    enable = true;
    nix-direnv.enable = true;
    enableZshIntegration = true;
  };

  # Disable X11/Linux cursor configuration on macOS
  home.pointerCursor = lib.mkIf (!pkgs.stdenv.isDarwin) {
    name = "Adwaita";
    package = pkgs.adwaita-icon-theme;
    size = 32;
    x11.enable = true;
  };
}
