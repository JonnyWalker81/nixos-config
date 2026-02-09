{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    home-manager.url = "github:nix-community/home-manager/release-25.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Locks nixpkgs to an older version with an older Kernel that boots
    # on VMware Fusion Tech Preview. This can be swapped to nixpkgs when
    # the TP fixes the bug.
    nixpkgs-old-kernel.url =
      "github:nixos/nixpkgs/bacbfd713b4781a4a82c1f390f8fe21ae3b8b95b";
    emacs-overlay.url = "github:nix-community/emacs-overlay";

    ghostty = { url = "github:ghostty-org/ghostty"; };

    hyprland.url = "github:hyprwm/Hyprland?ref=v0.50.1";

    nixvim = {
      url = "github:JonnyWalker81/cipher-nixvim";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # DankMono font
    dankmono.url = "github:JonnyWalker81/dankmono-font";
    dankmono.inputs.nixpkgs.follows = "nixpkgs";

    claude-code.url = "github:sadjow/claude-code-nix";

    opencode.url = "github:anomalyco/opencode";
    opencode.inputs.nixpkgs.follows = "nixpkgs-unstable";

    # nix-homebrew for managing Homebrew on macOS
    nix-homebrew.url = "github:zhaofengli-wip/nix-homebrew";
    homebrew-core = {
      url = "github:homebrew/homebrew-core";
      flake = false;
    };
    homebrew-cask = {
      url = "github:homebrew/homebrew-cask";
      flake = false;
    };
  };

  outputs = { self, darwin, ghostty, nixpkgs, nixpkgs-unstable, home-manager
    , nix-homebrew, homebrew-core, homebrew-cask, ... }@inputs:
    let
      # Overlays applied to all system configurations.
      # Split into three groups:
      #   1. External flake overlays (from flake inputs directly)
      #   2. Input-dependent overlays (our files that need flake inputs)
      #   3. Auto-discovered overlays (plain final: prev: files in overlays/)
      overlays = [
        # --- External flake overlays ---
        inputs.emacs-overlay.overlay
        inputs.claude-code.overlays.default

        # --- Input-dependent overlays (must be explicitly imported) ---
        (import ./overlays/unstable-packages.nix { inherit inputs; })
        (import ./overlays/fonts.nix { inherit inputs; })
        (import ./overlays/nixvim.nix { inherit inputs; })
        (final: prev: {
          opencode = inputs.opencode.packages.${prev.system}.default;
        })

        # --- Auto-discovered overlays (no inputs needed) ---
        (import ./overlays/default.nix)
        (import ./overlays/dwm.nix)
        (import ./overlays/firefox-hidpi.nix)
        (import ./overlays/picom.nix)
        (import ./overlays/tree-sitter.nix)
        (import ./overlays/vim-plugins.nix)
      ];

      # Single unified builder for all system configurations (NixOS, Darwin, WSL).
      mkSystem = import ./lib/mksystem.nix { inherit overlays nixpkgs inputs; };
    in {

      nixosConfigurations.vm-aarch64-prl = mkSystem "vm-aarch64-prl" {
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-aarch64 = mkSystem "vm-aarch64" {
        nixpkgsOverride = inputs.nixpkgs-old-kernel;
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-intel = mkSystem "vm-intel" {
        system = "x86_64-linux";
        user = "jrothberg";
      };

      darwinConfigurations.vm-darwin = mkSystem "vm-darwin" {
        system = "x86_64-darwin";
        user = "jrothberg";
        darwin = true;
        extraModules = [ inputs.nix-homebrew.darwinModules.nix-homebrew ];
      };

      darwinConfigurations.macbook-phantom = mkSystem "macbook-phantom" {
        system = "aarch64-darwin";
        user = "phantom";
        darwin = true;
        extraModules = [ inputs.nix-homebrew.darwinModules.nix-homebrew ];
      };

      darwinConfigurations.macbook-cipher = mkSystem "macbook-cipher" {
        system = "aarch64-darwin";
        user = "cipher";
        darwin = true;
        extraModules = [ inputs.nix-homebrew.darwinModules.nix-homebrew ];
      };

    };
}
