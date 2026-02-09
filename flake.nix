{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-25.05-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    zen-browser.url = "github:0xc000022070/zen-browser-flake";

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
    zig.url = "github:mitchellh/zig-overlay";

    ghostty = { url = "github:ghostty-org/ghostty"; };

    mozilla.url = "github:mozilla/nixpkgs-mozilla";

    hyprland.url = "github:hyprwm/Hyprland?ref=v0.50.1";
    hyprland-plugins = {
      url = "github:hyprwm/hyprland-plugins";
      inputs.hyprland.follows = "hyprland";
    };

    # I think technically you're not supposed to override the nixpkgs
    # used by neovim but recently I had failures if I didn't pin to my
    # own. We can always try to remove that anytime.
    neovim-nightly-overlay = {
      url = "github:nix-community/neovim-nightly-overlay";

      # Only need unstable until the lpeg fix hits mainline, probably
      # not very long... can safely switch back for 23.11.
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    neovim-flake = { url = "github:jordanisaacs/neovim-flake"; };

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

  outputs = { self, darwin, ghostty, nixpkgs-darwin, nixpkgs, nixpkgs-unstable
    , neovim-flake, home-manager, zen-browser, nix-homebrew, homebrew-core
    , homebrew-cask, ... }@inputs:
    let
      mkVM = import ./lib/mkvm.nix;
      mkVMDarwin = import ./lib/mkvm-darwin.nix;

      # Overlays applied to all system configurations.
      # Split into three groups:
      #   1. External flake overlays (from flake inputs directly)
      #   2. Input-dependent overlays (our files that need flake inputs)
      #   3. Auto-discovered overlays (plain final: prev: files in overlays/)
      overlays = [
        # --- External flake overlays ---
        inputs.mozilla.overlays.firefox
        inputs.emacs-overlay.overlay
        inputs.neovim-nightly-overlay.overlays.default
        inputs.zig.overlays.default
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

      mkSystem = import ./lib/mksystem.nix { inherit overlays nixpkgs inputs; };
    in {

      nixosConfigurations.vm-aarch64-prl = mkSystem "vm-aarch64-prl" {
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" {
        inherit overlays home-manager inputs;
        nixpkgs = inputs.nixpkgs-old-kernel;
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-intel = mkVM "vm-intel" {
        inherit overlays nixpkgs home-manager inputs;
        system = "x86_64-linux";
        user = "jrothberg";
      };

      darwinConfigurations.vm-darwin = mkVMDarwin "vm-darwin" {
        inherit overlays home-manager darwin inputs;
        nixpkgs = nixpkgs-darwin;
        system = "x86_64-darwin";
        user = "jrothberg";
      };

      darwinConfigurations.macbook-phantom = mkVMDarwin "macbook-phantom" {
        inherit overlays home-manager darwin inputs;
        nixpkgs = nixpkgs-darwin;
        system =
          "aarch64-darwin"; # Apple Silicon - change to "x86_64-darwin" if Intel Mac
        user = "phantom";
      };

      darwinConfigurations.macbook-cipher = mkVMDarwin "macbook-cipher" {
        inherit overlays home-manager darwin inputs;
        nixpkgs = nixpkgs-darwin;
        system = "aarch64-darwin"; # Apple Silicon
        user = "cipher";
      };

    };
}
