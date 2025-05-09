{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-24.11";
    # nixpkgs.url = "github:nixos/nixpkgs/nixos-23.11";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-24.11-darwin";
    # nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.11-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    zen-browser.url = "github:0xc000022070/zen-browser-flake";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-24.11";
    # home-manager.url = "github:nix-community/home-manager/release-23.11";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Locks nixpkgs to an older version with an older Kernel that boots
    # on VMware Fusion Tech Preview. This can be swapped to nixpkgs when
    # the TP fixes the bug.
    nixpkgs-old-kernel.url = "github:nixos/nixpkgs/bacbfd713b4781a4a82c1f390f8fe21ae3b8b95b";
    # nix-doom-emacs.url = "github:he-la/nix-doom-emacs/develop";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    # nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    # nightly-tools.url = "github:calops/nightly-tools";
    #
    # # Rust toolchain
    # fenix.url = "github:nix-community/fenix";
    # fenix.inputs.nixpkgs.follows = "nixpkgs";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # nix-doom-emacs.url = "github:vlaci/nix-doom-emacs/";
    zig.url = "github:mitchellh/zig-overlay";

    ghostty = {
      url = "github:ghostty-org/ghostty";
    };

    # Replacement for ls
    # eza = {
    #  url = "github:eza-community/eza";
    #  inputs = {
    #    nixpkgs.follows = "nixpkgs";
    #    # rust-overlay.follows = "rust-overlay";
    #  };
    #};

    hyprland.url = "github:hyprwm/Hyprland";
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

    neovim-flake = {
      url = "github:jordanisaacs/neovim-flake";
    };

    # Neve = {
    #   url = "github:redyf/Neve";
    #   inputs.nixpkgs.follows = "nixpkgs-unstable";
    # };

    # nixvim = {
    #   url = "github:niksingh710/nvix";
    #   inputs.nixpkgs.follows = "nixpkgs-unstable";
    # };

    # nixvim = {
    #   url = "github:fred-drake/neovim";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    # nixvim = {
    #   url = "github:spector700/Akari";
    # };

    # nixvim = {
    #   url = "github:elythh/nixvim";
    #   inputs.nixpkgs.follows = "nixpkgs";
    # };

    nixvim = {
      # url = "github:nix-community/nixvim";
      url = "github:JonnyWalker81/cipher-nixvim";
      # If you are not running an unstable channel of nixpkgs, select the corresponding branch of nixvim.
      # url = "github:nix-community/nixvim/nixos-24.11";

      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };

    # yazi.url = "github:sxyazi/yazi";
  };

  # inputs = {
  #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
  #  nur = { url = "github:nix-community/NUR"; };
  #};

  outputs =
    {
      self,
      darwin,
      ghostty,
      nixpkgs-darwin,
      nixpkgs,
      nixpkgs-unstable,
      neovim-flake,
      home-manager,
      zen-browser,
      ...
    }@inputs:
    let
      # mkVM = import ./lib/mkvm.nix { inherit overlays nixpkgs inputs; };
      mkVM = import ./lib/mkvm.nix;
      mkVMDarwin = import ./lib/mkvm-darwin.nix;
      system = "aarch64-linux";
      pkgs = nixpkgs.legacyPackages.${system};

      configModule = {
        # Add any custom options (and feel free to upstream them!)
        # options = ...

        config.vim.theme.enable = true;
      };

      # customNeovim = neovim-flake.lib.neovimConfiguration {
      #   modules = [ configModule ];
      #   inherit pkgs;
      # };

      # Overlays is the list of overlays we want to apply from flake inputs.
      overlays = [
        inputs.emacs-overlay.overlay
        # inputs.yazi.overlays.default
        # inputs.neovim-nightly-overlay.overlay
        inputs.neovim-nightly-overlay.overlays.default
        # (import (builtins.fetchTarball {
        #   url =
        #     "https://github.com/nix-community/emacs-overlay/archive/dec958258b133b4c21224c594da433919d852800.tar.gz";
        #   sha256 = "0jlvxg0k744zyvdhpvwf2hv3fxc7b31iapjwlm4h3qxsgpjh7gvm";
        # }))
        inputs.zig.overlays.default
        (final: prev: {
          picom = prev.picom.overrideAttrs (oldAttrs: {
            src = prev.fetchFromGitHub {
              owner = "yshui";
              repo = "picom";
              rev = "v12.5"; # Correct version
              sha256 = "sha256-H8IbzzrzF1c63MXbw5mqoll3H+vgcSVpijrlSDNkc+o=";
            };
            nativeBuildInputs = oldAttrs.nativeBuildInputs ++ [ prev.asciidoc ];
          });

          unstable = import nixpkgs-unstable { system = prev.system; };
          tree-sitter-grammars = prev.tree-sitter-grammars // {
            tree-sitter-tsx = prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs (_: {
              nativeBuildInputs = [ final.tree-sitter ];
              configurePhase = ''
                tree-sitter generate --abi 13 src/grammar.json
              '';
            });
            tree-sitter-go = prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs (_: {
              nativeBuildInputs = [ final.tree-sitter ];
              configurePhase = ''
                tree-sitter generate --abi 13 src/grammar.json
              '';
            });

          };
          # emacs = (import (builtins.fetchTarball {
          #   url =
          #     "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
          # }));

          # Go we always want the latest version
          # go = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.go_1_20;

          # To get Kitty 0.24.x. Delete this once it hits release.
          kitty = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.kitty;

          xmobar = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xmobar;

          awscli2 = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.awscli2;

          ghostty = ghostty.packages.${prev.system}.default;

          # nixvim = inputs.nixvim.packages.${pkgs.system}.default;
          # nixvim = inputs.nixvim.packages.${pkgs.system}.default.overrideAttrs (oldAttrs: {
          #   package = inputs.neovim-nightly-overlay.packages.${pkgs.system}.default;
          # });
          # neovim = inputs.Neve.packages.${pkgs.system}.default;
          # picom =
          #   inputs.nixpkgs-unstable.legacyPackages.${prev.system}.picom.overrideAttrs
          #   (o: {
          #     src = prev.fetchFromGitHub {
          #       repo = "pfastcompmgricom";
          #       owner = "ibhagwan";
          #       rev = "44b4970f70d6b23759a61a2b94d9bfb4351b41b1";
          #       sha256 = "0iff4bwpc00xbjad0m000midslgx12aihs33mdvfckr75r114ylh";
          #     };
          #     buildInputs = [
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.meson
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.pcre
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.cmake
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.libev
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.libX11
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.xcbutilrenderutil
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.xcbutil
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.libxcb
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.libxcb.dev
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.xcbutilimage
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xorg.libXext
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.pixman
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.libconfig
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.libGL
          #       inputs.nixpkgs-unstable.legacyPackages.${prev.system}.dbus
          #     ];
          #     # src = prev.fetchFromGitHub {
          #     #   repo = "picom";
          #     #   owner = "jonaburg";
          #     #   rev = "e3c19cd7d1108d114552267f302548c113278d45";
          #     #   sha256 = "4voCAYd0fzJHQjJo4x3RoWz5l3JJbRvgIXn1Kg6nz6Y=";
          #     # };
          # p });
        })
        # (import (fetchGit { url = "https://github.com/jonaburg/picom"; }))
      ];

      # pkgs = import nixpkgs {
      #   inherit system overlays;
      # };

      mkSystem = import ./lib/mksystem.nix { inherit overlays nixpkgs inputs; };
    in
    {

      # packages.${system}.neovim = customNeovim;

      # nixosConfigurations.vm-aarch64 = mkSystem "vm-aarch64" {
      #   system = "aarch64-linux";
      #   user = "cipher";
      # };

      nixosConfigurations.vm-aarch64-prl = mkSystem "vm-aarch64-prl" {
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" rec {
        inherit overlays home-manager inputs;
        nixpkgs = inputs.nixpkgs-old-kernel;
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-intel = mkVM "vm-intel" rec {
        inherit
          overlays
          nixpkgs
          home-manager
          inputs
          ;
        system = "x86_64-linux";
        user = "jrothberg";
      };

      # nixosConfigurations.vm-aarch64-prl = mkVM "vm-aarch64-prl" rec {
      #   inherit overlays home-manager inputs nixpkgs;
      #   # nixpkgs = nixpkgs-unstable;
      #   system = "aarch64-linux";
      #   user = "cipher";
      # };

      darwinConfigurations.vm-darwin = mkVMDarwin "vm-darwin" rec {
        inherit
          overlays
          home-manager
          darwin
          inputs
          ;
        nixpkgs = nixpkgs-darwin;
        system = "x86_64-darwin";
        user = "jrothberg";
      };

    };
}
