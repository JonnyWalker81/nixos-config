{
  description = "NixOS configuration and Home Manager configuration";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-23.05";
    nixpkgs-darwin.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";
    nixpkgs-unstable.url = "github:nixos/nixpkgs/nixos-unstable";

    darwin.url = "github:lnl7/nix-darwin/master";
    darwin.inputs.nixpkgs.follows = "nixpkgs-unstable";
    # home-manager.url = "github:nix-community/home-manager/release-21.11";
    home-manager.url = "github:nix-community/home-manager/release-23.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
    # Locks nixpkgs to an older version with an older Kernel that boots
    # on VMware Fusion Tech Preview. This can be swapped to nixpkgs when
    # the TP fixes the bug.
    nixpkgs-old-kernel.url =
      "github:nixos/nixpkgs/bacbfd713b4781a4a82c1f390f8fe21ae3b8b95b";
    # nix-doom-emacs.url = "github:he-la/nix-doom-emacs/develop";
    # nix-doom-emacs.inputs.doom-emacs.follows = "doom-emacs";
    # nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgs";
    # nix-doom-emacs.inputs.emacs-overlay.follows = "emacs-overlay";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    # nix-doom-emacs.url = "github:vlaci/nix-doom-emacs/";
    zig.url = "github:mitchellh/zig-overlay";
  };

  # inputs = {
  #  nixpkgs = { url = "github:nixos/nixpkgs/nixos-unstable"; };
  #  nur = { url = "github:nix-community/NUR"; };
  #};

  outputs = { self, darwin, nixpkgs-darwin, nixpkgs, home-manager, ... }@inputs:
    let
      mkVM = import ./lib/mkvm.nix;
      mkVMDarwin = import ./lib/mkvm-darwin.nix;

      # Overlays is the list of overlays we want to apply from flake inputs.
      overlays = [
        inputs.emacs-overlay.overlay
        # (import (builtins.fetchTarball {
        #   url =
        #     "https://github.com/nix-community/emacs-overlay/archive/dec958258b133b4c21224c594da433919d852800.tar.gz";
        #   sha256 = "0jlvxg0k744zyvdhpvwf2hv3fxc7b31iapjwlm4h3qxsgpjh7gvm";
        # }))
        inputs.zig.overlays.default
        (final: prev: {
          tree-sitter-grammars = prev.tree-sitter-grammars // {
            tree-sitter-tsx =
              prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs (_: {
                nativeBuildInputs = [ final.tree-sitter ];
                configurePhase = ''
                  tree-sitter generate --abi 13 src/grammar.json
                '';
              });
            tree-sitter-go =
              prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs (_: {
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

          awscli2 =
            inputs.nixpkgs-unstable.legacyPackages.${prev.system}.awscli2;
        })
        # (import (fetchGit { url = "https://github.com/jonaburg/picom"; }))
      ];
    in {
      nixosConfigurations.vm-aarch64 = mkVM "vm-aarch64" rec {
        inherit overlays home-manager;
        nixpkgs = inputs.nixpkgs-old-kernel;
        system = "aarch64-linux";
        user = "cipher";
      };

      nixosConfigurations.vm-intel = mkVM "vm-intel" rec {
        inherit overlays nixpkgs home-manager;
        system = "x86_64-linux";
        user = "jrothberg";
      };

      nixosConfigurations.vm-aarch64-prl = mkVM "vm-aarch64-prl" rec {
        inherit overlays nixpkgs home-manager;
        system = "aarch64-linux";
        user = "cipher";
      };

      darwinConfigurations.vm-darwin = mkVMDarwin "vm-darwin" rec {
        inherit overlays home-manager darwin;
        nixpkgs = nixpkgs-darwin;
        system = "x86_64-darwin";
        user = "jrothberg";
      };

    };

}
