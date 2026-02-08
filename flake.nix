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

      # Overlays is the list of overlays we want to apply from flake inputs.
      overlays = [
        inputs.mozilla.overlays.firefox
        inputs.emacs-overlay.overlay
        inputs.neovim-nightly-overlay.overlays.default

        inputs.zig.overlays.default
        inputs.claude-code.overlays.default
        (final: prev: {
          opencode = inputs.opencode.packages.${prev.system}.default;
        })
        (final: prev: {
          # Use yshui's picom (latest version)
          picom = prev.picom.overrideAttrs (oldAttrs: rec {
            pname = "picom";
            version = "unstable-latest";
            src = prev.fetchFromGitHub {
              owner = "yshui";
              repo = "picom";
              rev = "b700a37d56ab5debdbb78be7a6b905e72f69ff2d";
              sha256 =
                "sha256-C+icJXTkE+XMaU7N6JupsP8xhmRVggX9hY1P7za0pO0="; # Will be filled by nix
            };
            buildInputs = (oldAttrs.buildInputs or [ ])
              ++ [ prev.pcre prev.libconfig prev.libev prev.uthash ];
            nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ])
              ++ [ prev.asciidoc prev.pkg-config prev.meson prev.ninja ];
            doCheck = false;
            doInstallCheck = false;
          });

          luaPackages = prev.luaPackages // {
            fzf-lua = prev.luaPackages.fzf-lua.overrideAttrs (old: {
              # don't run the flaky UI tests
              doCheck = false;
              checkPhase = "echo skipping fzf-lua tests";
            });
          };

          # Fix for CopilotChat.nvim requiring fzf-lua
          vimPlugins = prev.vimPlugins // {
            CopilotChat-nvim = prev.vimPlugins.CopilotChat-nvim.overrideAttrs
              (old: {
                # Disable the require check that's failing
                doCheck = false;
                nvimRequireCheck = "";

                # Override the build phase to skip the check
                buildPhase = ''
                  runHook preBuild
                  runHook postBuild
                '';

                # Skip the install check phase
                installCheckPhase = ''
                  runHook preInstallCheck
                  runHook postInstallCheck
                '';
              });
          };

          unstable = import nixpkgs-unstable {
            system = prev.system;
            config.allowUnfree = true;
          };
          tree-sitter-grammars = prev.tree-sitter-grammars // {
            tree-sitter-tsx =
              prev.tree-sitter-grammars.tree-sitter-tsx.overrideAttrs (_: {
                nativeBuildInputs = [ final.tree-sitter ];
                configurePhase = ''
                  tree-sitter generate --abi 13 src/grammar.json
                '';
              });
            tree-sitter-go =
              prev.tree-sitter-grammars.tree-sitter-go.overrideAttrs (_: {
                nativeBuildInputs = [ final.tree-sitter ];
                configurePhase = ''
                  tree-sitter generate --abi 13 src/grammar.json
                '';
              });

          };

          # To get Kitty 0.24.x. Delete this once it hits release.
          kitty = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.kitty;

          xmobar = inputs.nixpkgs-unstable.legacyPackages.${prev.system}.xmobar;

          awscli2 =
            inputs.nixpkgs-unstable.legacyPackages.${prev.system}.awscli2;

          # Use the flake overlay for Linux (optimized build), ghostty-bin from unstable for macOS
          ghostty = if prev.stdenv.isLinux then
            ghostty.packages.${prev.system}.default
          else
            inputs.nixpkgs-unstable.legacyPackages.${prev.system}.ghostty-bin;

          nixvim = inputs.nixvim.packages.${prev.system}.default;

          # DankMono font - build it with our nixpkgs that allows unfree
          dankmono = prev.stdenv.mkDerivation rec {
            pname = "dankmono";
            version = "1.0.0";

            src = inputs.dankmono;

            installPhase = ''
              runHook preInstall

              # Install TrueType fonts
              install -D -m644 -t $out/share/fonts/truetype/dankmono OpenType-TT/*.ttf

              # Install OpenType fonts
              install -D -m644 -t $out/share/fonts/opentype/dankmono OpenType-PS/*.otf

              # Install web fonts
              install -D -m644 -t $out/share/fonts/woff2/dankmono Web-PS/*.woff2
              install -D -m644 Web-PS/dmvendor.css $out/share/fonts/woff2/dankmono/dmvendor.css

              # Install documentation
              install -D -m644 README.txt $out/share/doc/dankmono/README.txt
              install -D -m644 EULA.txt $out/share/licenses/dankmono/EULA.txt

              runHook postInstall
            '';

            # Add font configuration for fontconfig (Linux)
            postInstall = prev.lib.optionalString prev.stdenv.isLinux ''
              # Generate fontconfig cache for Linux
              ${prev.fontconfig}/bin/fc-cache -f $out/share/fonts/
            '';

            meta = with prev.lib; {
              description = "DankMono programming font";
              longDescription = ''
                Dank Mono is a monospaced font designed for coding with
                ligatures and a distinctive style.
              '';
              license = licenses.unfree;
              platforms = platforms.all;
            };
          };
        })
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
