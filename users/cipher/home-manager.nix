args@{ config, lib, pkgs, nix-doom-emacs, ... }: 

 
 # myEmacs
 {
  # imports = [
 #   nix-doom-emacs.hmModule
 # ];
#  unstable = import (fetchTarball
#    "https://github.com/NixOS/nixpkgs/archive/nixos-unstable.tar.gz") {
#      overlays = [
#        (import (builtins.fetchTarball {
#          url = https://github.com/nix-community/emacs-overlay/archive/610d85782bcf71a7821a2019055e7b411e28caec.tar.gz;
#          sha256 = "610d85782bcf71a7821a2019055e7b411e28caec";
#        }))
#      ];
#    };

# services.emacs.package = pkgs.emacsGcc;
# 
# nixpkgs.overlays = [
#  (import (builtins.fetchTarball {
#    # url = https://github.com/nix-community/emacs-overlay/archive/610d85782bcf71a7821a2019055e7b411e28caec.tar.gz;
#    url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
#  }))
# ];
# services.emacs.enable = true;
  home.file.".doom.d" = {
      source = ../../doom.d;
      # target = "~/.doom.d";
      recursive = true;
      # onChange = "doom upgrade";
    };

    home.file.".elisp" = {
      source = ../../elisp;
      recursive = true;
    };

   # home.file.emacs-config = {
   #   source = ../../../Repositories/doom-emacs;
   #   target = ".config/emacs";
   #   recursive = true;
   # };

  programs.emacs = {
    enable = true;
    package = pkgs.emacsGcc;
    extraPackages = (epkgs: [ epkgs.vterm ] );
   # doomPrivateDir = ../../doom.d;
   #  emacsPackagesOverlay = self: super: {
   #     # fixes https://github.com/vlaci/nix-doom-emacs/issues/394
   #     gitignore-mode = pkgs.emacsPackages.git-modes;
   #     gitconfig-mode = pkgs.emacsPackages.git-modes;
   #   };
  };

 # environment.defaultPackages = with pkgs; [
 #   emacsGcc
 # ];
 
#  services.emacs.package = pkgs.emacsPgtkGcc;
# 
#  nixpkgs.overlays = [
#    (import (builtins.fetchTarball {
#      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
#    }))
#  ];

 # environment.defaultPackages = with pkgs; [
 #   emacsGit
 # ];

# programs.emacs = {
#   enable = true;
#   package = pkgs.emacsGcc;
# };

   home.packages =  [
     pkgs.fd
     pkgs.go
     pkgs.gopls
     pkgs.rustup
     pkgs.clang
   ];

  # programs.emacs = {
  #   enable = true;
  #   version = "29.0.50";
  # };

   programs.git = {
     enable = true;
   };

   programs.zsh = {
     enable = true;
     shellAliases = {
       ll = "ls -l";
       update = "sudo nixos-rebuild switch --flake .#vm-aarch64";
     };

     enableAutosuggestions = true;
     sessionVariables = {
          LC_ALL = "en_US.utf8";
          LIBVIRT_DEFAULT_URI = "qemu:///system";
          GOPATH = "\${HOME}";
          PATH = "\${PATH}:\${HOME}/bin:\${HOME}/.cargo/bin";

          ZSH_TMUX_AUTOSTART = "true";
          ZSH_TMUX_AUTOCONNECT = "true";
     };

     oh-my-zsh = {
	    enable = true;
	    plugins = [ "git" "thefuck"];
	    theme = "robbyrussell";
    };

     history = {
       size = 100000;
       path = "${config.xdg.dataHome}/zsh/history";
     };
   };


   programs.neovim.enable = true;
   programs.neovim.viAlias = true;
   programs.neovim.vimAlias = true;


  programs.starship = {
    enable = true;
    # Configuration written to ~/.config/starship.toml
    settings = {
       add_newline = true;

       character = {
         success_symbol = "[➜](bold green)";
         error_symbol = "[➜](bold red)";
       };

      # package.disabled = true;
    };
  };
}
