{ config, lib, pkgs, ... }: 
{

   home.packages =  [
     pkgs.fd
     pkgs.go
     pkgs.gopls
     pkgs.rustup
     pkgs.clang
   ];

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
