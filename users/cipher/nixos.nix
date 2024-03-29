{ pkgs, ... }:

{
  # https://github.com/nix-community/home-manager/pull/2408
  # environment.pathsToLink = [ "/share/fish" ];
  #
  programs.zsh.enable = true;

  users.users.cipher = {
    isNormalUser = true;
    home = "/home/cipher";
    extraGroups = [ "docker" "wheel" ];
    initialPassword = "cipher";
    shell = pkgs.zsh;
    # hashedPassword = "$6$p5nPhz3G6k$6yCK0m3Oglcj4ZkUXwbjrG403LBZkfNwlhgrQAqOospGJXJZ27dI84CbIYBNsTgsoH650C1EBsbCKesSVPSpB1";
    # openssh.authorizedKeys.keys = [
    #  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGbTIKIPtrymhvtTvqbU07/e7gyFJqNS4S0xlfrZLOaY mitchellh"
    # ];
  };

  nixpkgs.overlays = import ../../lib/overlays.nix;

  # nixpkgs.overlays = import ../../lib/overlays.nix ++ [
  #   (import ./vim.nix)
  # ];
}
