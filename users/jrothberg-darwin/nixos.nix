{ pkgs, ... }:

{
  # https://github.com/nix-community/home-manager/pull/2408
  # environment.pathsToLink = [ "/share/fish" ];

  users.users.jrothberg = {
    # isNormalUser = true;
    home = "/Users/jrothberg";
    # extraGroups = [ "docker" "wheel" ];
    # initialPassword = "jrothberg";
    shell = pkgs.zsh;
    # hashedPassword = "$6$p5nPhz3G6k$6yCK0m3Oglcj4ZkUXwbjrG403LBZkfNwlhgrQAqOospGJXJZ27dI84CbIYBNsTgsoH650C1EBsbCKesSVPSpB1";
    # openssh.authorizedKeys.keys = [
    #  "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGbTIKIPtrymhvtTvqbU07/e7gyFJqNS4S0xlfrZLOaY mitchellh"
    # ];
  };

  nixpkgs.overlays = import ../../lib/overlays.nix;

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;

  # nixpkgs.overlays = import ../../lib/overlays.nix ++ [
  #   (import ./vim.nix)
  # ];
}
