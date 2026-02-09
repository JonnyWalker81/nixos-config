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

  # Overlays are applied at the system level in flake.nix via mkvm-darwin.
  # Do NOT set nixpkgs.overlays here -- it would conflict with the system-level overlays.

  nixpkgs.config.allowUnfree = true;
  nixpkgs.config.allowUnsupportedSystem = true;

  # Homebrew configuration (managed by nix-homebrew)
  homebrew = {
    enable = true;
    onActivation = {
      autoUpdate = true;
      cleanup = "zap"; # Remove unlisted packages
    };
    # Taps managed by nix-homebrew - list here to prevent cleanup from untapping
    taps = [ "homebrew/homebrew-core" "homebrew/homebrew-cask" ];
    brews = [
      # CLI tools managed by Homebrew
      # Example: "wget"
    ];
    casks = [
      # GUI apps managed by Homebrew
      # Example: "firefox"
    ];
  };
}
