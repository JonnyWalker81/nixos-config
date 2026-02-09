{ config, lib, pkgs, system, inputs, ... }:

{
  home.stateVersion = "25.05";
  home.enableNixpkgsReleaseCheck = false; # Disable version mismatch warning
  xdg.enable = true;

  # Pass inputs to sub-modules that need it (e.g., packages.nix for nixvim)
  _module.args.inputs = inputs;

  imports = [
    ./packages.nix
    ./shell.nix
    ./git.nix
    ./editors.nix
    ./terminal.nix
    ./desktop.nix
    ./services.nix
    ./dotfiles.nix
  ];
}
