# Packages pulled from unstable nixpkgs and ghostty platform-conditional build
# This overlay requires flake inputs, so it must be explicitly imported.
{ inputs }:

final: prev:
let
  unstablePkgs = import inputs.nixpkgs-unstable {
    system = prev.system;
    config.allowUnfree = true;
  };
in {
  # Expose the full unstable package set for downstream use
  unstable = unstablePkgs;

  # Specific packages from unstable
  kitty = unstablePkgs.kitty;
  xmobar = unstablePkgs.xmobar;
  awscli2 = unstablePkgs.awscli2;

  # Use the flake overlay for Linux (optimized build), ghostty-bin from unstable for macOS
  ghostty = if prev.stdenv.isLinux then
    inputs.ghostty.packages.${prev.system}.default
  else
    unstablePkgs.ghostty-bin;
}
