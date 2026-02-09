# Nixvim from flake input
# This overlay requires flake inputs, so it must be explicitly imported.
{ inputs }:

final: prev: {
  nixvim = inputs.nixvim.packages.${prev.system}.default;
}
