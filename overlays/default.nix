# This contains various packages we want to overlay. Note that the
# other ".nix" files in this directory are automatically loaded.
final: prev: {
  # waypoint = final.callPackage ../pkgs/waypoint.nix { };
  terraform = final.callPackage ../pkgs/terraform-bin.nix { };
}
