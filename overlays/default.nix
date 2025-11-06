# This contains various packages we want to overlay. Note that the
# other ".nix" files in this directory are automatically loaded.
final: prev:
let
  # Override function to add our custom prl-tools to any kernel package set
  extendKernelPackages = kpkgs: kpkgs.extend (lpfinal: lpprev: {
    prl-tools = lpfinal.callPackage ../pkgs/parallels-tools { };
  });
in
{
  # waypoint = final.callPackage ../pkgs/waypoint.nix { };
  terraform = final.callPackage ../pkgs/terraform-bin.nix { };
  opencode = final.callPackage ../pkgs/opencode-fhs.nix { };
  firefox-hidpi = (import ./firefox-hidpi.nix final prev).firefox-hidpi;

  # Override all kernel package sets to use our custom prl-tools
  linuxPackages = extendKernelPackages prev.linuxPackages;
  linuxPackages_6_6 = extendKernelPackages prev.linuxPackages_6_6;
  linuxPackages_6_1 = extendKernelPackages prev.linuxPackages_6_1;
  linuxPackages_latest = extendKernelPackages prev.linuxPackages_latest;
}
