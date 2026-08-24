# Core package overlays -- terraform and parallels tools kernel extensions.
# Other overlays in this directory are auto-discovered by convention.
final: prev:
let
  # Override function to add our custom prl-tools to any kernel package set
  extendKernelPackages =
    kpkgs:
    kpkgs.extend (
      lpfinal: lpprev: {
        prl-tools = lpfinal.callPackage ../pkgs/parallels-tools { };
      }
    );
in
{
  terraform = final.callPackage ../pkgs/terraform-bin.nix { };
  pi-coding-agent = final.callPackage ../pkgs/pi-coding-agent.nix { };

  # Omarchy-style DWM/X11 helper scripts (rofi-driven command menu + cheatsheet)
  omarchy-menu = final.callPackage ../pkgs/omarchy-menu.nix { };
  omarchy-keys = final.callPackage ../pkgs/omarchy-keys.nix { };

  # Override all kernel package sets to use our custom prl-tools
  linuxPackages = extendKernelPackages prev.linuxPackages;
  linuxPackages_6_6 = extendKernelPackages prev.linuxPackages_6_6;
  linuxPackages_6_1 = extendKernelPackages prev.linuxPackages_6_1;
  linuxPackages_latest = extendKernelPackages prev.linuxPackages_latest;
}
