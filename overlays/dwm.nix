# Custom DWM build from github.com/JonnyWalker81/dwm (xmonad-parity branch)
# This overrides the nixpkgs dwm package so that
# services.xserver.windowManager.dwm.enable uses our patched version.
#
# After pushing changes to the DWM repo, update the rev and hash below,
# then rebuild NixOS.
final: prev: {
  dwm = prev.dwm.overrideAttrs (old: {
    src = prev.fetchFromGitHub {
      owner = "JonnyWalker81";
      repo = "dwm";
      rev = "552206f5086a6f25fc5bbbfb3e5abb49484dba27";
      sha256 = "sha256-cDqnUHQbxjLNkD2VI/kTXsRNUKfolpuL/h9knO/ocDk=";
    };

    # config.mk uses pkg-config for NixOS compatibility
    nativeBuildInputs = (old.nativeBuildInputs or [ ]) ++ [ prev.pkg-config ];

    buildInputs = (old.buildInputs or [ ]) ++ [
      prev.xorg.libX11
      prev.xorg.libXft
      prev.xorg.libXinerama
      prev.freetype
      prev.fontconfig
    ];
  });
}
