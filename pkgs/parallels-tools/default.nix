# Credit: https://github.com/wegank/nixos-config/tree/main/hardware/parallels-unfree
{
  stdenv,
  lib,
  makeWrapper,
  p7zip,
  gawk,
  util-linux,
  xorg,
  glib,
  dbus-glib,
  zlib,
  kernel,
  libsOnly ? false,
  fetchurl,
  undmg,
  perl,
  autoPatchelfHook,
  gconf,
}:

stdenv.mkDerivation rec {
  # version = "18.3.1-53614";
  version = "20.4.0-55980";
  pname = "prl-tools";

  # We download the full distribution to extract prl-tools-lin.iso from
  # => ${dmg}/Parallels\ Desktop.app/Contents/Resources/Tools/prl-tools-lin.iso
  src = fetchurl {
    url = "https://download.parallels.com/desktop/v${lib.versions.major version}/${version}/ParallelsDesktop-${version}.dmg";
    sha256 = "1xl9v0s8mkhysxl7bh1s4xxj18ws1sr1mmv363cd9jzr9a063k86";
  };

  # patches = [ ./prl-tools-6.1.patch ];

  hardeningDisable = [
    "pic"
    "format"
  ];

  nativeBuildInputs =
    [
      p7zip
      undmg
      perl
      autoPatchelfHook
    ]
    ++ lib.optionals (!libsOnly) [ makeWrapper ]
    ++ kernel.moduleBuildDependencies;

  buildInputs =
    with xorg;
    [
      libXrandr
      libXext
      libX11
      libXcomposite
      libXinerama
    ]
    ++ lib.optionals (!libsOnly) [
      libXi
      glib
      dbus-glib
      zlib
      gconf
    ];

  runtimeDependencies = [
    glib
    xorg.libXrandr
    gconf
  ];

  inherit libsOnly;

  unpackPhase = ''
    undmg "${src}"
    export sourceRoot=prl-tools-build
    7z x "Parallels Desktop.app/Contents/Resources/Tools/prl-tools-lin${lib.optionalString stdenv.isAarch64 "-arm"}.iso" -o$sourceRoot
    if test -z "$libsOnly"; then
      ( cd $sourceRoot/kmods; tar -xaf prl_mod.tar.gz )
    fi
  '';

  kernelVersion = lib.optionalString (!libsOnly) kernel.modDirVersion;
  kernelDir = lib.optionalString (!libsOnly) "${kernel.dev}/lib/modules/${kernelVersion}";
  scriptPath = lib.concatStringsSep ":" (
    lib.optionals (!libsOnly) [
      "${util-linux}/bin"
      "${gawk}/bin"
    ]
  );

  buildPhase = ''
    if test -z "$libsOnly"; then
      ( # kernel modules
        cd kmods
        make -f Makefile.kmods \
          KSRC=$kernelDir/source \
          HEADERS_CHECK_DIR=$kernelDir/source \
          KERNEL_DIR=$kernelDir/build \
          SRC=$kernelDir/build \
          KVER=$kernelVersion
      )
    fi
  '';

  installPhase = ''
    if test -z "$libsOnly"; then
      ( # kernel modules
        cd kmods
        mkdir -p $out/lib/modules/${kernelVersion}/extra
        cp prl_fs/SharedFolders/Guest/Linux/prl_fs/prl_fs.ko $out/lib/modules/${kernelVersion}/extra
        cp prl_fs_freeze/Snapshot/Guest/Linux/prl_freeze/prl_fs_freeze.ko $out/lib/modules/${kernelVersion}/extra
        cp prl_tg/Toolgate/Guest/Linux/prl_tg/prl_tg.ko $out/lib/modules/${kernelVersion}/extra
        ${lib.optionalString stdenv.isAarch64 "cp prl_notifier/Installation/lnx/prl_notifier/prl_notifier.ko $out/lib/modules/${kernelVersion}/extra"}
      )
    fi
    ( # tools
      cd tools/tools${
        if stdenv.isAarch64 then
          "-arm64"
        else if stdenv.isx86_64 then
          "64"
        else
          "32"
      }
      mkdir -p $out/lib
      if test -z "$libsOnly"; then
        # install binaries
        for i in bin/* sbin/prl_nettool sbin/prl_snapshot; do
          install -Dm755 $i $out/$i
        done
        mkdir -p $out/bin
        install -Dm755 ../../tools/prlfsmountd.sh $out/sbin/prlfsmountd
        wrapProgram $out/sbin/prlfsmountd \
          --prefix PATH ':' "$scriptPath"
        for i in lib/libPrl*.0.0; do
          cp $i $out/lib
          ln -s $out/$i $out/''${i%.0.0}
        done
        mkdir -p $out/share/man/man8
        install -Dm644 ../mount.prl_fs.8 $out/share/man/man8
        mkdir -p $out/etc/pm/sleep.d
        install -Dm644 ../99prltoolsd-hibernate $out/etc/pm/sleep.d
      fi
    )
  '';

  meta = with lib; {
    description = "Parallels Tools for Linux guests";
    homepage = "https://parallels.com";
    platforms = [
      "aarch64-linux"
      "i686-linux"
      "x86_64-linux"
    ];
    license = licenses.unfree;
  };
}

# Credit: https://github.com/wegank/nixos-config/tree/main/hardware/parallels-unfree
# {
#   stdenv,
#   lib,
#   makeWrapper,
#   p7zip,
#   gawk,
#   util-linux,
#   xorg,
#   glib,
#   dbus-glib,
#   zlib,
#   kernel,
#   libsOnly ? false,
#   fetchurl,
#   undmg,
#   perl,
#   autoPatchelfHook,
# }:

# stdenv.mkDerivation rec {
#   # version = "18.3.1-53614";
#   # version = "19.1.1-54734";
#   # version = "19.3.0-54924";
#   # version = "19.4.0-54962";
#   # version = "19.4.1-54985";
#   # version = "20.0.1-55659";
#   # version = "20.1.2-55742";
#   version = "20.1.3-55743";
#   pname = "prl-tools";

#   # We download the full distribution to extract prl-tools-lin.iso from
#   # => ${dmg}/Parallels\ Desktop.app/Contents/Resources/Tools/prl-tools-lin.iso
#   src = fetchurl {
#     url = "https://download.parallels.com/desktop/v${lib.versions.major version}/${version}/ParallelsDesktop-${version}.dmg";
#     # sha256 = "sha256-MZtNxByY2GSoPFeH9mPieCPPNfUgfla+lYgpeD+SgOc=";
#     # sha256 = "sha256-02YxBkV9pZGfXuK6GvUDTgE9U5H2MOMk24h9qGJdFTM=";
#     # sha256 = "sha256-Hj1manQSZHiht6mmWes44RVk2Bdqp6QdNCdK322bzWc=";
#     # sha256 = "sha256-c/MrWUvwY/Z38uOBbetJSVkZlwkdzFhw6wpk1L0BuQs=";
#     # sha256 = "sha256-VBHCsxaMI6mfmc/iQ4hJW/592rKck9HilTX2Hq7Hb5s=";
#     # sha256 = "sha256-R7pQhmLpMOHExPwH4YM3WDnp1PcwpH5Bif3C1/N55Bg=";
#     # sha256 = "5h8WZB7L6D9KOgIPSstN1sNcf3FZQiOQFB5MUC4YzvA=";
#     sha256 = "sha256-5lbTTQucop/jnsVudoqTO9bESR5tdn8NFu9Nm2WphU4=";
#   };

#   patches = [ ./prl-tools-6.1.patch ];

#   hardeningDisable = [
#     "pic"
#     "format"
#   ];

#   nativeBuildInputs = [
#     p7zip
#     undmg
#     perl
#     autoPatchelfHook
#   ] ++ lib.optionals (!libsOnly) [ makeWrapper ] ++ kernel.moduleBuildDependencies;

#   buildInputs =
#     with xorg;
#     [
#       libXrandr
#       libXext
#       libX11
#       libXcomposite
#       libXinerama
#     ]
#     ++ lib.optionals (!libsOnly) [
#       libXi
#       glib
#       dbus-glib
#       zlib
#     ];

#   runtimeDependencies = [
#     glib
#     xorg.libXrandr
#   ];

#   inherit libsOnly;

#   unpackPhase = ''
#     undmg "${src}"
#     export sourceRoot=prl-tools-build
#     7z x "Parallels Desktop.app/Contents/Resources/Tools/prl-tools-lin${lib.optionalString stdenv.isAarch64 "-arm"}.iso" -o$sourceRoot
#     if test -z "$libsOnly"; then
#       ( cd $sourceRoot/kmods; tar -xaf prl_mod.tar.gz )
#     fi
#   '';

#   kernelVersion = lib.optionalString (!libsOnly) kernel.modDirVersion;
#   kernelDir = lib.optionalString (!libsOnly) "${kernel.dev}/lib/modules/${kernelVersion}";
#   scriptPath = lib.concatStringsSep ":" (
#     lib.optionals (!libsOnly) [
#       "${util-linux}/bin"
#       "${gawk}/bin"
#     ]
#   );

#   buildPhase = ''
#     if test -z "$libsOnly"; then
#       ( # kernel modules
#         cd kmods
#         make -f Makefile.kmods \
#           KSRC=$kernelDir/source \
#           HEADERS_CHECK_DIR=$kernelDir/source \
#           KERNEL_DIR=$kernelDir/build \
#           SRC=$kernelDir/build \
#           KVER=$kernelVersion
#       )
#     fi
#   '';

#   installPhase = ''
#     if test -z "$libsOnly"; then
#       ( # kernel modules
#         cd kmods
#         mkdir -p $out/lib/modules/${kernelVersion}/extra
#         cp prl_fs/SharedFolders/Guest/Linux/prl_fs/prl_fs.ko $out/lib/modules/${kernelVersion}/extra
#         cp prl_fs_freeze/Snapshot/Guest/Linux/prl_freeze/prl_fs_freeze.ko $out/lib/modules/${kernelVersion}/extra
#         cp prl_tg/Toolgate/Guest/Linux/prl_tg/prl_tg.ko $out/lib/modules/${kernelVersion}/extra
#         ${lib.optionalString stdenv.isAarch64 "cp prl_notifier/Installation/lnx/prl_notifier/prl_notifier.ko $out/lib/modules/${kernelVersion}/extra"}
#       )
#     fi
#     ( # tools
#       cd tools/tools${
#         if stdenv.isAarch64 then
#           "-arm64"
#         else if stdenv.isx86_64 then
#           "64"
#         else
#           "32"
#       }
#       mkdir -p $out/lib
#       if test -z "$libsOnly"; then
#         # install binaries
#         for i in bin/* sbin/prl_nettool sbin/prl_snapshot; do
#           install -Dm755 $i $out/$i
#         done
#         mkdir -p $out/bin
#         install -Dm755 ../../tools/prlfsmountd.sh $out/sbin/prlfsmountd
#         wrapProgram $out/sbin/prlfsmountd \
#           --prefix PATH ':' "$scriptPath"
#         for i in lib/libPrl*.0.0; do
#           cp $i $out/lib
#           ln -s $out/$i $out/''${i%.0.0}
#         done
#         mkdir -p $out/share/man/man8
#         install -Dm644 ../mount.prl_fs.8 $out/share/man/man8
#         mkdir -p $out/etc/pm/sleep.d
#         install -Dm644 ../99prltoolsd-hibernate $out/etc/pm/sleep.d
#       fi
#     )
#   '';

#   meta = with lib; {
#     description = "Parallels Tools for Linux guests";
#     homepage = "https://parallels.com";
#     platforms = [
#       "aarch64-linux"
#       "i686-linux"
#       "x86_64-linux"
#     ];
#     license = licenses.unfree;
#   };
# }
