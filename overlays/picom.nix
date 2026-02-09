# Picom compositor from yshui's GitHub (latest version)
final: prev: {
  picom = prev.picom.overrideAttrs (oldAttrs: rec {
    pname = "picom";
    version = "unstable-latest";
    src = prev.fetchFromGitHub {
      owner = "yshui";
      repo = "picom";
      rev = "b700a37d56ab5debdbb78be7a6b905e72f69ff2d";
      sha256 = "sha256-C+icJXTkE+XMaU7N6JupsP8xhmRVggX9hY1P7za0pO0=";
    };
    buildInputs = (oldAttrs.buildInputs or [ ])
      ++ [ prev.pcre prev.libconfig prev.libev prev.uthash ];
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ])
      ++ [ prev.asciidoc prev.pkg-config prev.meson prev.ninja ];
    doCheck = false;
    doInstallCheck = false;
  });
}
