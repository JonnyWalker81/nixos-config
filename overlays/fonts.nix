# DankMono font derivation
# This overlay requires flake inputs, so it must be explicitly imported.
{ inputs }:

final: prev: {
  dankmono = prev.stdenv.mkDerivation rec {
    pname = "dankmono";
    version = "1.0.0";

    src = inputs.dankmono;

    installPhase = ''
      runHook preInstall

      # Install TrueType fonts
      install -D -m644 -t $out/share/fonts/truetype/dankmono OpenType-TT/*.ttf

      # Install OpenType fonts
      install -D -m644 -t $out/share/fonts/opentype/dankmono OpenType-PS/*.otf

      # Install web fonts
      install -D -m644 -t $out/share/fonts/woff2/dankmono Web-PS/*.woff2
      install -D -m644 Web-PS/dmvendor.css $out/share/fonts/woff2/dankmono/dmvendor.css

      # Install documentation
      install -D -m644 README.txt $out/share/doc/dankmono/README.txt
      install -D -m644 EULA.txt $out/share/licenses/dankmono/EULA.txt

      runHook postInstall
    '';

    # Add font configuration for fontconfig (Linux)
    postInstall = prev.lib.optionalString prev.stdenv.isLinux ''
      # Generate fontconfig cache for Linux
      ${prev.fontconfig}/bin/fc-cache -f $out/share/fonts/
    '';

    meta = with prev.lib; {
      description = "DankMono programming font";
      longDescription = ''
        Dank Mono is a monospaced font designed for coding with
        ligatures and a distinctive style.
      '';
      license = licenses.unfree;
      platforms = platforms.all;
    };
  };
}
