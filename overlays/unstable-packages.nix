# Packages pulled from unstable nixpkgs and ghostty platform-conditional build
# This overlay requires flake inputs, so it must be explicitly imported.
{ inputs }:

final: prev:
let
  unstablePkgs = import inputs.nixpkgs-unstable {
    system = prev.stdenv.hostPlatform.system;
    config.allowUnfree = true;
  };
in {
  # Expose the full unstable package set for downstream use
  unstable = unstablePkgs;

  # Specific packages from unstable
  kitty = unstablePkgs.kitty;
  xmobar = unstablePkgs.xmobar;
  awscli2 = unstablePkgs.awscli2;

  # Stable 25.05 still ships the legacy TypeScript codex (0.1.x); use unstable
  # for the current Rust rewrite.
  codex = unstablePkgs.codex;

  # opencode from unstable rather than the upstream flake: the flake's
  # node_modules derivation runs `bun install --frozen-lockfile` against the live
  # npm registry, which now fails for any pinned tag once resolution drifts.
  # Wrap prettier onto PATH so opencode's built-in formatter resolves at runtime.
  opencode = prev.symlinkJoin {
    name = "opencode-${unstablePkgs.opencode.version}";
    paths = [ unstablePkgs.opencode ];
    nativeBuildInputs = [ prev.makeWrapper ];
    postBuild = ''
      wrapProgram $out/bin/opencode \
        --prefix PATH : ${prev.lib.makeBinPath [ prev.prettier ]}
    '';
    inherit (unstablePkgs.opencode) meta;
  };

  # Use the flake overlay for Linux (optimized build), ghostty-bin from unstable for macOS
  ghostty = if prev.stdenv.hostPlatform.isLinux then
    inputs.ghostty.packages.${prev.stdenv.hostPlatform.system}.default
  else
    unstablePkgs.ghostty-bin;
}
