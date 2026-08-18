# Disable GNU mailutils in the emacs build on aarch64-darwin.
#
# nixpkgs' emacs builder enables `--with-mailutils` by default (withMailutils
# defaults to true on every platform). On aarch64-darwin, mailutils 3.21 fails
# to link:
#
#   ld: symbol(s) not found for architecture arm64
#   make[4]: *** [Makefile:1288: vacation.la] Error 1   (libmu_sieve/extensions)
#
# which takes the whole emacs -> system-path -> darwin-system build down with it.
# With mailutils disabled, emacs falls back to its bundled `movemail`; the only
# feature lost is mailutils' extra POP3/IMAP movemail support. Guarded to Darwin
# so Linux (where mailutils builds fine) is unaffected.
final: prev:
prev.lib.optionalAttrs prev.stdenv.hostPlatform.isDarwin {
  emacs-unstable = prev.emacs-unstable.override { withMailutils = false; };
}
