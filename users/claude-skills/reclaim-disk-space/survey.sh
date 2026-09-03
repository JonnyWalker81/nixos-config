#!/usr/bin/env bash
# Disk space survey — MEASURES ONLY, DELETES NOTHING.
#
# Run with plain `bash survey.sh` (not the user's interactive shell — zsh rc hooks
# pollute loop output). Reports reclaimable space ranked by size, with the recovery
# cost of each item, so groups can be proposed for approval.
#
#   bash survey.sh          # fast survey (~30s), skips the slow /nix/store du
#   bash survey.sh --deep   # adds full /nix/store measurement (1-3 min)

set -u
DEEP=0
[ "${1:-}" = "--deep" ] && DEEP=1

H="${HOME:-/home/$(id -un)}"
hr() { printf '\n\033[1m=== %s ===\033[0m\n' "$1"; }
# sudo that never prompts; prints nothing if unavailable
s() { sudo -n "$@" 2>/dev/null; }
have() { command -v "$1" >/dev/null 2>&1; }
# du that tolerates missing paths and never crosses filesystems
d() { du -shx -- "$@" 2>/dev/null | sort -rh; }

hr "FILESYSTEM"
df -h / | tail -1
FREE_BEFORE=$(df -k / | awk 'NR==2{print $4}')
echo "free_kb=$FREE_BEFORE"

# ---------------------------------------------------------------- nix
hr "NIX STORE"
if [ -d /nix/store ]; then
  # --closure-size only (never also -S: same flag, and -S is the one the skill's
  # gotcha table warns about summing). One path here, so the number is exact.
  live=$(nix path-info --closure-size /nix/var/nix/profiles/system 2>/dev/null | awk '{printf "%.1f", $NF/1073741824}')
  echo "current system closure (live, cannot be freed): ${live:-?} GB"
  b=$(readlink -f /run/booted-system 2>/dev/null); c=$(readlink -f /run/current-system 2>/dev/null)
  if [ -n "$b" ] && [ "$b" != "$c" ]; then
    echo "  !! booted != current (rebuild without reboot):"
    echo "     booted:  $b"
    echo "     current: $c"
    echo "     Use --delete-older-than, NOT -d: -d drops the booted generation's"
    echo "     boot entry, costing the ability to boot back into the running config."
  fi
  gens=$(s nix-env --list-generations -p /nix/var/nix/profiles/system | wc -l)
  echo "system generations: ${gens:-?}  (each old one pins its own closure)"
  s nix-env --list-generations -p /nix/var/nix/profiles/system | head -3
  echo "  ..."
  s nix-env --list-generations -p /nix/var/nix/profiles/system | tail -2
  if [ "$DEEP" = 1 ]; then
    echo "store on disk: $(du -shx /nix/store 2>/dev/null | cut -f1)  <- compare to live closure above"
  else
    echo "store on disk: (re-run with --deep; takes 1-3 min)"
  fi

  # direnv / manual gcroots pin dev-shell closures and BLOCK the GC.
  # These must be cleared BEFORE garbage collection or the GC reclaims far less.
  n=$(ls -1 /nix/var/nix/gcroots/auto 2>/dev/null | wc -l)
  echo "auto gcroots: $n"
  if [ "$n" -gt 0 ]; then
    echo "  repos pinning dev shells (count of roots each):"
    for l in /nix/var/nix/gcroots/auto/*; do readlink "$l" 2>/dev/null; done \
      | grep -oE '/[^/]+/[^/]+/(Repositories|src|code|projects|work)/[^/]+' \
      | sort | uniq -c | sort -rn | head -15 | sed 's/^/    /'
    echo "  NOTE: 'keep-outputs'/'keep-derivations' in nix.conf make each pinned"
    echo "        shell hold its whole BUILD closure. Check with:"
    echo "        nix config show | grep -E 'keep-(outputs|derivations)'"
  fi
fi

# ---------------------------------------------------------------- home
hr "HOME TOP-LEVEL (top 15)"
d "$H"/* "$H"/.[a-z]* 2>/dev/null | head -15

hr "BUILD ARTIFACTS (regenerable — recovery = rebuild)"
for root in "$H/Repositories" "$H/src" "$H/code" "$H/projects" "$H/work"; do
  [ -d "$root" ] || continue
  # Rust target/ dirs: only real ones (must contain CACHEDIR.TAG or debug/release)
  find "$root" -maxdepth 3 -type d -name target -prune 2>/dev/null | while read -r t; do
    [ -e "$t/CACHEDIR.TAG" ] || [ -d "$t/debug" ] || [ -d "$t/release" ] || continue
    printf "  %8s  rust    %s\n" "$(du -shx "$t" 2>/dev/null | cut -f1)" "$t"
  done
  find "$root" -maxdepth 3 -type d -name .terraform -prune 2>/dev/null | while read -r t; do
    printf "  %8s  tf      %s\n" "$(du -shx "$t" 2>/dev/null | cut -f1)" "$t"
  done
  find "$root" -maxdepth 3 -type d -name node_modules -prune 2>/dev/null | while read -r t; do
    printf "  %8s  node    %s\n" "$(du -shx "$t" 2>/dev/null | cut -f1)" "$t"
  done
done | sort -rh | head -20
echo "  (for each, check activity before proposing: git -C <repo> log -1 --format=%cr)"

hr "PACKAGE / TOOL CACHES (regenerable — recovery = re-download)"
GOMOD=$(go env GOMODCACHE 2>/dev/null)
for p in \
  "${GOMOD:-$H/go/pkg/mod}" "$H/pkg/mod" "$H/.cache/go-build" "$H/.cache/goimports" \
  "$H/.npm/_cacache" "$H/.npm/_npx" "$H/.cache/pnpm" "$H/.local/share/pnpm" \
  "$H/.local/share/yarn" "$H/.cache/yarn" "$H/.cargo/registry/cache" \
  "$H/.cache/nix/tarball-cache" "$H/.cache/mozilla" "$H/.cache/chromium" \
  "$H/.cache/uv" "$H/.cache/pip" "$H/.gradle/caches" "$H/.m2/repository" \
  "$H/.cache/bazel" "$H/.cache/ms-playwright" "$H/.cache/Cypress" \
  "$H/.rustup/toolchains" "$H/.local/share/Trash/files" ; do
  [ -e "$p" ] && printf "  %8s  %s\n" "$(du -shx "$p" 2>/dev/null | cut -f1)" "$p"
done | sort -rh
echo "  GOMODCACHE=${GOMOD:-<go not on PATH>}  <- may be \$HOME/pkg/mod, not ~/go"
have rustup && echo "  rust default toolchain: $(rustup default 2>/dev/null)"

hr "LARGE LOOSE FILES (>200M in Downloads/Desktop/tmp dirs)"
for dir in "$H/Downloads" "$H/Desktop" "$H/tmp" "$H/scratch"; do
  [ -d "$dir" ] || continue
  find "$dir" -maxdepth 1 -type f -size +200M -printf '  %10s  %TY-%Tm-%Td  %p\n' 2>/dev/null
done | sort -rn | head -20

# ---------------------------------------------------------------- docker
hr "DOCKER"
if have docker && docker info >/dev/null 2>&1; then
  docker system df
  echo
  echo "  builders (buildx prune only affects the SELECTED '*' one!):"
  docker buildx ls 2>/dev/null | sed 's/^/    /'
  echo
  # 'docker system df' Build Cache covers ONLY the docker-driver builder, so a
  # container-driver builder's cache is invisible there. Measure each directly.
  echo "  per-builder cache (docker system df CANNOT see container-driver builders):"
  docker buildx ls 2>/dev/null \
    | awk 'NR>1 && $1 !~ /^\\_/ && NF{sel=($1 ~ /\*$/)?"*":" "; gsub(/\*$/,"",$1); print $1, sel}' \
    | while read -r bldr sel; do
        printf "    %-1s %-14s %s\n" "$sel" "$bldr" \
          "$(docker buildx du --builder "$bldr" 2>/dev/null | tail -2 | tr '\n' ' ')"
      done
  echo
  echo "  volume sizes:"
  s sh -c 'du -shx /var/lib/docker/volumes/* 2>/dev/null | sort -rh | head -10' | sed 's/^/    /'
  echo "  ^ buildx_buildkit_*_state = pure build cache, safe."
  echo "    ANY OTHER named volume may hold real local dev database data."
  echo
  echo "  stopped containers:"
  docker ps -a --filter status=exited --format '    {{.Names}} ({{.Status}})' | head -10
else
  echo "docker not available"
fi

# ---------------------------------------------------------------- system
hr "SYSTEM"
have journalctl && echo "journal: $(journalctl --disk-usage 2>/dev/null | grep -oE '[0-9.]+[KMG]' | tail -1)"
s sh -c 'du -shx /var/lib/* 2>/dev/null | sort -rh | head -5' | sed 's/^/  /'
[ -d /boot/loader/entries ] && echo "boot entries: $(ls -1 /boot/loader/entries | wc -l)"

hr "DONE — nothing was deleted"
echo "Next: group these by recovery cost, then get approval for EACH group separately."
echo "Order matters: clear gcroots BEFORE running the nix GC."
