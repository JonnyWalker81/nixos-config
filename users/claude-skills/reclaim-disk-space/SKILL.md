---
name: reclaim-disk-space
description: Use when a filesystem is low or out of space — disk at 90%+ or 100%, "No space left on device", builds or nixos-rebuild failing for lack of space, or a request to free up / clean up / reclaim disk space.
---

# Reclaim Disk Space

Free disk space by measuring first, then deleting in approval-gated groups. Every byte
proposed for deletion is measured, classified by recovery cost, and deleted only after the
user approves that specific group.

## The Iron Rule

**No deletion without explicit approval of that specific group.**

Approval of one group is never consent for another. Not for "obviously safe caches", not for
"while I'm in here", not for a group the user approved a variant of earlier. If the user
approved a 3-item group and you find a 4th item mid-execution, that 4th item needs its own ask.

## Workflow

1. **Survey.** Run `bash survey.sh` (in this skill's directory; `--deep` adds the slow
   `/nix/store` measurement). It measures only and deletes nothing.
2. **Classify** every candidate by recovery cost — this is what the user is actually deciding:
   - *Free* — pure cache, regenerates silently (build caches, thumbnails, buildkit cache)
   - *Cheap* — re-download or rebuild on next use (module caches, `target/`, `.terraform/`)
   - *Expensive* — long rebuild or manual re-fetch (large monorepo builds, toolchains)
   - **Never** — see the never-delete list below
3. **Check activity before proposing** anything in a repo: `git -C <repo> log -1 --format=%cr`
   and the directory mtime. Propose dormant repos' artifacts; flag active ones as a cost.
4. **Propose groups**, largest first, each with its measured size and recovery command.
5. **Ask per group** via AskUserQuestion. Offer a narrower middle option, not just yes/no.
6. **Execute** the approved group, then re-check `df -h /` so the running total stays visible.
7. **Verify** (checklist below) and report the real numbers, including anything that failed.

Stop and report as soon as the user has enough headroom — don't run the remaining groups just
because they were planned.

## Ordering Rule

**Unpin before collecting.** On Nix systems, clear dormant `.direnv` dirs and stale `result`
symlinks *before* running the GC. Those gcroots pin whole dev-shell closures; collecting first
reclaims a fraction of what's available and can't be redone without a second full GC pass.

Unpinning is **itself a deletion group and needs its own approval** — it is not free prep, and
the Iron Rule applies to it unchanged. `.direnv` in an *active* repo costs a full `nix develop`
re-evaluation and re-download, so propose dormant ones and price the active ones.

## Gotchas That Cost Real Time

| Trap | Reality |
|---|---|
| `nix path-info -S` to sum sizes | `-S` is *closure* size per path. Summing it across a closure double-counts wildly — measure your own overcount ratio, don't trust a remembered figure. Dedupe with `nix path-info -r`, then sum `narSize` from `--json`. |
| `nix-collect-garbage` as user | Only touches the user profile, and reports ~nothing while old *system* generations pin everything. The system profile needs `sudo`. Run both — they free different paths. |
| `nix-collect-garbage --dry-run` | Not reliably read-only: it can delete profile generations before dry-running the collection. Never use it as a "safe preview". |
| Weekly `nix.gc` freeing almost nothing | Check `keep-outputs` / `keep-derivations` in `nix config show`. With gcroots present they hold entire build closures alive, so a scheduled GC frees ~1 GB while a manual pass after unpinning frees tens of GB. |
| `docker system df` Build Cache line | Reports **only the `docker`-driver builder**. A `docker-container` builder — often the `*` row in `docker buildx ls` — keeps its cache inside its own `buildx_buildkit_<name>0_state` volume, which `system df` counts as an ACTIVE *volume* with 0B reclaimable, never under Build Cache. A `0B` Build Cache is no evidence the selected builder is empty. Measure per builder: `docker buildx du --builder <name>`. |
| `docker buildx prune` | Scoped to the **selected** builder only — name every builder you intend to prune. The `buildx_buildkit_*_state` volume survives `buildx prune` *and* `volume prune`; only `docker buildx rm <name>` reclaims it. |
| `docker system prune --volumes` | Destroys named volumes holding real local dev databases. Use `container prune` + `image prune` + `buildx prune`/`rm` instead. |
| A cache-clean that reports success | `npm`/`pnpm`/`go` are often absent from a non-interactive shell's PATH, so the command fails while the script prints success. Verify the size actually dropped, or delete `~/.npm/_cacache` directly. |
| Multi-step logic in the user's shell | Interactive rc files (zoxide, git prompts, aliases) corrupt loop output and swallow exit codes — write a script, run it with plain `bash`. Same for `du -sh /*`: minutes long, competes with the GC for IO. Measure targeted paths and kill strays. |
| A repo's own `cleanup-*.sh` | Read it before running. These commonly bundle `docker system prune --volumes`, an unconditional `nix-collect-garbage -d`, or a `rm -rf` of a whole package registry. Reuse the safe steps individually. |

## Never Delete Without Naming It Separately

- Named Docker volumes other than `buildx_buildkit_*_state` — assume local DB data
- `*.tfstate*`, `*.tfvars`, `.terraform.lock.hcl` (only `.terraform/` itself is cache)
- Anything tracked by git, or untracked-but-uncommitted work
- Session/history stores that look like caches but aren't (agent `storage/`, `*.db`, `snapshot/`)
- SSH/GPG keys, credential and token stores
- **Both** `/run/current-system` and `/run/booted-system`. They differ after any
  rebuild-without-reboot (check `readlink -f` on each). The booted closure itself is held by the
  permanent `/nix/var/nix/gcroots/booted-system` root, so a GC will not break the running
  system — but `nix-collect-garbage -d` still deletes its *profile generation link*, which drops
  it from the boot menu and costs you the ability to boot back into the config you are running.
  Prefer `--delete-older-than`; defer `-d` until booted == current.

## Verify Before Claiming Success

- `df -h /` — the real delta, not the predicted one
- Tracked files untouched: `git -C <repo> status --short` in every repo touched
- Nix: `nix path-info -r /nix/var/nix/profiles/system` resolves; `sudo nix-store --verify`
- Boot entries still have kernels; note any now-orphaned generations in the menu
- Docker: the dev volumes you promised to keep are still listed
- Config caches: state/lock files still present and counted

Report what *didn't* work too. A step whose command failed silently is a step that didn't run.

## Common Mistakes

- Predicting the reclaim instead of measuring it after the fact
- Bundling groups to "save round trips" — the per-group ask is the whole point
- Deleting artifacts of an actively developed repo without flagging the rebuild cost
- Treating a directory as cache because of its name (`.local/share/<tool>` is often real state)
- Continuing to delete after the user already has enough space
