# Codebase Concerns

**Analysis Date:** 2026-02-24

## Tech Debt

**Hardcoded User Paths Throughout Shell Config:**
- Issue: Shell aliases, systemd services, and Hyprland config reference absolute paths like `/home/cipher/nixos-config/scripts/...` instead of using Nix store paths or relative derivation references. This breaks for any user other than `cipher` and assumes the repo is checked out at that exact path.
- Files:
  - `users/common/shell.nix` (lines 49-71, 100-120): ~20 display-profile aliases all hardcode `/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh`
  - `users/common/shell.nix` (line 186): `source /home/cipher/.opam/opam-init/init.zsh` hardcodes cipher's home
  - `users/cipher/home-manager.nix` (line 89): wallpaper service hardcodes `/home/cipher/nixos-config/scripts/setup-wallpaper.sh`
  - `users/hyprland.nix` (lines 194, 224, 243): Hyprland exec-once and keybindings hardcode `/home/cipher/nixos-config/scripts/...`
  - `modules/display-x11.nix` (lines 111, 119, 159): References `../scripts/display-profiles/display-switcher.sh` via Nix path interpolation (correct) but the script itself is also aliased with hardcoded paths
- Impact: The `jrothberg` and `phantom` users share the same `common/shell.nix` — all display-profile aliases point to `/home/cipher/...` which won't work for them. WSL or alternative checkout paths also break.
- Fix approach: Convert script references to Nix store paths by wrapping them with `pkgs.writeShellScriptBin` or using `${../scripts/...}` Nix path interpolation. Replace `/home/cipher` with `${config.home.homeDirectory}` or `$HOME`.

**Duplicate Shell Alias Definitions:**
- Issue: Display profile aliases (`dp`, `dp-hidpi`, `dp-retina`, etc.) and `prl-display` are defined identically in both `home.shellAliases` and `programs.zsh.shellAliases` within the same file.
- Files: `users/common/shell.nix` (lines 47-67 and lines 99-117)
- Impact: Redundant definitions that must be kept in sync. Changes to one set but not the other would create confusing behavior.
- Fix approach: Define display-profile aliases only in `home.shellAliases` (which applies to all shells) and remove the duplicate `programs.zsh.shellAliases` entries. Or vice versa.

**Repeated `allowUnfree`/`allowUnsupportedSystem` Declarations:**
- Issue: `nixpkgs.config.allowUnfree = true` and `nixpkgs.config.allowUnsupportedSystem = true` are set in `lib/mksystem.nix` globally, then redundantly repeated in 6+ other files.
- Files:
  - `lib/mksystem.nix` (line 54): Sets `allowUnfree` globally for all configs
  - `machines/vm-shared.nix` (lines 35-36): Redundant
  - `machines/vm-aarch64-prl.nix` (lines 235-236): Redundant
  - `machines/vm-aarch64.nix` (lines 13-14): Redundant
  - `users/cipher/darwin.nix` (lines 78, 80): Redundant
  - `users/phantom/darwin.nix` (lines 78-79): Redundant
  - `users/jrothberg/darwin.nix` (lines 22-23): Redundant
  - `overlays/unstable-packages.nix` (line 9): Necessary (separate nixpkgs import)
- Impact: Confusing for maintainers — unclear where the authoritative setting lives. No functional harm but makes the intent unclear.
- Fix approach: Remove all redundant declarations. Keep only `lib/mksystem.nix` for NixOS and Darwin system configs, and keep the `overlays/unstable-packages.nix` one (separate import).

**Four Window Managers Enabled Simultaneously:**
- Issue: All four window managers (XMonad, DWM, AwesomeWM, Hyprland) are enabled in `vm-shared.nix` even though only DWM is set as the default session. This significantly increases build time and closure size.
- Files:
  - `machines/vm-shared.nix` (lines 88-91): All four `.enable = true`
  - `machines/vm-shared.nix` (line 96): `defaultSession = "none+dwm"` — only DWM is actually used
  - `modules/desktop/xmonad.nix`: Pulls in Haskell packages (xmonad-contrib, xmonad-extras)
  - `modules/desktop/awesome.nix`: Pulls in Lua packages (luarocks, luadbi-mysql)
  - `modules/desktop/hyprland.nix`: Pulls Hyprland from flake input
- Impact: Slower rebuilds, larger system closure. The comment "All four enabled for backward compatibility" suggests this is known debt.
- Fix approach: Disable unused WMs (`desktop.xmonad.enable = false`, etc.) in `vm-shared.nix`. Only enable DWM (the default session) and optionally Hyprland if Wayland is desired.

**Unused/Dead Files:**
- Issue: Several files exist in the repo but are never imported or referenced by any active configuration.
- Files:
  - `lib/overlays.nix`: Old auto-discovery overlay loader — only referenced in commented-out lines in `users/cipher/darwin.nix` (line 83) and `users/phantom/darwin.nix` (line 82). Replaced by explicit overlay list in `flake.nix`.
  - `lib/greetd.nix`: Greetd display manager config — never imported by any module. Mostly commented out.
  - `pkgs/opencode-fhs.nix`: FHS wrapper for OpenCode — never referenced in any module or overlay. OpenCode is now imported via the flake input in `flake.nix` (line 67).
  - `modules/display-profiles.nix`: Imports `display-x11.nix` and `parallels-display.nix` but is never imported by any machine config.
  - `modules/display-x11.nix`: Display profile NixOS module — never imported by active machine configs (only by the unused `display-profiles.nix`).
  - `modules/parallels-display.nix`: Parallels display optimizations — never imported by active machine configs.
- Impact: Code rot. These files may contain outdated patterns and confuse future maintainers about what's actually active.
- Fix approach: Either integrate `display-profiles.nix` / `parallels-display.nix` / `display-x11.nix` into the active machine config (`vm-aarch64-prl.nix`) if the features are wanted, or delete them. Delete `lib/overlays.nix`, `lib/greetd.nix`, and `pkgs/opencode-fhs.nix`.

**WSL Support References Without Input:**
- Issue: `lib/mksystem.nix` references `inputs.nixos-wsl.nixosModules.wsl` (line 58) but `nixos-wsl` is not declared in `flake.nix` inputs. If anyone tried to build a WSL config, it would fail with an undefined attribute error.
- Files: `lib/mksystem.nix` (lines 9, 16, 58)
- Impact: Attempting to use `wsl = true` in any config will crash. Dead code path.
- Fix approach: Either add `nixos-wsl` to `flake.nix` inputs, or remove WSL support from `mksystem.nix` if it's not planned.

**Empty Machine Configs:**
- Issue: Three Darwin machine configs are essentially empty — they contain only parameter declarations and an empty attribute set `{ }`.
- Files:
  - `machines/vm-darwin.nix` (3 lines): `{ config, pkgs, ... }: { }`
  - `machines/macbook-cipher.nix` (3 lines): `{ config, pkgs, ... }: { }`
  - `machines/macbook-phantom.nix` (3 lines): `{ config, pkgs, ... }: { }`
- Impact: All machine-specific config lives in the user darwin.nix files instead. This creates confusion about where system-level Darwin config should go.
- Fix approach: Either move Darwin system config from `users/*/darwin.nix` into these machine files (proper separation), or document that Darwin machines use user-level files for system config.

**Commented-Out Code Accumulation:**
- Issue: Large blocks of commented-out code persist across multiple files, reducing readability and making it unclear what's intentionally disabled vs. abandoned.
- Files:
  - `users/cipher/home-manager.nix` (lines 214-292): ~80 lines of commented-out email configuration (mbsync, msmtp)
  - `users/jrothberg/home-manager.nix` (lines 69-145): ~75 lines of identical commented-out email configuration
  - `users/cipher/nixos.nix` (lines 14-18): Commented-out hashedPassword and SSH keys
  - `users/jrothberg/nixos.nix` (lines 13-16): Same commented-out hashedPassword and SSH keys
  - `modules/parallels-guest.nix` (lines 143-147): Commented-out prlsga service
  - `lib/greetd.nix` (lines 9-15, 22-24): Mostly commented out
- Impact: Clutters code review, increases cognitive load.
- Fix approach: Remove commented-out code. Use git history to recover if needed.

## Known Bugs

**`source ~/.bash_join_db` Fails on Fresh Systems:**
- Symptoms: ZSH init produces an error on any system that doesn't have `~/.bash_join_db` file, since `source` fails if the file doesn't exist.
- Files: `users/common/shell.nix` (line 184)
- Trigger: Open any new shell on a fresh system or a system without that specific file.
- Workaround: Create an empty `~/.bash_join_db` file. The error is non-fatal but noisy.

**Duplicate `sessionCommands` Attribute in `display-x11.nix`:**
- Symptoms: Nix evaluator may error or silently discard one of two `services.xserver.displayManager.sessionCommands` blocks defined in the same file.
- Files: `modules/display-x11.nix` (lines 116-137 and lines 140-149): Two separate `sessionCommands` assignments — one for display profile initialization and one for Xft settings.
- Trigger: If this module is imported, the duplicate attribute causes an evaluation error.
- Workaround: Module is currently not imported by any active config. To fix, merge both `sessionCommands` blocks into a single `mkAfter` call.

**VMware Guest Video Drivers Don't Compile (Known TODO):**
- Symptoms: VMware video drivers (`xf86inputvmmouse`, `vmware` video driver) are commented out with a TODO note.
- Files: `modules/vmware-guest.nix` (lines 70-72)
- Trigger: Attempting to uncomment the `videoDrivers` or `modules` lines will fail to build.
- Workaround: Leave commented out. The VM uses fallback drivers.

**`neofetch` Is Deprecated/Unmaintained:**
- Symptoms: `neofetch` is listed as a package and called on every shell init. The upstream project is archived and unmaintained.
- Files:
  - `users/common/packages.nix` (line 103): Package installation
  - `users/common/shell.nix` (line 191): Called on every new shell
- Trigger: Every new terminal session runs `neofetch`, adding startup latency.
- Workaround: Replace with `fastfetch` or remove from shell init.

**Display Profile Modelines Use Invalid `optimized` Keyword:**
- Symptoms: The Parallels display module generates X11 modelines with `optimized` as the pixel clock value, which is not valid modeline syntax.
- Files: `modules/parallels-display.nix` (line 144): `Modeline "${res}_60.00" optimized`
- Trigger: If this module were actually imported and used, X server could reject the modeline entries.
- Workaround: Module is currently not imported by any active config.

## Security Considerations

**Plaintext Initial Passwords in Version Control:**
- Risk: User accounts `cipher` and `jrothberg` have `initialPassword` set to their username in plaintext. While `initialPassword` is only used on first login, these passwords are in a public/shared git repository.
- Files:
  - `users/cipher/nixos.nix` (line 13): `initialPassword = "cipher"`
  - `users/jrothberg/nixos.nix` (line 11): `initialPassword = "jrothberg"`
- Current mitigation: The `initialPassword` is overridden by `passwd` after first login. `users.mutableUsers = false` in `vm-shared.nix` (line 123) means password changes don't persist across rebuilds — but `initialPassword` is still used.
- Recommendations: Use `hashedInitialPassword` with a properly hashed value, or use `passwordFile` pointing to a file not in version control. Consider using agenix/sops-nix for secret management.

**SSH Password Authentication Enabled:**
- Risk: SSH password authentication is enabled in the VM, allowing brute-force attacks if the VM is network-accessible.
- Files: `machines/vm-shared.nix` (line 171): `services.openssh.settings.PasswordAuthentication = true`
- Current mitigation: Firewall is disabled (line 176) and the comment says "we're in a VM and we want to make it easy." Root login is disabled.
- Recommendations: Use SSH key-only authentication. The Makefile bootstrap process should inject SSH keys automatically.

**Firewall Disabled on All VMs:**
- Risk: All VM configurations inherit `networking.firewall.enable = false` from `vm-shared.nix`. While VMs use NAT networking, any port-forwarding or bridged network config would expose all services.
- Files: `machines/vm-shared.nix` (line 176)
- Current mitigation: NAT networking provides some isolation.
- Recommendations: Enable the firewall with allowed ports for SSH and any other required services.

**Hardcoded UID 1000 in Multiple Locations:**
- Risk: Parallels shared folder mount and VMware host filesystem both hardcode `uid=1000`, which may not match the actual user's UID. Incorrect UID means another user could access files with wrong permissions.
- Files:
  - `modules/parallels-guest.nix` (line 107): `uid=1000,gid=100`
  - `machines/vm-aarch64.nix` (line 25): `uid=1000`
  - `machines/vm-intel.nix` (line 17): `uid=1000`
- Current mitigation: Only one user account per VM, and it's always UID 1000.
- Recommendations: Use `config.users.users.${user}.uid` or a parameterized approach.

**SSH `HostkeyAlgorithms +ssh-rsa` Allows Weak Algorithm:**
- Risk: SSH config globally adds `ssh-rsa` to accepted host key algorithms. SHA-1 based `ssh-rsa` is deprecated due to collision attacks.
- Files: `users/common/git.nix` (line 55): `HostkeyAlgorithms +ssh-rsa`
- Current mitigation: Only a fallback — `+` prefix means it's added to (not replacing) the default list.
- Recommendations: Move `ssh-rsa` allowance to specific host blocks that require it (e.g., the `bluebeam` match block already has it). Remove the global `+ssh-rsa`.

**Clipboard State Stored in /tmp:**
- Risk: Display profile state (`/tmp/.current-display-profile`, `/tmp/.parallels-dynamic-resolution`) is stored in world-readable `/tmp`. Any user on the system can read or modify the display profile.
- Files:
  - `scripts/display-profiles/display-switcher.sh` (line 7): `PROFILE_FILE="/tmp/.current-display-profile"`
  - `modules/parallels-display.nix` (line 28): `touch /tmp/.parallels-dynamic-resolution`
  - `overlays/firefox-hidpi.nix` (lines 7-9): Reads from `/tmp/.current-display-profile`
- Current mitigation: Single-user VM.
- Recommendations: Use `$XDG_RUNTIME_DIR` instead of `/tmp` for user-specific state.

## Performance Bottlenecks

**`neofetch` on Every Shell Init:**
- Problem: `neofetch` runs on every new terminal/shell invocation, adding 200-500ms startup latency.
- Files: `users/common/shell.nix` (line 191): `neofetch`
- Cause: Called unconditionally in `initContent` for ZSH.
- Improvement path: Remove from shell init, or replace with `fastfetch` which is significantly faster. Alternatively, only run on login shells, not subshells.

**All Four WMs Increase Build Time:**
- Problem: Building all four window managers (XMonad, DWM, AwesomeWM, Hyprland) on every NixOS rebuild is slow, especially XMonad which requires GHC and Haskell packages.
- Files: `machines/vm-shared.nix` (lines 88-91)
- Cause: All desktop toggles set to `true` for backward compatibility.
- Improvement path: Disable unused WMs. If DWM is the default, only enable DWM (and optionally Hyprland for Wayland support).

**Large Package List With Unused Tools:**
- Problem: The common packages list includes many heavy packages that may not be actively used (e.g., `lapce`, `warp-terminal`, `nyxt`, `vscode`, `pgadmin4`, `libreoffice`). Each adds to the system closure and rebuild time.
- Files: `users/common/packages.nix` (lines 4-118): ~115 packages total
- Cause: Packages accumulated over time without pruning.
- Improvement path: Audit package usage and move rarely-used packages to per-project `shell.nix` / `flake.nix` dev environments instead of the system-wide config.

## Fragile Areas

**Parallels Tools Package (`pkgs/parallels-tools/default.nix`):**
- Files: `pkgs/parallels-tools/default.nix`, `modules/parallels-guest.nix`
- Why fragile: This is a custom derivation that downloads a DMG from Parallels' CDN, extracts an ISO from it, and patches binaries. It replaces the upstream nixpkgs module (via `disabledModules`). Any Parallels version update requires updating the URL, hash, and potentially the patch file and build logic. The prlfsmountd patch uses fragile string replacement (`--replace-fail`).
- Safe modification: When updating Parallels version, update `version` and `sha256` in `pkgs/parallels-tools/default.nix`. Test that the `preBuild` substituteInPlace still matches. Rebuild and verify shared folders, clipboard, and display still work.
- Test coverage: None. Manual testing required.

**Custom VMware Guest Module:**
- Files: `modules/vmware-guest.nix`, `machines/vm-aarch64.nix`
- Why fragile: Overrides the upstream nixpkgs `virtualisation/vmware-guest.nix` module with a custom version. Upstream changes won't be automatically inherited. Some features (video drivers) are known broken (TODO comment).
- Safe modification: Compare against upstream nixpkgs version when updating. Video driver section is non-functional — don't uncomment without testing.
- Test coverage: None.

**Clock Skew Detector / Time Sync System:**
- Files: `machines/vm-aarch64-prl.nix` (lines 22-158, 194-232)
- Why fragile: Complex bash script embedded in Nix that monitors CLOCK_MONOTONIC, detects VM freeze/unfreeze, calls an external HTTP API (timeapi.io) for time sync. If timeapi.io goes down or changes its API, time sync fails silently (after retries). The clock skew detector uses arithmetic in bash which can be fragile with edge cases.
- Safe modification: Keep the retry logic. Consider adding a fallback time source (e.g., `worldtimeapi.org` or NTP-based approach).
- Test coverage: None. Relies on manual VM sleep/wake testing.

**Parallels Clipboard X11 Bridge:**
- Files: `modules/parallels-clipboard-x11-bridge.nix`
- Why fragile: Uses `mkForce` to override the default prlcp service definition from `modules/parallels-guest.nix`. Hardcodes `DISPLAY = ":0"`. Disables a "problematic focus guard" service (lines 46-49) by forcing it to empty. The clipboard bridge between host macOS and VM X11 depends on multiple interacting services (prltoolsd, prlcp, xclip).
- Safe modification: If clipboard breaks, run `fix-parallels-clipboard` first. Check `journalctl --user -u prlcp` for errors.
- Test coverage: None. Includes diagnostic scripts (`fix-parallels-clipboard`, `test-clipboard-sync`) for manual testing.

**DWM Overlay with External Git Dependency:**
- Files: `overlays/dwm.nix`
- Why fragile: Fetches DWM source from `github:JonnyWalker81/dwm` at a pinned commit. Any DWM change requires: commit in DWM repo → push → update rev in overlay → build to get hash → update hash → rebuild. This 8-step process (documented in CLAUDE.md) is error-prone. If the DWM repo's `xmonad-parity` branch is force-pushed or deleted, the build breaks.
- Safe modification: Follow the exact workflow in CLAUDE.md. Never skip the push step.
- Test coverage: None.

## Scaling Limits

**Single-Machine Configuration Sharing via `vm-shared.nix`:**
- Current capacity: 3 NixOS VMs share `vm-shared.nix` but with different hypervisors (Parallels, VMware) and different needs.
- Limit: Adding more machines with divergent requirements will require either more conditionals in `vm-shared.nix` or splitting it.
- Scaling path: Refactor `vm-shared.nix` into smaller, composable modules (e.g., `modules/base-system.nix`, `modules/desktop-environment.nix`, `modules/vm-common.nix`) that can be imported selectively.

**User Configuration Duplication:**
- Current capacity: 3 users (cipher, jrothberg, phantom) with significant copy-paste between their `home-manager.nix` and `darwin.nix` files.
- Limit: Adding a fourth user requires copying ~150+ lines of boilerplate Darwin config.
- Scaling path: Extract shared Darwin patterns into a `users/common/darwin.nix` module (similar to how `users/common/` handles shared home-manager config). The macOS system defaults, homebrew config, and font packages are nearly identical across all three Darwin user configs.

## Dependencies at Risk

**Pinned `nixpkgs-old-kernel` for VMware Compatibility:**
- Risk: `nixpkgs-old-kernel` is pinned to a specific commit (`bacbfd713b...`) that provides Linux kernel 6.1 for VMware Fusion Tech Preview compatibility. This nixpkgs snapshot will become increasingly stale and may accumulate security vulnerabilities.
- Impact: The `vm-aarch64` config (VMware on M1) uses outdated packages from this pinned nixpkgs.
- Migration plan: When VMware Fusion supports newer kernels on aarch64, switch to main `nixpkgs` input. If VMware support is no longer needed (Parallels is the primary VM), consider removing this config entirely.

**External Time API Dependency (`timeapi.io`):**
- Risk: The time sync service depends on `timeapi.io`, a free third-party API with no SLA. If the service goes down, the VM's clock may drift after sleep/wake cycles.
- Impact: `vm-aarch64-prl.nix` time sync fails after max retries, leaving the system with incorrect time.
- Migration plan: Add fallback time sources. Consider using `chronyd` or `ntpd` with multiple servers instead of an HTTP-based approach.

**`picom-pijulius` Package in System Packages:**
- Risk: `picom-pijulius` is listed in `machines/vm-shared.nix` (line 148) as a system package but is a fork of picom. Meanwhile, the picom overlay in `overlays/picom.nix` overrides the main `picom` package. Having both may cause confusion.
- Impact: Unclear which picom is actually used at runtime.
- Migration plan: Remove `picom-pijulius` from system packages if the picom overlay provides the desired version.

**Hyprland Pinned to Specific Tag (`v0.50.1`):**
- Risk: Hyprland is pinned to `v0.50.1` via `?ref=v0.50.1` in `flake.nix` (line 21). This version may become incompatible with the Hyprland config in `users/hyprland.nix` if settings syntax changes.
- Impact: Updating Hyprland requires reviewing the config for breaking changes.
- Migration plan: When updating, check the Hyprland changelog for config syntax changes. Test thoroughly.

## Missing Critical Features

**No Secret Management System:**
- Problem: There is no secret management solution (agenix, sops-nix, git-crypt) in use. Passwords are in plaintext, SSH keys must be manually managed, and API credentials are referenced from files that don't exist in the repo (e.g., `~/.bash_join_db`).
- Blocks: Cannot safely store API keys, database credentials, or other secrets in the repository. Makes bootstrapping new machines require manual secret transfer.

**No Automated Testing:**
- Problem: There are no NixOS tests, no `nix flake check`, and no CI pipeline. Configuration validity is only verified by manual `nixos-rebuild switch`.
- Blocks: Cannot catch regressions before deploying. Breaking changes to shared modules (like `common/`) silently break other user/machine configs.

**No `nix flake check` or CI:**
- Problem: The flake does not define `checks` outputs. There is no GitHub Actions, Hydra, or other CI that validates builds.
- Blocks: Pull requests (if used) cannot be automatically validated. Drift between Darwin and NixOS configs goes undetected.

## Test Coverage Gaps

**Zero Automated Tests:**
- What's not tested: Everything. No NixOS VM tests, no flake checks, no linting.
- Files: No test files exist anywhere in the repository.
- Risk: Any change to shared modules (`users/common/`, `lib/mksystem.nix`, overlays) could break multiple system configs without detection. The Parallels tools package could silently fail to build for new kernel versions.
- Priority: High — at minimum, add `nix flake check` with build tests for each system configuration.

**Parallels Tools Not Integration Tested:**
- What's not tested: The custom `pkgs/parallels-tools/default.nix` derivation, the clipboard bridge, and the time sync system.
- Files: `pkgs/parallels-tools/default.nix`, `modules/parallels-guest.nix`, `modules/parallels-clipboard-x11-bridge.nix`, `machines/vm-aarch64-prl.nix`
- Risk: Parallels version updates could break shared folders, clipboard, or display without detection.
- Priority: High — this is the primary development environment.

**Darwin Configs Not Regularly Built:**
- What's not tested: The three Darwin configurations (`vm-darwin`, `macbook-cipher`, `macbook-phantom`) may not be regularly built/tested since the primary work happens on the Parallels NixOS VM.
- Files: `machines/vm-darwin.nix`, `machines/macbook-cipher.nix`, `machines/macbook-phantom.nix`, `users/*/darwin.nix`
- Risk: Changes to `users/common/` or overlays could break Darwin builds silently.
- Priority: Medium — add Darwin build checks to CI if/when implemented.

---

*Concerns audit: 2026-02-24*
