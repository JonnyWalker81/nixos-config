# Testing Patterns

**Analysis Date:** 2026-02-24

## Test Framework

**Runner:**
- No automated test framework (no Jest, pytest, vitest, or similar)
- Testing is done via NixOS rebuild commands that validate configuration at build time
- Nix's evaluation and build system serves as the primary "test harness" -- invalid configurations fail at `nixos-rebuild` time

**Validation Commands:**
```bash
# Test a configuration without applying (evaluates and builds, does not switch)
make test NIXNAME=vm-aarch64-prl

# Equivalent direct command
sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild test --flake ".#vm-aarch64-prl"

# Full rebuild and switch (the "integration test")
make switch NIXNAME=vm-aarch64-prl

# Equivalent direct command
sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"
```

**Config:** `Makefile` (lines 19-23)

## Test File Organization

**Location:**
- No dedicated test directory or test files exist
- Two ad-hoc test scripts exist at the repo root (both are gitignored):
  - `test_emacs.sh` -- Tests Emacs JavaScript mode detection in batch mode
  - `test_interactive.sh` -- Creates temp files and guides manual Emacs testing

**Naming:**
- Test scripts follow `test_*.sh` or `test-*.sh` pattern
- Both patterns are in `.gitignore` (line 27: `test-*.sh`, line 4: `test_emacs.sh`, line 5: `test_interactive.sh`)

**Structure:**
```
/                           # Repo root
├── test_emacs.sh           # Ad-hoc Emacs test (gitignored)
├── test_interactive.sh     # Ad-hoc interactive test (gitignored)
├── run.sh                  # Docker-based LazyVim test
└── Makefile                # Primary test/build commands
```

## Test Strategy

**Nix Evaluation as Testing:**
- The Nix language is purely functional and lazily evaluated. Configuration errors are caught at evaluation time by `nixos-rebuild test` or `nixos-rebuild switch`
- Type checking is enforced by the NixOS module system's `mkOption` type declarations:
  ```nix
  type = types.bool;          # modules/parallels-guest.nix
  type = types.enum [...]     # modules/display-x11.nix (profileType)
  type = types.attrsOf (...)  # modules/display-x11.nix (profiles)
  ```
- Assertions provide runtime validation:
  ```nix
  assertions = [ {
    assertion = pkgs.stdenv.isi686 || pkgs.stdenv.isx86_64 || pkgs.stdenv.isAarch64;
    message = "VMWare guest is not currently supported on ${pkgs.stdenv.hostPlatform.system}";
  } ];
  ```
  (see `modules/vmware-guest.nix` line 30)

**Build-Time Checks:**
- Overlay overrides explicitly disable checks for packages with broken test suites:
  ```nix
  doCheck = false;
  doInstallCheck = false;
  ```
  (see `overlays/picom.nix` line 16-17, `overlays/vim-plugins.nix` lines 7, 21, 27, 34)

**Manual Validation:**
- After rebuilding, manual testing validates:
  - Window managers launch correctly (DWM, XMonad, AwesomeWM, Hyprland)
  - Clipboard works between VM and host
  - Display profiles switch correctly
  - Services start properly (check with `systemctl status`)
- Diagnostic scripts exist for manual validation:
  - `test-clipboard-sync` -- Tests clipboard round-trip between VM and host (defined in `modules/parallels-clipboard-x11-bridge.nix`)
  - `fix-parallels-clipboard` -- Restarts clipboard service and tests (defined in `modules/parallels-clipboard-x11-bridge.nix`)
  - `parallels-display-info` -- Shows display configuration status (defined in `modules/parallels-display.nix`)

## Ad-Hoc Test Scripts

**`test_emacs.sh`:**
```bash
#!/bin/bash
echo "Testing Emacs JavaScript mode detection..."
echo "1. Testing with plain emacs (no Doom):"
emacs --batch --eval '(progn (find-file "test.js") (message "Major mode: %s" major-mode))' 2>&1 | grep "Major mode"
# ... more batch-mode tests
```
Pattern: Uses `emacs --batch --eval` for non-interactive validation of Emacs configuration.

**`run.sh`:**
```bash
docker run -w /root -it --rm alpine:edge sh -uelic '
  apk add git lazygit neovim ripgrep alpine-sdk --update
  git clone https://github.com/LazyVim/starter ~/.config/nvim
  cd ~/.config/nvim
  nvim
'
```
Pattern: Uses Docker for isolated testing of editor configurations.

## Testing Approaches for Different Config Areas

**Machine Configurations:**
- Test with: `sudo nixos-rebuild test --flake ".#<machine-name>"`
- Validates: NixOS module evaluation, package resolution, service definitions
- Available machines: `vm-aarch64-prl`, `vm-aarch64`, `vm-intel`

**Darwin Configurations:**
- Test with: `sudo darwin-rebuild switch --flake ".#<darwin-name>"`
- Available: `vm-darwin`, `macbook-phantom`, `macbook-cipher`

**Overlay Changes:**
- Overlays are tested implicitly during rebuild
- If an overlay breaks package resolution, `nixos-rebuild` fails with clear error
- Hash mismatches (e.g., after updating DWM rev) produce a specific error with the correct hash

**Shell Scripts:**
- No automated testing for shell scripts in `scripts/`
- Scripts are tested manually after rebuild
- Some scripts include self-test functionality (e.g., `test-clipboard-sync`)

## Coverage

**Requirements:** None enforced

**What IS validated by rebuild:**
- All Nix expression evaluation (syntax, types, option constraints)
- Package availability and buildability
- Module option compatibility
- Import path resolution
- Overlay application order

**What is NOT validated by rebuild:**
- Runtime behavior of services (systemd units, scripts)
- Correct display/UI rendering
- Network-dependent functionality (time sync, clipboard sync)
- Shell script logic (scripts in `scripts/` directory)
- Correct keybindings in window managers

## Common Patterns

**Testing a Configuration Change:**
```bash
# 1. Make changes to nix files
# 2. Test build (does NOT apply)
make test NIXNAME=vm-aarch64-prl
# 3. If test passes, apply
make switch NIXNAME=vm-aarch64-prl
# 4. Manually verify runtime behavior
# 5. Commit (after user confirms rebuild success)
```

**Testing Overlay Hash Updates (DWM example):**
```bash
# 1. Set sha256 to empty string "" in overlays/dwm.nix
# 2. Rebuild -- will fail with hash mismatch
sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"
# 3. Copy correct sha256 from error output (the "got:" line)
# 4. Update overlays/dwm.nix with correct hash
# 5. Rebuild again -- should succeed
```

**Testing VM Configurations Remotely:**
```bash
# Copy config to VM
make vm/copy NIXADDR=<ip>
# Apply on VM
make vm/switch NIXADDR=<ip> NIXNAME=<config>
```

**Service Debugging:**
```bash
# Check service status
systemctl status <service-name>
systemctl --user status <user-service>

# View service logs
journalctl -u <service-name> -n 50
journalctl --user -u <user-service> -n 50
```

## Recommendations for Adding Tests

**If adding Nix-level tests:**
- Use NixOS test infrastructure (`nixosTest`) for integration tests
- Place in a `tests/` directory at repo root
- Add to flake outputs under `checks`

**If adding shell script tests:**
- Use `bats` (Bash Automated Testing System) or simple assertion scripts
- Place alongside scripts in `scripts/tests/`
- Test critical scripts: time sync, display profile switching, clipboard fix

**For validating configurations across all targets:**
```bash
# Build all configurations without switching (would need to be added)
nix build .#nixosConfigurations.vm-aarch64-prl.config.system.build.toplevel
nix build .#nixosConfigurations.vm-intel.config.system.build.toplevel
nix build .#darwinConfigurations.macbook-cipher.config.system.build.toplevel
```

---

*Testing analysis: 2026-02-24*
