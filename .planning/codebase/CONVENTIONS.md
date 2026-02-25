# Coding Conventions

**Analysis Date:** 2026-02-24

## Naming Patterns

**Files:**
- Use lowercase-kebab-case for all `.nix` files: `vm-aarch64-prl.nix`, `home-manager.nix`, `unstable-packages.nix`
- Machine configs match the flake configuration name exactly: `machines/vm-aarch64-prl.nix` maps to `nixosConfigurations.vm-aarch64-prl`
- User directories match the system user name: `users/cipher/`, `users/jrothberg/`, `users/phantom/`
- Shell scripts use lowercase-kebab-case: `display-switcher.sh`, `setup-wallpaper.sh`, `ssh-key-manager.sh`
- Overlay files are named for what they overlay: `overlays/dwm.nix`, `overlays/picom.nix`, `overlays/fonts.nix`

**Nix Attribute Names:**
- Use camelCase for custom attributes: `machineConfig`, `userOSConfig`, `userHMConfig`, `isWSL`, `isDarwin`, `isLinux`
- Follow nixpkgs conventions for options: `hardware.parallels.enable`, `desktop.dwm.enable`
- Use camelCase for `let` bindings: `syncTimeScript`, `clockSkewDetector`, `extendKernelPackages`

**Module Options:**
- Boolean options use `mkEnableOption` with a descriptive string: `lib.mkEnableOption "DWM window manager"` (see `modules/desktop/dwm.nix`)
- Custom option namespaces use dotted paths under logical groups: `desktop.xmonad.enable`, `hardware.parallels.display.dynamicResolution`

**Shell Scripts:**
- Use UPPER_SNAKE_CASE for script-level constants: `WALLPAPER_DIR`, `PROFILE_FILE`, `MAX_RETRIES`
- Use lowercase for local variables inside functions: `local profile`, `local test_string`
- Function names use snake_case: `apply_profile()`, `wait_for_network()`, `mono_now()`

## Code Style

**Formatting:**
- The codebase uses `nixpkgs-fmt` (installed in `users/common/packages.nix`) for Nix formatting
- `nixfmt-rfc-style` is also installed but `nixpkgs-fmt` is the primary formatter
- No `.nixfmt` or formatting config file exists; use default `nixpkgs-fmt` settings
- Indentation: 2 spaces for Nix files
- Indentation: 2 spaces for shell scripts embedded in Nix, 4 spaces for standalone `.sh` files

**Linting:**
- No dedicated Nix linter configuration (no statix, deadnix config files)
- `nil` and `nixd` language servers are installed for editor-based linting (`users/common/packages.nix`)

**Line Length:**
- No enforced line length limit
- Long attribute paths and strings are allowed to extend beyond 80 characters
- Multi-line strings use `''` (Nix multi-line string syntax) for embedded shell scripts

## Nix Language Patterns

**Function Signatures:**
- Module files use the standard NixOS module pattern with destructured attrset:
  ```nix
  { config, lib, pkgs, ... }:
  ```
- Custom arguments from `_module.args` are added to the signature when needed:
  ```nix
  { config, pkgs, lib, currentSystem, currentSystemName, inputs, ... }:
  ```
  (see `machines/vm-shared.nix`)

**`with` Usage:**
- Use `with lib;` at the module level for modules that heavily use lib functions (see `modules/parallels-guest.nix`, `modules/vmware-guest.nix`)
- Use `with pkgs;` inside list expressions for package lists (see `machines/vm-shared.nix` line 138)
- Avoid `with` for short usages; prefer explicit `lib.mkIf`, `lib.mkForce` in most files

**`let...in` Blocks:**
- Place `let` bindings at the top of the file or module, before the main attrset
- Use `let` for computed values, script derivations, and conditional logic:
  ```nix
  let
    isLinux = !isDarwin;
    common = import ../common { ... };
  in { ... }
  ```
  (see `users/cipher/home-manager.nix`)

**Platform Conditionals:**
- Use `lib.mkIf isLinux` / `lib.mkIf isDarwin` for platform-specific sections
- Use `lib.optionals (!pkgs.stdenv.isDarwin)` for conditional list items (packages)
- Use `lib.mkForce` to override inherited values on specific platforms:
  ```nix
  systemd.user.services = lib.mkIf isDarwin (lib.mkForce { });
  ```
- The `isDarwin`/`isLinux` booleans are derived from either function arguments or `pkgs.stdenv.isDarwin`

**Overlay Patterns:**

Two overlay styles exist:

1. **Plain overlays** (no inputs needed) use `final: prev:` signature:
   ```nix
   # overlays/dwm.nix
   final: prev: {
     dwm = prev.dwm.overrideAttrs (old: { ... });
   }
   ```

2. **Input-dependent overlays** use a curried function taking `{ inputs }`:
   ```nix
   # overlays/unstable-packages.nix
   { inputs }:
   final: prev:
   let
     unstablePkgs = import inputs.nixpkgs-unstable { ... };
   in { ... }
   ```

All overlays are imported explicitly in `flake.nix` lines 57-77 with clear section comments.

**Package Override Patterns:**
- Use `overrideAttrs` for modifying existing derivations (see `overlays/picom.nix`, `overlays/dwm.nix`)
- Use `callPackage` for fully custom packages (see `overlays/default.nix` line 11)
- Disable tests in overrides with `doCheck = false;` (see `overlays/vim-plugins.nix`)
- Append to build inputs with `(old.buildInputs or [ ]) ++ [...]` pattern

## Import Organization

**Order in module files:**
1. Imports list (`imports = [ ... ]`)
2. Options declarations (for custom modules)
3. Configuration settings

**Import path conventions:**
- Use relative paths with `../` for cross-directory imports: `../hardware/vm-aarch64-prl.nix`
- Use `./` for same-directory imports: `./vm-shared.nix`
- Use `modulesPath` for nixpkgs built-in modules: `(modulesPath + "/profiles/qemu-guest.nix")`

**Flake import pattern in `flake.nix`:**
```nix
overlays = [
  # --- External flake overlays ---
  inputs.emacs-overlay.overlay

  # --- Input-dependent overlays (must be explicitly imported) ---
  (import ./overlays/unstable-packages.nix { inherit inputs; })

  # --- Auto-discovered overlays (no inputs needed) ---
  (import ./overlays/default.nix)
];
```

## Error Handling

**Nix Evaluation:**
- Use `throw` for unsupported configurations: `throw "unsupported system: ${system}"` (see `pkgs/hashicorp/generic.nix` line 18)
- Use assertions for preconditions in modules:
  ```nix
  assertions = [ {
    assertion = pkgs.stdenv.isi686 || pkgs.stdenv.isx86_64 || pkgs.stdenv.isAarch64;
    message = "VMWare guest is not currently supported on ${pkgs.stdenv.hostPlatform.system}";
  } ];
  ```
  (see `modules/vmware-guest.nix` line 30)

**Shell Scripts (embedded in Nix):**
- Use `set -e` at the top of standalone scripts (see `scripts/display-profiles/display-switcher.sh`)
- Use `|| true` for commands that may fail but should not abort: `pkill -9 prlcp 2>/dev/null || true`
- Use retry loops with configurable max retries for network-dependent operations (see `machines/vm-aarch64-prl.nix` lines 72-158)
- Log to systemd journal via `systemd-cat` for system services:
  ```bash
  log() {
    echo "$1" | ${systemd-cat} -t "$LOG_TAG" -p "''${2:-info}"
  }
  ```

**Systemd Services:**
- Use `Restart = "on-failure"` or `Restart = "always"` for resilient services
- Include `RestartSec` to prevent rapid restart loops
- Use `StartLimitIntervalSec` and `StartLimitBurst` for rate limiting (see `modules/parallels-clipboard-x11-bridge.nix`)

## Comments

**When to Comment:**
- Add a file-level comment explaining the purpose of each overlay file (first line):
  ```nix
  # Custom DWM build from github.com/JonnyWalker81/dwm (xmonad-parity branch)
  ```
- Add comments explaining "why" for non-obvious workarounds:
  ```nix
  # The official parallels guest support does not work currently.
  # https://github.com/NixOS/nixpkgs/pull/153665
  disabledModules = [ "virtualisation/parallels-guest.nix" ];
  ```
- Add inline comments for configuration values that need context:
  ```nix
  boot.kernel.sysctl = {
    "vm.swappiness" = 1;  # implicit from context
  };
  ```
- Use section-separator comments (`# ---`) for major sections within a file:
  ```nix
  # --- Linux-only configuration ---
  # --- Darwin-only configuration ---
  ```

**Comment Style:**
- Use `#` for Nix comments (single-line)
- Use `/* */` only for multi-line doc comments in library code (see `lib/overlays.nix`)
- Reference GitHub issues/PRs when disabling upstream features
- Use `# NOTE:` prefix sparingly; prefer explanatory comments without prefix

## Module Design

**NixOS Module Pattern:**
- Custom modules declare options under a namespace and use `config = mkIf` to conditionally apply:
  ```nix
  {
    options.desktop.dwm.enable = lib.mkEnableOption "DWM window manager";
    config = lib.mkIf config.desktop.dwm.enable {
      services.xserver.windowManager.dwm.enable = true;
    };
  }
  ```
  (see `modules/desktop/dwm.nix`, `modules/desktop/awesome.nix`, `modules/desktop/hyprland.nix`)

**Barrel/Index Files:**
- Use `default.nix` as a barrel file that imports sub-modules:
  ```nix
  # modules/desktop/default.nix
  { config, lib, pkgs, ... }:
  { imports = [ ./xmonad.nix ./dwm.nix ./awesome.nix ./hyprland.nix ]; }
  ```
  ```nix
  # users/common/default.nix
  { ... }:
  { imports = [ ./packages.nix ./shell.nix ./git.nix ./editors.nix ... ]; }
  ```

**Home-Manager Modules:**
- Home-manager configs use a double-function pattern (curried) to receive flake args then HM args:
  ```nix
  { isWSL, isDarwin, inputs, ... }:
  { config, lib, pkgs, ... }:
  let
    common = import ../common { ... };
  in { imports = [ common ]; ... }
  ```
  (see `users/cipher/home-manager.nix`, `users/jrothberg/home-manager.nix`)

**Shared vs Per-User Configuration:**
- Common/shared config lives in `users/common/` split by concern (packages, shell, git, editors, terminal, desktop, services, dotfiles)
- Per-user overrides go in `users/{username}/home-manager.nix` using `lib.mkIf` and `lib.mkForce`
- Per-user OS config goes in `users/{username}/nixos.nix` (Linux) or `users/{username}/darwin.nix` (macOS)

## Git Commit Conventions

**Format:** `<type>(<scope>): <short description>`

**Types used (from commit history):**
- `feat`: New features
- `fix`: Bug fixes
- `refactor`: Code restructuring
- `chore`: Maintenance (flake.lock updates, dependency bumps)
- `docs`: Documentation

**Scopes used:**
- `flake`, `machines`, `doom`, `parallels`, `packages`, `modules`, `home-manager`, `users`, `lib`, `overlays`, `scripts`, `pkgs`

**Rules:**
- One logical change per commit
- Auto-commit after user confirms successful rebuild
- `chore(flake): update flake.lock` for lock file updates

## Embedded Shell Script Conventions

**Binary references in Nix-embedded scripts:**
- Always use full Nix store paths for binaries via `let` bindings:
  ```nix
  let
    curl = "${pkgs.curl}/bin/curl";
    jq = "${pkgs.jq}/bin/jq";
    date = "${pkgs.coreutils}/bin/date";
  in ...
  ```
  (see `machines/vm-aarch64-prl.nix` lines 9-15)

**`writeShellScript` and `writeShellScriptBin`:**
- Use `pkgs.writeShellScript` for scripts that don't need to be on PATH (systemd ExecStart)
- Use `pkgs.writeShellScriptBin` for scripts that should be callable by name
- Reference Nix store paths inline with `${pkgs.foo}/bin/foo` pattern

**Nix string escaping in shell:**
- Use `''${VAR}` (doubled single quotes) to escape `${}` in Nix multi-line strings:
  ```nix
  ''
    ELAPSED=''${CHECK_INTERVAL}s
    echo "interval is ''${RETRY_DELAY}s"
  ''
  ```

---

*Convention analysis: 2026-02-24*
