# Architecture

**Analysis Date:** 2026-02-24

## Pattern Overview

**Overall:** Modular Nix Flake Configuration with Unified System Builder

**Key Characteristics:**
- Single `mkSystem` builder function that generates NixOS, nix-darwin, and WSL configurations from a unified interface
- Layered composition: flake inputs -> overlays -> machine config -> user OS config -> home-manager config
- Platform-conditional logic (`isDarwin`/`isLinux`/`isWSL`) within shared modules to maximize code reuse
- Option-based feature toggles for window managers via NixOS module system (`desktop.*.enable`)
- Overlay-based package customization with three distinct overlay categories (external flake, input-dependent, auto-discovered)

## Layers

**Flake Layer (Entry Point):**
- Purpose: Defines all inputs (dependencies), overlays, and system configurations
- Location: `flake.nix`
- Contains: Input declarations, overlay composition, system configuration declarations via `mkSystem`
- Depends on: External flake inputs (nixpkgs, home-manager, nix-darwin, ghostty, hyprland, nixvim, etc.)
- Used by: `nixos-rebuild switch --flake` and `darwin-rebuild switch --flake`

**System Builder Layer:**
- Purpose: Unified factory function that assembles a complete NixOS or nix-darwin system from modular components
- Location: `lib/mksystem.nix`
- Contains: Logic to resolve nixpkgs, select OS-specific user config, wire home-manager, apply overlays, inject module args
- Depends on: `nixpkgs`, `overlays`, `inputs` (passed via partial application)
- Used by: `flake.nix` to create each `nixosConfigurations.*` and `darwinConfigurations.*`

**Machine Layer:**
- Purpose: Hardware-specific and machine-specific system configuration (boot, networking, services, virtualization)
- Location: `machines/*.nix`
- Contains: Machine-specific imports (hardware, modules), networking, systemd services, VM guest tools
- Depends on: `hardware/*.nix`, `modules/*.nix`, `machines/vm-shared.nix`
- Used by: `lib/mksystem.nix` (auto-resolved from machine name)

**Hardware Layer:**
- Purpose: Low-level hardware definitions (kernel modules, filesystems, boot devices)
- Location: `hardware/*.nix`
- Contains: `boot.initrd.availableKernelModules`, `fileSystems`, `swapDevices`, CPU microcode
- Depends on: Nothing (leaf nodes)
- Used by: `machines/*.nix`

**Module Layer:**
- Purpose: Reusable NixOS modules with `options`/`config` pattern for feature toggles and VM integration
- Location: `modules/*.nix`, `modules/desktop/*.nix`
- Contains: Window manager toggles, Parallels guest integration, VMware guest, display profile management, clipboard bridging
- Depends on: nixpkgs, flake inputs (for hyprland package)
- Used by: `machines/*.nix` (imported directly or via `vm-shared.nix`)

**Overlay Layer:**
- Purpose: Package overrides, version pinning, and custom package injection into nixpkgs
- Location: `overlays/*.nix`
- Contains: Three categories: (1) external flake overlays, (2) input-dependent overlays requiring `{ inputs }`, (3) plain `final: prev:` auto-discovered overlays
- Depends on: Flake inputs (for unstable packages, nixvim, ghostty, fonts)
- Used by: Applied globally via `nixpkgs.overlays` in `lib/mksystem.nix`

**User OS Layer:**
- Purpose: System-level user account definitions and OS-specific system settings (not home-manager)
- Location: `users/<username>/nixos.nix` (Linux), `users/<username>/darwin.nix` (macOS)
- Contains: `users.users.*` definitions, shell assignment, groups, nix-homebrew config (Darwin), system preferences (Darwin)
- Depends on: Nothing (leaf config)
- Used by: `lib/mksystem.nix` (auto-resolved from user name and darwin flag)

**Home-Manager Layer:**
- Purpose: User-space configuration (packages, dotfiles, programs, services) shared across platforms
- Location: `users/<username>/home-manager.nix`
- Contains: Platform-conditional imports, program configurations, dotfile symlinks, user services
- Depends on: `users/common/` modules, `users/hyprland.nix`, dotfile source directories under `users/`
- Used by: `lib/mksystem.nix` via `home-manager.users.<user>`

**Common User Modules Layer:**
- Purpose: Shared home-manager configuration split into concern-based modules
- Location: `users/common/*.nix`
- Contains: `packages.nix` (all user packages), `shell.nix` (zsh config), `git.nix`, `editors.nix` (emacs), `terminal.nix` (alacritty, kitty, ghostty, wezterm), `desktop.nix` (firefox, picom), `services.nix` (ssh-agent), `dotfiles.nix` (doom emacs, elisp, etc.)
- Depends on: Dotfile source directories under `users/` (e.g., `users/doom.d/`, `users/ghostty/`, `users/kitty/`)
- Used by: `users/<username>/home-manager.nix` (imported via `users/common/default.nix`)

**Custom Packages Layer:**
- Purpose: Custom package derivations not available or needing customization from nixpkgs
- Location: `pkgs/*.nix`, `pkgs/hashicorp/`, `pkgs/parallels-tools/`
- Contains: Terraform binary package (HashiCorp generic builder), Parallels Tools kernel module build
- Depends on: nixpkgs build infrastructure
- Used by: `overlays/default.nix` (terraform, prl-tools kernel extension)

**Scripts Layer:**
- Purpose: Shell scripts for system management, display profiles, Hyprland helpers, Parallels integration
- Location: `scripts/*.sh`, `scripts/display-profiles/*.sh`, `scripts/hyprland/*.sh`
- Contains: Display profile switching, wallpaper setup, SSH key management, screenshot, clipboard fixes
- Depends on: System packages (xrandr, hyprctl, etc.)
- Used by: Referenced from Nix configs (session commands, systemd services, shell aliases), also run manually

## Data Flow

**System Build Flow:**

1. User invokes `sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"` (or `make switch NIXNAME=vm-aarch64-prl`)
2. Nix evaluates `flake.nix`, finds `nixosConfigurations.vm-aarch64-prl`
3. `mkSystem "vm-aarch64-prl" { system = "aarch64-linux"; user = "cipher"; }` is called
4. `lib/mksystem.nix` resolves: `machineConfig` = `machines/vm-aarch64-prl.nix`, `userOSConfig` = `users/cipher/nixos.nix`, `userHMConfig` = `users/cipher/home-manager.nix`
5. `nixpkgs.lib.nixosSystem` is called with modules list: overlays -> allowUnfree -> WSL (no-op) -> machineConfig -> userOSConfig -> home-manager -> module args
6. Machine config imports: `hardware/vm-aarch64-prl.nix`, `modules/parallels-guest.nix`, `modules/parallels-clipboard-x11-bridge.nix`, `machines/vm-shared.nix`
7. `vm-shared.nix` imports `modules/desktop/` (all WM toggles), sets shared Linux VM config
8. Home-manager evaluates `users/cipher/home-manager.nix`, which imports `users/common/default.nix` (all common modules) and conditionally `users/hyprland.nix`
9. Overlays are applied globally, making custom packages (dwm, terraform, picom, nixvim, unstable packages) available throughout

**Darwin Build Flow:**

1. User invokes `darwin-rebuild switch --flake ".#macbook-cipher"`
2. `mkSystem "macbook-cipher" { system = "aarch64-darwin"; user = "cipher"; darwin = true; extraModules = [ nix-homebrew ]; }` is called
3. `lib/mksystem.nix` uses `inputs.darwin.lib.darwinSystem` instead of `nixpkgs.lib.nixosSystem`
4. Uses `inputs.home-manager.darwinModules` instead of `nixosModules`
5. `machineConfig` = `machines/macbook-cipher.nix` (empty), `userOSConfig` = `users/cipher/darwin.nix` (macOS system prefs, homebrew, fonts)
6. Home-manager evaluates with `isDarwin = true`, disabling Linux modules and enabling Darwin-specific config
7. `nix-homebrew` module is injected via `extraModules`

**Overlay Resolution Order:**

1. External flake overlays: `emacs-overlay`, `claude-code`
2. Input-dependent overlays: `unstable-packages.nix`, `fonts.nix`, `nixvim.nix`, inline opencode overlay
3. Auto-discovered overlays: `default.nix`, `dwm.nix`, `firefox-hidpi.nix`, `picom.nix`, `tree-sitter.nix`, `vim-plugins.nix`

**State Management:**
- All state is declarative Nix configuration; no imperative state management
- `system.stateVersion` and `home.stateVersion` pin NixOS/home-manager compatibility versions
- Display profiles use `/tmp/.current-display-profile` as runtime state marker
- Parallels dynamic resolution uses `/tmp/.parallels-dynamic-resolution` marker file

## Key Abstractions

**mkSystem (Unified System Builder):**
- Purpose: Single function to create any system type (NixOS, Darwin, WSL)
- Location: `lib/mksystem.nix`
- Pattern: Partial application - first call provides closure (`{ nixpkgs, overlays, inputs }`), second call provides machine name, third provides config (`{ system, user, darwin?, wsl?, nixpkgsOverride?, extraModules? }`)
- Convention: Machine name maps to `machines/<name>.nix`, user name maps to `users/<user>/nixos.nix` or `users/<user>/darwin.nix` plus `users/<user>/home-manager.nix`

**Desktop Module System:**
- Purpose: Toggleable window manager installations via NixOS option system
- Location: `modules/desktop/*.nix`
- Pattern: Each WM defines `options.desktop.<wm>.enable = lib.mkEnableOption` and `config = lib.mkIf config.desktop.<wm>.enable { ... }`
- Examples: `modules/desktop/dwm.nix`, `modules/desktop/xmonad.nix`, `modules/desktop/awesome.nix`, `modules/desktop/hyprland.nix`
- Toggled in: `machines/vm-shared.nix` (`desktop.xmonad.enable = true;`, etc.)

**Common User Modules:**
- Purpose: Shared home-manager configuration split by concern for reuse across all users
- Location: `users/common/default.nix` (barrel import), individual modules in `users/common/*.nix`
- Pattern: Each module is a standard home-manager module `{ config, lib, pkgs, ... }: { ... }`. `default.nix` imports all sub-modules.
- Used by all user home-manager configs: `cipher`, `jrothberg`, `phantom`

**Overlay Categories:**
- Purpose: Three-tier overlay system separating external, input-dependent, and self-contained overlays
- Pattern:
  - External: Direct flake overlay attributes (e.g., `inputs.emacs-overlay.overlay`)
  - Input-dependent: Functions taking `{ inputs }` and returning `final: prev:` overlays (e.g., `overlays/unstable-packages.nix`)
  - Auto-discovered: Plain `final: prev:` overlays (e.g., `overlays/dwm.nix`, `overlays/picom.nix`)

**Platform Conditional Pattern:**
- Purpose: Share a single home-manager config across Linux and macOS
- Pattern: `home-manager.nix` receives `{ isWSL, isDarwin, inputs }` as outer args, uses `lib.mkIf isLinux` / `lib.mkIf isDarwin` guards throughout
- Examples: `users/cipher/home-manager.nix`, `users/common/shell.nix` (uses `pkgs.stdenv.isDarwin`)

## Entry Points

**Primary Entry Point (flake.nix):**
- Location: `flake.nix`
- Triggers: `nixos-rebuild switch --flake ".#<name>"`, `darwin-rebuild switch --flake ".#<name>"`, `make switch NIXNAME=<name>`
- Responsibilities: Declares all inputs, composes overlays, defines all system configurations via `mkSystem`

**Makefile (Convenience Wrapper):**
- Location: `Makefile`
- Triggers: `make switch`, `make test`, `make vm/copy`, `make vm/switch`, `make vm/bootstrap0`, `make vm/bootstrap`
- Responsibilities: Wraps `nixos-rebuild`, SSH-based VM management, ISO building

**Per-Machine Configs:**
- `machines/vm-aarch64-prl.nix`: Primary active config - ARM64 Parallels VM with time sync, clock skew detection
- `machines/vm-aarch64.nix`: ARM64 VMware VM with old kernel (6.1) and custom VMware guest module
- `machines/vm-intel.nix`: x86_64 VMware VM
- `machines/vm-shared.nix`: Shared Linux VM config (desktop WMs, boot, fonts, packages, services)
- `machines/vm-darwin.nix`: macOS Darwin config (empty - all config in user darwin.nix)
- `machines/macbook-cipher.nix`: macOS for cipher user (empty)
- `machines/macbook-phantom.nix`: macOS for phantom user (empty)

## Error Handling

**Strategy:** Nix's declarative evaluation model - errors are build-time failures, not runtime

**Patterns:**
- `lib.mkIf` / `lib.mkForce` guards prevent module evaluation errors on wrong platforms
- `lib.optionals (!pkgs.stdenv.isDarwin)` conditionally includes packages with platform-specific dependencies
- `disabledModules` pattern used to replace broken upstream modules (e.g., `virtualisation/parallels-guest.nix` in `machines/vm-aarch64-prl.nix`, `targets/darwin/linkapps.nix` in user home-manager configs)
- Shell scripts use `|| true` for non-critical failures and retry loops with backoff (e.g., `syncTimeScript` in `machines/vm-aarch64-prl.nix`)
- Systemd services use `Restart = "on-failure"` / `Restart = "always"` with `RestartSec` for resilient services

## Cross-Cutting Concerns

**Platform Abstraction:**
- `isDarwin` / `isLinux` / `isWSL` flags threaded through `lib/mksystem.nix` to all modules
- `pkgs.stdenv.isDarwin` used in `users/common/*.nix` for platform detection without explicit flag passing
- macOS Darwin configs (`users/*/darwin.nix`) handle Homebrew, system preferences, Nix Apps
- Linux configs handle X11/Wayland, window managers, systemd services

**Virtualization Integration:**
- Parallels: Custom `modules/parallels-guest.nix` (replaces upstream), `modules/parallels-clipboard-x11-bridge.nix`, `modules/parallels-display.nix`, `pkgs/parallels-tools/`
- VMware: Custom `modules/vmware-guest.nix` (replaces upstream for aarch64 compatibility)
- Both provide guest tools, clipboard sync, shared folders, display management

**Display Management:**
- `modules/display-profiles.nix` aggregates `display-x11.nix` and `parallels-display.nix`
- `scripts/display-profiles/display-switcher.sh` provides runtime profile switching (hidpi, retina, standard, present, ultrawide, auto)
- Shell aliases (`dp`, `dp-hidpi`, etc.) in `users/common/shell.nix` for quick access

**Package Version Pinning:**
- `overlays/unstable-packages.nix`: Pulls specific packages from nixpkgs-unstable (kitty, xmobar, awscli2)
- `overlays/dwm.nix`: Pins DWM to specific commit from external repo
- `overlays/picom.nix`: Pins picom to specific upstream commit
- `pkgs/terraform-bin.nix`: Pins Terraform to specific version via HashiCorp generic builder

---

*Architecture analysis: 2026-02-24*
