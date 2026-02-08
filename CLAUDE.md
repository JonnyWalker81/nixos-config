# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a NixOS configuration repository that uses Nix Flakes. It manages system configurations for both NixOS (Linux) and nix-darwin (macOS) systems, with a focus on virtual machine environments. The repository uses a modular design pattern to organize configurations across different machines, hardware, and users.

## System Knowledge

- Screenshots are typically saved in `~/Pictures` on Linux and `~/Desktop` on macOS

## Build and Configuration Commands

### Building and Applying Configuration

To rebuild and switch to a configuration:

```bash
# Rebuild and switch to a specific configuration
sudo nixos-rebuild switch --flake ".#<configuration-name>"

# Using the Makefile (more convenient)
make switch NIXNAME=<configuration-name>

# Examples for specific configurations
sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"  # ARM64 Parallels VM
sudo nixos-rebuild switch --flake ".#vm-aarch64"      # ARM64 VM (older kernel for VMware)
sudo nixos-rebuild switch --flake ".#vm-intel"        # x86_64 Intel VM
```

### Testing Configuration

```bash
# Test a configuration without applying it
make test NIXNAME=<configuration-name>

# Test build a configuration directly
sudo nixos-rebuild test --flake ".#<configuration-name>"
```

### VM Management (via Makefile)

For managing VM configurations:

```bash
# Copy configuration to VM
make vm/copy NIXADDR=<vm-ip-address> NIXPORT=<ssh-port> NIXUSER=<username>

# Apply configuration on VM
make vm/switch NIXADDR=<vm-ip-address> NIXPORT=<ssh-port> NIXUSER=<username> NIXNAME=<config-name>

# Bootstrap a new VM (two-step process)
make vm/bootstrap0 NIXADDR=<vm-ip-address> NIXPORT=<ssh-port> NIXBLOCKDEVICE=<device>
make vm/bootstrap NIXADDR=<vm-ip-address> NIXPORT=<ssh-port>

# Default values: NIXPORT=22, NIXUSER=jrothberg, NIXNAME=vm-intel
```

### Building ISO Images

```bash
# Build a NixOS ISO image
make iso/nixos.iso
```

## Architecture and Structure

### Key Components

1. **Flake Structure**:
   - `flake.nix`: The main entry point, defining inputs (dependencies) and outputs (configurations)
   - Uses helper functions like `mkSystem`, `mkVM`, and `mkVMDarwin` to generate system configurations

2. **Directory Organization**:
   - `machines/`: Contains machine-specific NixOS configurations
   - `hardware/`: Hardware-specific NixOS modules
   - `users/`: User-specific configurations (NixOS and home-manager)
   - `lib/`: Helper functions for creating configurations (`mksystem.nix`)
   - `overlays/`: Package overrides and customizations
   - `modules/`: Reusable NixOS modules
   - `pkgs/`: Custom packages (e.g., terraform-bin)

3. **Configuration Pattern**:
   - Each machine configuration imports relevant hardware and user configurations
   - Uses home-manager for user-specific configuration
   - Uses overlays for customizing and overriding packages

### System Generation

The `mksystem.nix` library function creates a NixOS system configuration with:
- System architecture specification
- User configuration
- Home-manager integration
- Support for WSL and Darwin variants

## Flake Configuration Details

The flake defines several system configurations:
- `vm-aarch64-prl`: ARM64 Parallels VM configuration
- `vm-aarch64`: ARM64 VM using older kernel (6.1) for VMware compatibility
- `vm-intel`: x86_64 Intel VM configuration  
- `vm-darwin`: macOS Darwin configuration

### Key Inputs and Dependencies

- **nixpkgs**: Stable NixOS 25.05 channel
- **nixpkgs-unstable**: Latest packages for specific tools (Go, Kitty, etc.)
- **nixpkgs-old-kernel**: Older kernel (6.1) for VMware compatibility
- **home-manager**: User environment management
- **nix-darwin**: macOS system configuration
- **nixvim**: Custom Neovim configuration
- **ghostty**: Terminal emulator
- **zen-browser**: Alternative browser
- **hyprland**: Wayland compositor

### Package Overlays

The configuration uses overlays to:
- Override specific package versions (picom, tree-sitter grammars)
- Pull packages from unstable (kitty, awscli2, go_1_23, fzf, etc.)
- Apply custom patches and configurations
- Integrate third-party flake packages (ghostty, zen-browser)

Common overlay patterns:
```nix
# Pull package from unstable
kitty = unstable.kitty;

# Override with custom derivation
picom = prev.picom.overrideAttrs (old: {
  src = prev.fetchFromGitHub { ... };
});

# Use specific nixpkgs input
kernel = inputs.nixpkgs-old-kernel.legacyPackages.${system}.kernel;
```

## Working with the Codebase

### Available System Configurations

Check `flake.nix` outputs section for all available configurations. Current systems:
- `nixosConfigurations.*`: Linux systems
- `darwinConfigurations.*`: macOS systems

### DWM (Window Manager)

The DWM source code lives in a **separate repository** at `~/Repositories/dwm` (GitHub: `JonnyWalker81/dwm`, branch `xmonad-parity`). It is **not** part of this nixos-config repo directly — it is pulled in via `overlays/dwm.nix` using `fetchFromGitHub` with a pinned commit hash.

**MANDATORY workflow for ANY DWM changes -- you MUST follow ALL steps:**

1. Make changes locally in `~/Repositories/dwm` (e.g., edit `config.h`, `dwm.c`, etc.)
2. Commit the changes in `~/Repositories/dwm`
3. **Push** the changes to the remote: `git push origin xmonad-parity` (from `~/Repositories/dwm`)
4. Get the new commit hash: `git rev-parse HEAD` (from `~/Repositories/dwm`)
5. Update `overlays/dwm.nix` in **this** repo (`~/nixos-config`):
   - Set `rev` to the new commit hash from step 4
   - Set `sha256` to an empty string `""`
6. Rebuild NixOS: `sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"` (from `~/nixos-config`)
   - The build will fail with a hash mismatch error showing the correct `sha256`
   - Copy the correct `sha256` from the error output (the `got:` line)
7. Update `overlays/dwm.nix` again with the correct `sha256` from the error
8. Rebuild NixOS again -- this time it will succeed

**CRITICAL:** Changes to `~/Repositories/dwm` MUST be pushed to GitHub before rebuilding NixOS, because the Nix build fetches the source from GitHub, not from the local directory. Skipping the push will cause the build to use stale code.

### Modifying Configurations

1. **Adding a new machine**:
   - Create configuration file in `machines/` directory
   - Add corresponding hardware file in `hardware/` directory  
   - Reference in `flake.nix` outputs section using `mkSystem` or `mkVM`

2. **User configuration structure**:
   - `users/<username>/nixos.nix`: NixOS system-level user settings
   - `users/<username>/darwin.nix`: macOS system-level user settings  
   - `users/<username>/home-manager.nix`: User packages, dotfiles, and home environment

3. **Adding custom packages**:
   - Define in `pkgs/` directory following existing patterns
   - Reference in overlays or directly in user/machine configurations
   - For Terraform providers, see `pkgs/terraform-bin.nix` pattern

4. **Configuration workflow**:
   - Make changes to relevant nix files
   - Test with `make test NIXNAME=<config>` before applying
   - Apply with `make switch NIXNAME=<config>` locally
   - For VMs: `make vm/copy` then `make vm/switch`

### Development Environment

- **Neovim configurations**: Multiple variants available
  - `nvim`: Standard configuration
  - `lazy`: LazyVim-based configuration
  - `lazyvim`: Full LazyVim setup
  - `nixvim`: Nix-based Neovim configuration
  - `nvim-kickstart`: Kickstart.nvim configuration
- **Emacs**: Doom Emacs configuration with Rust support
- **Shell configurations**: ZSH config in `users/config.zsh`
- **Terminal emulators**: Ghostty, Kitty, Wezterm configurations available
- **Window managers**: XMonad, Hyprland configurations included

### Testing and Development Scripts

- `run.sh`: Docker script for testing LazyVim configuration
- `test_emacs.sh`: Emacs testing script
- `test_interactive.sh`: Interactive testing script

## Git Commit Policy

**All commits MUST be atomic and use semantic commit messages.**

### Semantic Commit Format

```
<type>(<scope>): <short description>

[optional body with more detail]
```

### Commit Types

- `feat`: New feature or functionality
- `fix`: Bug fix
- `refactor`: Code restructuring without behavior change
- `docs`: Documentation changes
- `style`: Formatting, whitespace, etc. (no code logic change)
- `chore`: Maintenance tasks (dependency updates, build config, etc.)
- `perf`: Performance improvements
- `ci`: CI/CD changes

### Scope Examples

- `doom`: Doom Emacs configuration
- `dwm`: DWM window manager
- `xmonad`: XMonad window manager
- `awesome`: AwesomeWM
- `flake`: Nix flake inputs/outputs
- `machines`: Machine-specific configs
- `home-manager`: Home-manager user configs
- `overlays`: Nix overlays
- `scripts`: Shell scripts

### Atomic Commit Rules

1. **One logical change per commit** -- do not mix unrelated changes
2. **Each commit should leave the system in a buildable state** when possible
3. **Group related file changes together** -- e.g., a new module and its integration belong in one commit
4. **Separate concerns** -- Doom Emacs changes, window manager changes, Nix system changes, etc. should be in different commits
5. **Lock file updates** get their own commit (e.g., `chore(flake): update flake.lock`)

### Auto-Commit After Validated Changes

**After making changes to NixOS/nix configuration files, always follow this workflow:**

1. Make the requested changes to the configuration files
2. Tell the user to rebuild (e.g., `sudo nixos-rebuild switch --flake ".#vm-aarch64-prl"`)
3. **Wait for the user to confirm the rebuild succeeded** -- do NOT commit until the user explicitly confirms
4. Once the user confirms the rebuild was successful, **immediately create an atomic semantic commit** for the validated changes
5. Do NOT ask the user whether to commit -- just do it automatically after they confirm the rebuild worked

**This applies to all configuration changes** including but not limited to:
- Nix files (`*.nix`): flake.nix, machine configs, modules, overlays, packages
- Doom Emacs files (`*.el`): config.el, init.el, packages.el, and all config-*.el modules
- Window manager configs: XMonad, DWM, AwesomeWM, Hyprland
- Shell configs, scripts, dotfiles managed by home-manager
- Any other file that is part of the system configuration

**Exception:** If changes span multiple unrelated concerns, split them into multiple atomic commits (per the atomic commit rules above).

## Makefile Default Values

When using Makefile commands without parameters:
- `NIXNAME`: defaults to `vm-intel`
- `NIXUSER`: defaults to `jrothberg`
- `NIXPORT`: defaults to `22`