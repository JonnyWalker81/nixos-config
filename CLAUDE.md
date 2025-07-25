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

## Makefile Default Values

When using Makefile commands without parameters:
- `NIXNAME`: defaults to `vm-intel`
- `NIXUSER`: defaults to `jrothberg`
- `NIXPORT`: defaults to `22`
```