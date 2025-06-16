# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is a NixOS configuration repository that uses Nix Flakes. It manages system configurations for both NixOS (Linux) and nix-darwin (macOS) systems, with a focus on virtual machine environments. The repository uses a modular design pattern to organize configurations across different machines, hardware, and users.

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
sudo nixos-rebuild switch --flake ".#vm-intel"        # x86_64 Intel VM
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
```

### Testing Configuration

```bash
# Test a configuration without applying it
make test NIXNAME=<configuration-name>
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
   - `lib/`: Helper functions for creating configurations
   - `overlays/`: Package overrides and customizations
   - `modules/`: Reusable NixOS modules
   - `pkgs/`: Custom packages

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

## Working with the Codebase

When modifying configurations:

1. **Adding a new machine**:
   - Create a new file in `machines/` directory
   - Reference appropriate hardware configuration
   - Configure system settings specific to the machine

2. **Modifying user configuration**:
   - Edit files in `users/<username>/` directory
   - System-specific settings go in `nixos.nix` or `darwin.nix`
   - User-specific packages and dotfiles go in `home-manager.nix`

3. **Adding custom packages**:
   - Add new package definitions to `pkgs/` directory
   - Reference these packages in machine or user configurations

4. **Applying changes**:
   - Use `make switch` to apply configuration locally
   - For VMs, use `make vm/copy` followed by `make vm/switch`