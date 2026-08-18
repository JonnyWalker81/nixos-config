# Technology Stack

**Analysis Date:** 2026-02-24

## Languages

**Primary:**
- Nix (Nix Expression Language) - All system configuration, package definitions, overlays, modules
- Bash/Shell - Scripts in `scripts/`, inline systemd services, Makefile targets

**Secondary:**
- Haskell - XMonad window manager config at `users/xmonad/xmonad.hs` (589 lines)
- Emacs Lisp - Doom Emacs config at `users/doom.d/` (26 files including `init.el`, `config.el`, `packages.el`)
- Lua - AwesomeWM config at `users/awesome/rc.lua`, Wezterm config at `users/wezterm/wezterm.lua`
- C - DWM window manager (external repo `JonnyWalker81/dwm`, built via `overlays/dwm.nix`)
- CSS - Waybar and Wofi styling at `users/waybar/style.css`, `users/wofi/style.css`

## Runtime

**Environment:**
- NixOS 25.05 (stable) - Primary Linux target via `nixpkgs.url = "github:nixos/nixpkgs/nixos-25.05"` in `flake.nix`
- nix-darwin - macOS target via `darwin.url = "github:lnl7/nix-darwin/master"` in `flake.nix`
- Nix with flakes enabled - `experimental-features = nix-command flakes` in `machines/vm-shared.nix`

**Package Manager:**
- Nix (latest version) - `pkgs.nixVersions.latest` in `machines/vm-shared.nix`
- Homebrew (macOS only) - Managed declaratively via `nix-homebrew` in `users/cipher/darwin.nix`, `users/phantom/darwin.nix`
- Lockfile: `flake.lock` (present, pinning all inputs)

## Frameworks

**Core:**
- Nix Flakes - Configuration entry point and dependency management (`flake.nix`)
- Home Manager (release-25.05) - User environment management, dotfile deployment (`users/common/`)
- NixOS modules - System-level configuration (`modules/`)
- nix-darwin modules - macOS system configuration (`machines/macbook-*.nix`, `users/*/darwin.nix`)

**Window Managers (all enabled in `machines/vm-shared.nix`):**
- DWM - Default session (`defaultSession = "none+dwm"`), custom build from GitHub (`overlays/dwm.nix`)
- XMonad - Haskell-based tiling WM (`modules/desktop/xmonad.nix`)
- AwesomeWM - Lua-based WM (`modules/desktop/awesome.nix`)
- Hyprland v0.50.1 - Wayland compositor from flake input (`modules/desktop/hyprland.nix`)

**Editors:**
- Doom Emacs - Primary editor config (`users/doom.d/`), evil-mode keybindings, 260-line `init.el`
- Neovim (nixvim) - Nix-managed config from external flake `JonnyWalker81/cipher-nixvim` (`overlays/nixvim.nix`)
- Multiple Neovim variants available: standard, LazyVim, kickstart (`users/nvim/`, `users/lazy/`, `users/lazyvim/`)

**Shell:**
- ZSH - Default shell for all users (`users/common/shell.nix`)
- Oh My ZSH - Plugin framework, `robbyrussell` theme
- Starship - Cross-shell prompt (`users/common/shell.nix`)
- zsh-vi-mode - Vi-mode keybindings

**Terminal Emulators:**
- Ghostty - Platform-conditional: flake build on Linux, unstable package on macOS (`overlays/unstable-packages.nix`)
- Kitty - From nixpkgs-unstable (`overlays/unstable-packages.nix`, config at `users/kitty/kitty.conf`)
- Alacritty - Configured via home-manager (`users/common/terminal.nix`)
- Wezterm - Lua-based config (`users/wezterm/wezterm.lua`)

**Build/Dev:**
- GNU Make - Build orchestration (`Makefile`, 87 lines)
- Docker - Enabled system-wide (`machines/vm-shared.nix`: `virtualisation.docker.enable = true`)
- direnv + nix-direnv - Per-project Nix environments (`users/common/shell.nix`)

## Key Dependencies

**Critical (from overlays and packages):**
- Terraform v1.14.3 - Custom binary package from HashiCorp releases (`pkgs/terraform-bin.nix`)
- terraform-ls - Language server for Terraform (`users/common/packages.nix`)
- Parallels Tools v26.1.1-57288 - VM guest tools, custom package with NixOS patches (`pkgs/parallels-tools/default.nix`)
- open-vm-tools - VMware guest support (`modules/vmware-guest.nix`)

**Development Languages (installed via `users/common/packages.nix`):**
- Go - From nixpkgs-unstable (`users/cipher/home-manager.nix`)
- Rust - Via `rustup` + `clang` (Linux only)
- Python 3 - With packages: epc, orjson, sexpdata, six, setuptools, paramiko, rapidfuzz
- Bun - JavaScript runtime (Linux only)
- Node.js - TypeScript/JavaScript tooling

**Language Servers & Formatters (`users/common/packages.nix`):**
- nil, nixd - Nix language servers
- nixpkgs-fmt, nixfmt-rfc-style - Nix formatters
- lua-language-server, stylua, luaformatter - Lua tooling
- typescript-language-server - TypeScript/JavaScript
- sqls, sql-formatter - SQL tooling
- shfmt - Shell formatter
- golangci-lint - Go linter
- tree-sitter - Incremental parsing (with ABI 13 grammar fixes in `overlays/tree-sitter.nix`)

**CLI Tools (`users/common/packages.nix` and `users/common/shell.nix`):**
- ripgrep, fd - Fast search
- fzf - Fuzzy finder
- bat - Syntax-highlighted cat
- eza - Modern ls replacement
- zoxide - Smart cd replacement
- jq - JSON processor
- delta, difftastic - Diff tools
- k9s - Kubernetes TUI
- yazi, xplr - File managers
- claude-code - AI coding assistant (from `inputs.claude-code` flake)
- opencode - AI coding assistant (from `inputs.opencode` flake)
- gh - GitHub CLI

**Infrastructure (`users/common/packages.nix`):**
- kubernetes, kubernetes-helm - Kubernetes management
- docker-compose - Container orchestration
- awscli2 - AWS CLI (from unstable, `overlays/unstable-packages.nix`)
- ssm-session-manager-plugin - AWS SSM SSH proxy
- postgresql_14 - PostgreSQL client
- cachix - Nix binary cache client (`machines/vm-shared.nix`)

**Fonts (installed in `machines/vm-shared.nix` and `users/common/packages.nix`):**
- DankMono - Custom proprietary font from private flake (`overlays/fonts.nix`)
- JetBrains Mono - Primary coding font (used in Alacritty config)
- Fira Code + symbols - Programming ligature font
- Cascadia Code, Victor Mono, Iosevka, Input, Monaspace - Additional coding fonts
- All Nerd Fonts - Icon/powerline fonts

## Configuration

**Environment:**
- No `.env` files - All configuration is declarative in Nix
- Session variables defined in `users/common/shell.nix` (EDITOR=nvim, GOPATH, GOPRIVATE, AWS_PAGER, etc.)
- SSH keys expected at `~/.ssh/id_ed25519`, `~/.ssh/id_rsa`, `~/.ssh/id_github`
- `~/.bash_join_db` sourced at shell init for database connection strings
- Display profiles managed via `/tmp/.current-display-profile` runtime state

**Build:**
- `flake.nix` - Entry point, defines all system configurations and overlay composition
- `lib/mksystem.nix` - Unified builder function for NixOS, nix-darwin, and WSL systems
- `Makefile` - Convenience targets: `switch`, `test`, `vm/copy`, `vm/switch`, `vm/bootstrap0`, `vm/bootstrap`, `iso/nixos.iso`

**Overlay Composition (applied in order in `flake.nix`):**
1. External flake overlays: `emacs-overlay`, `claude-code`
2. Input-dependent overlays: `overlays/unstable-packages.nix`, `overlays/fonts.nix`, `overlays/nixvim.nix`, opencode inline
3. Auto-discovered overlays: `overlays/default.nix`, `overlays/dwm.nix`, `overlays/firefox-hidpi.nix`, `overlays/picom.nix`, `overlays/tree-sitter.nix`, `overlays/vim-plugins.nix`

## Platform Requirements

**Development (NixOS VMs):**
- Nix with flakes enabled
- SSH access for VM provisioning (password auth enabled)
- Docker daemon running
- X11 (DWM/XMonad/Awesome) or Wayland (Hyprland) display server
- SDDM display manager
- Linux kernel 6.6 (`machines/vm-shared.nix`: `boot.kernelPackages = pkgs.linuxPackages_6_6`)

**Production/Deployment Targets:**
- `vm-aarch64-prl` - ARM64 NixOS in Parallels Desktop on macOS (primary dev environment)
- `vm-aarch64` - ARM64 NixOS in VMware Fusion (older kernel 6.1 for compatibility)
- `vm-intel` - x86_64 NixOS in VMware Fusion
- `macbook-cipher` - aarch64 macOS with nix-darwin
- `macbook-phantom` - aarch64 macOS with nix-darwin
- `vm-darwin` - x86_64 macOS with nix-darwin

**VM Performance Tuning (`machines/vm-shared.nix`):**
- zram swap enabled (25% of memory)
- vm.swappiness = 1
- vm.vfs_cache_pressure = 50
- Mesa + VDPAU/VAAPI for GPU acceleration
- systemd-boot EFI bootloader

## User Accounts

**cipher:**
- Primary user for Parallels VMs and macOS (`users/cipher/`)
- Email: `jon@join.build`
- Shell: ZSH
- Groups: docker, wheel
- Full Linux + Darwin configuration with Hyprland support

**jrothberg:**
- Secondary user for Intel VMs (`users/jrothberg/`)
- Email: `jrothberg@bluebeam.com`
- Shell: ZSH (implied by common config)
- Linux-only configuration

**phantom:**
- macOS-only user (`users/phantom/`)
- Email: placeholder
- Darwin-only configuration with Homebrew

---

*Stack analysis: 2026-02-24*
