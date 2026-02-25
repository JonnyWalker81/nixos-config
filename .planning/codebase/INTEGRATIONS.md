# External Integrations

**Analysis Date:** 2026-02-24

## Flake Inputs (External Nix Dependencies)

All external dependencies are pinned in `flake.nix` and locked in `flake.lock`.

**Core Nixpkgs Channels:**
- `nixpkgs` (nixos-25.05 stable) - Primary package source
  - URL: `github:nixos/nixpkgs/nixos-25.05`
- `nixpkgs-unstable` (nixos-unstable) - Bleeding-edge packages (kitty, xmobar, awscli2, ghostty-bin)
  - URL: `github:nixos/nixpkgs/nixos-unstable`
  - Packages pulled via: `overlays/unstable-packages.nix`
- `nixpkgs-old-kernel` - Pinned commit for Linux kernel 6.1 (VMware Fusion compatibility)
  - URL: `github:nixos/nixpkgs/bacbfd713b4781a4a82c1f390f8fe21ae3b8b95b`
  - Used by: `nixosConfigurations.vm-aarch64` in `flake.nix`

**System Frameworks:**
- `home-manager` (release-25.05) - User environment and dotfile management
  - URL: `github:nix-community/home-manager/release-25.05`
  - Follows: `nixpkgs`
- `darwin` (nix-darwin) - macOS system configuration
  - URL: `github:lnl7/nix-darwin/master`
  - Follows: `nixpkgs-unstable`
- `nix-homebrew` - Declarative Homebrew management on macOS
  - URL: `github:zhaofengli-wip/nix-homebrew`
  - Used in: `users/cipher/darwin.nix`, `users/phantom/darwin.nix` (via `extraModules`)

**Application Flakes:**
- `ghostty` - Terminal emulator
  - URL: `github:ghostty-org/ghostty`
  - Integration: `overlays/unstable-packages.nix` (Linux: flake build, macOS: unstable package)
- `hyprland` v0.50.1 - Wayland compositor
  - URL: `github:hyprwm/Hyprland?ref=v0.50.1`
  - Integration: `modules/desktop/hyprland.nix`
- `nixvim` - Custom Neovim configuration
  - URL: `github:JonnyWalker81/cipher-nixvim`
  - Follows: `nixpkgs-unstable`
  - Integration: `overlays/nixvim.nix`, installed in `users/common/packages.nix` (Linux only)
- `emacs-overlay` - Latest Emacs builds
  - URL: `github:nix-community/emacs-overlay`
  - Integration: Applied as overlay in `flake.nix`, provides `emacs-unstable` for macOS (`users/cipher/darwin.nix`)
- `claude-code` - Claude AI coding assistant
  - URL: `github:sadjow/claude-code-nix`
  - Integration: Overlay applied in `flake.nix`, package in `users/common/packages.nix`
- `opencode` - AI coding assistant
  - URL: `github:anomalyco/opencode`
  - Follows: `nixpkgs-unstable`
  - Integration: Inline overlay in `flake.nix`, package in `users/common/packages.nix`
- `dankmono` - Proprietary programming font
  - URL: `github:JonnyWalker81/dankmono-font`
  - Follows: `nixpkgs`
  - Integration: `overlays/fonts.nix`

**Homebrew Sources (macOS, non-flake):**
- `homebrew-core` - `github:homebrew/homebrew-core` (flake = false)
- `homebrew-cask` - `github:homebrew/homebrew-cask` (flake = false)
- Tap management: `users/cipher/darwin.nix` lines 15-19

## APIs & External Services

**Time Synchronization:**
- timeapi.io - HTTP time API for VM clock sync after Parallels freeze/unfreeze
  - Endpoint: `https://timeapi.io/api/v1/time/current/unix`
  - Client: curl + jq in bash script
  - Used by: `machines/vm-aarch64-prl.nix` (systemd services `sync-time` and `sync-time-resume`)
  - Auth: None required (public API)
  - Retry logic: 8 retries with 5s delay, 30s network wait, double-fetch validation

**NTP:**
- pool.ntp.org, time.nist.gov - Standard NTP time servers
  - Configured in: `machines/vm-shared.nix` (`networking.timeServers`)
  - Service: `services.timesyncd.enable = true` (disabled on Parallels VMs where custom sync is used)

**Nix Binary Cache:**
- cache.nixos.org - Official NixOS binary cache
  - Configured in: `machines/vm-shared.nix`
  - Key: `cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY=`
  - Client: `cachix` installed in `machines/vm-shared.nix`

**HashiCorp Releases:**
- releases.hashicorp.com - Terraform binary downloads
  - URL pattern: `https://releases.hashicorp.com/${name}/${version}/${name}_${version}_${goSystem}.zip`
  - Used by: `pkgs/hashicorp/generic.nix`
  - Current: Terraform v1.14.3 (`pkgs/terraform-bin.nix`)

**Parallels Desktop:**
- download.parallels.com - Parallels Tools DMG download
  - URL: `https://download.parallels.com/desktop/v26/26.1.1-57288/ParallelsDesktop-26.1.1-57288.dmg`
  - Used by: `pkgs/parallels-tools/default.nix`
  - Extracted artifact: `prl-tools-lin-arm.iso` from DMG

## Data Storage

**Databases:**
- PostgreSQL 14 - Client installed via `users/common/packages.nix` (`pkgs.postgresql_14`)
  - Connection config: `~/.bash_join_db` (sourced at shell init in `users/common/shell.nix`)
  - Tools: pgmanage, pgadmin4, rainfrog (TUI), sqls (language server)
  - psqlrc configured in: `users/common/dotfiles.nix`

**File Storage:**
- Local filesystem only (ext4 root, vfat boot)
- Parallels shared folders mounted at `/media/psf` via `prl_fsd` FUSE (`modules/parallels-guest.nix`)
- VMware shared folders mounted at `/host` via `vmhgfs-fuse` (`machines/vm-aarch64.nix`, `machines/vm-intel.nix`)

**Caching:**
- Nix store (`/nix/store`) - All packages cached locally
- Binary cache: `cache.nixos.org` (substituters in `machines/vm-shared.nix`)
- Git credential cache: 10-hour timeout (`users/common/git.nix`: `credential.helper = "cache --timeout 36000"`)

## Authentication & Identity

**SSH:**
- ed25519 keys - Primary key type (`~/.ssh/id_ed25519`)
  - GitHub match block: `users/common/git.nix` (user: `jonnywalker81`)
  - Bluebeam SCM: `scm.bluebeam.com:7999` with `+ssh-rsa` algorithm support
  - SSH agent: systemd-managed on Linux (`users/common/services.nix`), launchd-managed on macOS
  - Agent forwarding enabled globally, IdentitiesOnly enabled
  - SSH over AWS SSM: `ProxyCommand` for `i-*` and `mi-*` hosts (`users/common/git.nix`)
- RSA keys - Fallback (`~/.ssh/id_rsa`)
- GitHub-specific key - `~/.ssh/id_github`

**Git Credential Helpers:**
- Linux: `cache --timeout 36000` (10 hours) - `users/common/git.nix`
- macOS: `osxkeychain` - `users/cipher/home-manager.nix`, `users/phantom/home-manager.nix`

**macOS Keychain:**
- SSH keys stored via `UseKeychain yes` and `AddKeysToAgent yes` (`users/cipher/home-manager.nix`)

**GPG:**
- gnupg + pinentry installed on Linux (`users/common/packages.nix`)
- Used for: pass (password store), git signing (implied by commented email config)

**Email (DISABLED):**
- Gmail IMAP via mbsync was configured but disabled to prevent GPG password prompts
  - Accounts: `jon@join.build` (cipher), `jrothberg@bluebeam.com` (jrothberg)
  - Would use: isync (mbsync), msmtp, pass for passwords
  - Config locations: commented out in `users/cipher/home-manager.nix`, `users/jrothberg/home-manager.nix`

## AWS Integration

**CLI:**
- awscli2 from nixpkgs-unstable (`overlays/unstable-packages.nix`)
- AWS_PAGER set to empty string (`users/common/shell.nix`)
- `ap` shell alias for interactive AWS profile selection via fzf

**SSM Session Manager:**
- ssm-session-manager-plugin installed (`users/common/packages.nix`)
- SSH ProxyCommand configured for `i-*` and `mi-*` instance IDs (`users/common/git.nix`)
  - Pattern: `aws ssm start-session --target %h --document-name AWS-StartSSHSession --parameters 'portNumber=%p'`

**Terraform:**
- Custom binary from HashiCorp releases v1.14.3 (`pkgs/terraform-bin.nix`)
- terraform-ls language server installed
- Workspace switching alias `sw` via fzf (`users/common/shell.nix`)
- Doom Emacs terraform module enabled (`users/doom.d/init.el`)

## GitHub Integration

**Source Code Fetching (Nix build-time):**
- `JonnyWalker81/dwm` (branch: `xmonad-parity`) - Custom DWM build (`overlays/dwm.nix`)
- `yshui/picom` - Custom picom compositor build (`overlays/picom.nix`)
- `JonnyWalker81/cipher-nixvim` - Neovim config (`flake.nix` input)
- `JonnyWalker81/dankmono-font` - Proprietary font (`flake.nix` input)
- `copilot-emacs/copilot.el` - GitHub Copilot for Emacs (`users/doom.d/packages.el`)

**GitHub CLI:**
- `gh` installed on Linux (`users/common/packages.nix`)
- Magit + Forge in Doom Emacs (`users/doom.d/init.el`: `(magit +forge)`)

**Copilot:**
- GitHub Copilot Emacs integration (`users/doom.d/packages.el`: `copilot` package)
- CopilotChat.nvim (with check disabled) - `overlays/vim-plugins.nix`

## Monitoring & Observability

**Error Tracking:**
- None (no external error tracking service)

**Logs:**
- systemd journal - capped at 100M (`machines/vm-shared.nix`: `services.journald.extraConfig = "SystemMaxUse=100M"`)
- Custom logging via `systemd-cat` in time sync scripts (`machines/vm-aarch64-prl.nix`)
- Tags: `sync-time`, `clock-skew-detector`

**System Monitoring:**
- neofetch - Displayed on terminal open (`users/common/shell.nix`)
- acpi - Battery/power status (`users/common/packages.nix`)
- procs - Process viewer (`users/common/packages.nix`)
- bottom - System monitor (`users/common/packages.nix`)
- k9s - Kubernetes cluster monitoring (`users/common/packages.nix`)

## CI/CD & Deployment

**Hosting:**
- Local VMs (Parallels Desktop, VMware Fusion) - No cloud hosting
- macOS bare-metal (nix-darwin)

**CI Pipeline:**
- None configured in this repository

**Deployment Method:**
- SSH + rsync for VM deployment (`Makefile`: `vm/copy` target)
- `nixos-rebuild switch --flake` for applying configurations
- `darwin-rebuild switch --flake` for macOS
- VM bootstrap: disk partitioning + NixOS install via SSH (`Makefile`: `vm/bootstrap0`, `vm/bootstrap`)

## Virtualization

**Parallels Desktop (Primary):**
- Custom parallels-tools package v26.1.1 (`pkgs/parallels-tools/default.nix`)
- Driverless virtio-vsock protocol (v26.1+)
- Services: prltoolsd, prl-fsd-mount, prlshprint, prlcc, prldnd, prlcp, prlshprof (`modules/parallels-guest.nix`)
- Clipboard: X11-native prlcp with restart scripts (`modules/parallels-clipboard-x11-bridge.nix`)
- Display: Dynamic resolution monitoring, custom modelines (`modules/parallels-display.nix`)
- Clock: Custom skew detector + timeapi.io sync (`machines/vm-aarch64-prl.nix`)

**VMware Fusion:**
- open-vm-tools (`modules/vmware-guest.nix`)
- vmhgfs-fuse shared folders
- Custom module with aarch64 support patches

**Docker:**
- Enabled system-wide (`machines/vm-shared.nix`)
- docker-compose installed (`users/common/packages.nix`)

**QEMU:**
- Installed on Linux (`users/common/packages.nix`)

## Search Engines (Firefox)

Configured in `users/common/desktop.nix`:
- Google (default, alias: `@g`)
- Nix Packages search (`search.nixos.org/packages`, alias: `@np`)
- NixOS Wiki (`nixos.wiki`, alias: `@nw`)
- Searx instance (`searx.aicampground.com`, alias: `@searx`)

## Webhooks & Callbacks

**Incoming:**
- None

**Outgoing:**
- None

## Environment Configuration

**Required runtime files:**
- `~/.bash_join_db` - Database connection strings (sourced in `users/common/shell.nix`)
- `~/.ssh/id_ed25519` - Primary SSH key
- `~/.ssh/known_hosts` - SSH host verification

**Secrets management:**
- `pass` (password store) - Referenced in commented email config
- gnupg/pinentry - Encryption backend
- macOS Keychain - SSH key storage on Darwin
- Git credential cache - 10h timeout on Linux

**No external secret stores** (no Vault, no AWS Secrets Manager, no .env files).

## External Repositories

**DWM Source:**
- Repository: `github:JonnyWalker81/dwm` (branch: `xmonad-parity`)
- Local clone expected at: `~/Repositories/dwm`
- Pinned commit: `552206f5086a6f25fc5bbbfb3e5abb49484dba27` (`overlays/dwm.nix`)
- Build requires: push to GitHub before NixOS rebuild (Nix fetches from remote)

**Nixvim Config:**
- Repository: `github:JonnyWalker81/cipher-nixvim`
- Managed entirely as flake input

**Tree-sitter-indent (Doom Emacs):**
- Local repository: `~/Repositories/tree-sitter-indent` (`users/doom.d/packages.el`)

---

*Integration audit: 2026-02-24*
