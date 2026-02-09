{ config, lib, pkgs, ... }:

let
  isDarwin = pkgs.stdenv.isDarwin;

  zoxide = pkgs.zoxide;
  zoxideBin = zoxide + "/bin/zoxide";

  manpager = (pkgs.writeShellScriptBin "manpager" (if isDarwin then ''
    sh -c 'col -bx | bat -l man -p'
  '' else ''
    cat "$1" | col -bx | bat --language man --style plain
  ''));
in {
  home.sessionPath =
    [ "$HOME/.claude/local" "$HOME/.local/bin" "$HOME/bin" "$HOME/.cargo/bin" ];

  home.sessionVariables = {
    LANG = "en_US.UTF-8";
    LC_CTYPE = "en_US.UTF-8";
    LC_ALL = "en_US.UTF-8";
    EDITOR = "nvim";
    VISUAL = "nvim";
    PAGER = "less -FirSwX";
    MANPAGER = "${manpager}/bin/manpager";

    # Firefox/Mozilla HiDPI scaling
    MOZ_ENABLE_WAYLAND = "1";
    MOZ_USE_XINPUT2 = "1";
    MOZ_DBUS_REMOTE = "1";

    # Development
    GOPATH = "\${HOME}";
    GOPRIVATE = "github.com/JoinCAD,github.com/JonnyWalker81";
    PKG_CONFIG_PATH = "${pkgs.openssl.dev}/lib/pkgconfig";
    LIBVIRT_DEFAULT_URI = "qemu:///system";
    AWS_PAGER = "";
    TERM = "xterm-256color";
  } // lib.optionalAttrs (!isDarwin) {
    # Linux-only: Set SSH_AUTH_SOCK for systemd ssh-agent
    SSH_AUTH_SOCK = "/run/user/1000/ssh-agent";
  };

  home.shellAliases = {
    code = "code --enable-features=UseOzonePlatform --ozone-platform=wayland";

    # Display profile shortcuts
    dp =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
    dp-hidpi =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
    dp-retina =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
    dp-standard =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
    dp-present =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
    dp-ultrawide =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
    dp-auto =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
    dp-current =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
    dp-list =
      "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
    prl-display = "parallels-display-info";

    # SSH key management
    ssh-keys = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh";
    ssh-add-keys = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh add";
    ssh-status = "/home/cipher/nixos-config/scripts/ssh-key-manager.sh status";
  };

  programs.zsh = {
    enable = true;
    shellAliases = {
      ll = "eza -l";
      l = "eza -lah";
      rebuild =
        "sudo NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 nixos-rebuild switch --flake .#vm-aarch64";
      rp = "sudo nixos-rebuild switch --flake .#vm-aarch64-prl";
      ri = "sudo nixos-rebuild switch --flake .#vm-intel";
      rdd = "sudo darwin-rebuild switch --flake .#vm-darwin";
      f = "history | fzf --sort --exact | sh";
      bc = "git branch | grep '*' | awk '{print $2}' | pbcopy";

      ap = ''
        export AWS_PROFILE=$(aws configure list-profiles | fzf --prompt "Choose active AWS profile:")'';
      sw = ''
        terraform workspace list | fzf --prompt "Choose workspace:" | xargs -r terraform workspace select'';
      ka = "ps -aux | fzf | awk '{print $2}' | xargs -r kill -9";

      cd = "z";
      ys = "yarn install && yarn start";
      ff = ''
        cd "$(find $(git rev-parse --show-toplevel 2>/dev/null || pwd) -mindepth 1 -type d | fzf)"'';

      # Display profile management
      dp =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh";
      dp-hidpi =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh hidpi";
      dp-retina =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh retina";
      dp-standard =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh standard";
      dp-present =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh present";
      dp-ultrawide =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh ultrawide";
      dp-auto =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh auto";
      dp-current =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh current";
      dp-list =
        "/home/cipher/nixos-config/scripts/display-profiles/display-switcher.sh list";
      prl-display = "parallels-display-info";

      # SSH agent management
      ssh-cleanup = "/home/cipher/nixos-config/scripts/cleanup-ssh-agents.sh";

      # CodeRabbit CLI
      cr = "coderabbit";
    };

    autosuggestion = { enable = true; };
    enableCompletion = true;
    syntaxHighlighting.enable = true;
    sessionVariables = {
      # Only ZSH-specific variables here, general ones are in home.sessionVariables
      ZSH_TMUX_AUTOSTART = "true";
      ZSH_TMUX_AUTOCONNECT = "true";
    };

    initContent = ''
      # Ensure claude is in PATH
      export PATH="$HOME/.claude/local:$PATH"

      source ${pkgs.zsh-vi-mode}/share/zsh-vi-mode/zsh-vi-mode.plugin.zsh
      eval "$(${zoxideBin} init zsh)"

      # Prepend home-manager bin to PATH for Darwin
      if [[ "$(uname)" == "Darwin" ]]; then
        export PATH="$HOME/.local/state/nix/profiles/home-manager/home-path/bin:$PATH"

        # macOS SSH agent setup - use the system SSH agent
        # The SSH agent on macOS is managed by launchd and the socket is dynamic
        # No need to set SSH_AUTH_SOCK as macOS handles it automatically
      fi

      # Ensure ~/.local/bin is in PATH for Linux (in case session vars aren't loaded properly)
      if [[ "$(uname)" == "Linux" ]]; then
        export PATH="$HOME/.local/bin:$PATH"
      fi

      # Import SSH environment from systemd user environment (Linux only)
      if [[ "$(uname)" == "Linux" ]] && command -v systemctl >/dev/null 2>&1; then
        export SSH_AUTH_SOCK="/run/user/1000/ssh-agent"
      fi

      # SSH key management - only load keys if SSH agent is available and keys aren't already loaded
      if [[ -n "$SSH_AUTH_SOCK" ]] && command -v ssh-add >/dev/null 2>&1; then
        # Check if keys are already loaded
        if ! ssh-add -l >/dev/null 2>&1; then
          # Load SSH keys if they exist
          if [[ "$(uname)" == "Darwin" ]]; then
            # On macOS, use --apple-use-keychain to store passphrase in keychain
            for key in ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_github; do
              if [[ -f "$key" ]]; then
                ssh-add --apple-use-keychain "$key" >/dev/null 2>&1 || true
              fi
            done
          else
            # On Linux, just add the keys normally
            for key in ~/.ssh/id_ed25519 ~/.ssh/id_rsa ~/.ssh/id_github; do
              if [[ -f "$key" ]]; then
                ssh-add "$key" >/dev/null 2>&1 || true
              fi
            done
          fi
        fi
      fi

      source ~/.bash_join_db

      [[ ! -r /home/cipher/.opam/opam-init/init.zsh ]] || source /home/cipher/.opam/opam-init/init.zsh  > /dev/null 2> /dev/null

      source <(fzf --zsh)

      # Show system info on new terminal
      neofetch

    '';

    oh-my-zsh = {
      enable = true;
      plugins = [ "git" ];
      theme = "robbyrussell";
    };

    history = { size = 100000; };
  };

  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
  };

  programs.direnv = {
    enable = true;
    nix-direnv = { enable = true; };
  };

  programs.starship = {
    enable = true;
    # Configuration written to ~/.config/starship.toml
    settings = {
      directory.fish_style_pwd_dir_length =
        1; # turn on fish directory truncation
      directory.truncation_length = 2; # number of directories not to truncate
      add_newline = true;

      character = {
        success_symbol = "[➜](bold green)";
        error_symbol = "[➜](bold red)";
      };
    };
  };
}
