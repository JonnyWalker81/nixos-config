{
  config,
  lib,
  pkgs,
  inputs,
  ...
}:

{
  home.packages = [
    # Programming fonts
    pkgs.cascadia-code
    pkgs.jetbrains-mono
    pkgs.victor-mono
    pkgs.input-fonts
    pkgs.iosevka
    pkgs.ripgrep
    pkgs.fd
    pkgs.monaspace
    pkgs.zoxide
    pkgs.bat
    pkgs.delta
    pkgs.jq
    pkgs.eza
    pkgs.fzf
    pkgs.k9s
    pkgs.procs
    pkgs.graphviz
    pkgs.fira-code
    pkgs.fira-code-symbols
    pkgs.llvm
    pkgs.bind
    pkgs.pandoc
    pkgs.terraform-ls
    pkgs.tree-sitter
    pkgs.file
    pkgs.nil
    pkgs.nixpkgs-fmt
    pkgs.nixfmt-rfc-style
    pkgs.shfmt
    (pkgs.python3.withPackages (
      p:
      with p;
      [
        epc
        orjson
        sexpdata
        six
        setuptools
        paramiko
      ]
      ++ lib.optionals (!pkgs.stdenv.isDarwin) [ rapidfuzz ]
    ))

    pkgs.difftastic
    pkgs.nixd
    pkgs.luaformatter
    pkgs.lua-language-server
    pkgs.stylua
    pkgs.sql-formatter
    pkgs.typescript-language-server
    pkgs.sqls
    pkgs.yazi

    pkgs.claude-code
    pkgs.opencode
    pkgs.pi-coding-agent
  ]
  ++ lib.optionals (!pkgs.stdenv.isDarwin) [
    # Packages with wayland dependencies (Linux only)
    pkgs.qemu
    pkgs.pgmanage
    pkgs.pgadmin4
    pkgs.nixvim # Neovim with cipher-nixvim config (has wayland clipboard deps)
    pkgs.libreoffice
    pkgs.chromium
    pkgs.rustup
    pkgs.clang
    pkgs.just
    pkgs.docker-compose
    pkgs.postgresql_14
    pkgs.feh
    pkgs.xplr
    pkgs.kitty
    pkgs.font-awesome_5
    pkgs.powerline-fonts
    pkgs.powerline-symbols
    pkgs.openssl
    pkgs.lsof
    pkgs.gnupg
    pkgs.pinentry
    pkgs.hunspell
    pkgs.hunspellDicts.en-us
    pkgs.croc
    pkgs.bottom
    pkgs.kubernetes-helm
    pkgs.unzip
    pkgs.lapce
    pkgs.terraform
    pkgs.enchant
    pkgs.gh

    pkgs.sqlite
    pkgs.acpi
    pkgs.golangci-lint
    pkgs.kubernetes
    pkgs.pcmanfm
    pkgs.nemo
    pkgs.rofi
    pkgs.vscode
    pkgs.haskellPackages.libmpd
    pkgs.haskellPackages.xmobar
    pkgs.nyxt
    pkgs.ssm-session-manager-plugin
    pkgs.bun
    pkgs.wezterm
    pkgs.ghostty # Now using nixpkgs version which supports macOS
    pkgs.zellij
    pkgs.gtk3
    pkgs.teller
    pkgs.warp-terminal
    pkgs.neofetch

    # Wayland/Hyprland tools (Linux only)
    pkgs.waybar
    pkgs.wl-clipboard # Essential for Wayland clipboard
    pkgs.maim # Screenshot tool (X11, Linux-only)
    pkgs.xclip # Essential for X11 clipboard compatibility
    pkgs.swww
    pkgs.wofi
    pkgs.cliphist # Clipboard history manager
    pkgs.grim
    pkgs.slurp
    pkgs.swaylock
    pkgs.swayidle
    pkgs.swaynotificationcenter
  ];
}
