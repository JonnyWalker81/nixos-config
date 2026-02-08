;;; config.el --- Doom Emacs configuration bootstrap -*- lexical-binding: t; -*-

;; ============================================================================
;; DOOM EMACS CONFIGURATION
;; ============================================================================
;; This file bootstraps the configuration by loading modular config files.
;; Each config-*.el file handles a specific concern.

(message "Loading Doom config...")

;; ----------------------------------------------------------------------------
;; Load Path Setup
;; ----------------------------------------------------------------------------

(defun load-directory (dir)
  "Load all .el files in DIR."
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Add personal doom config directory to load-path
(add-to-list 'load-path "~/.doom.d")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'load-path "~/Repositories/org-reveal")
(add-to-list 'load-path "~/Repositories/komodo")

;; Load all .el files from komodo utility library
(when (file-directory-p "~/Repositories/komodo")
  (load-directory "~/Repositories/komodo"))

;; ----------------------------------------------------------------------------
;; System-specific Path Setup
;; ----------------------------------------------------------------------------

;; Ensure exec-path-from-shell is available early
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell nil t))

(if (eq system-type 'darwin)
    (progn
      ;; Add nix paths to exec-path for macOS
      (let ((home (expand-file-name "~")))
        (add-to-list 'exec-path (concat home "/.local/state/nix/profiles/home-manager/home-path/bin"))
        (add-to-list 'exec-path (concat home "/.nix-profile/bin")))
      (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
      (add-to-list 'exec-path "/run/current-system/sw/bin")
      ;; Initialize exec-path-from-shell if available
      (when (fboundp 'exec-path-from-shell-initialize)
        (exec-path-from-shell-initialize))
      ;; Copy environment variables
      (when (fboundp 'exec-path-from-shell-copy-env)
        (exec-path-from-shell-copy-env "GOPATH")
        (exec-path-from-shell-copy-env "RUST_SRC_PATH")
        (exec-path-from-shell-copy-env "RUSTUP_HOME")
        (exec-path-from-shell-copy-env "CARGO_HOME")
        (exec-path-from-shell-copy-env "PATH"))))

(if (eq system-type 'linux)
    (progn
      (exec-path-from-shell-copy-env "GOPATH")
      (exec-path-from-shell-copy-env "RUST_SRC_PATH")
      (exec-path-from-shell-copy-env "RUSTUP_HOME")
      (exec-path-from-shell-copy-env "CARGO_HOME")))

;; Tree-sitter extra load path
(add-to-list 'treesit-extra-load-path "~/.tree-sitter/bin")

;; ----------------------------------------------------------------------------
;; User Information
;; ----------------------------------------------------------------------------

(setq user-full-name "Jonathan Rothberg"
      user-mail-address "jon@join.build")

;; Auth sources
(setq auth-sources '("~/.authinfo"))

;; ----------------------------------------------------------------------------
;; Theme and Appearance
;; ----------------------------------------------------------------------------

(setq doom-theme 'doom-tokyo-night)
(setq doom-line-numbers-style 'relative)
(setq display-line-numbers-type 'relative)
(toggle-frame-maximized)

;; Visual settings
(setq visual-fill-column-width 80)
(setq nlinum-highlight-current-line t)
(setq ns-use-thin-smoothing t)
(setq +popup-buffer-mode 'toggle)

;; ----------------------------------------------------------------------------
;; Core Editor Settings
;; ----------------------------------------------------------------------------

;; Global setting - use spaces instead of tabs
(setq-default indent-tabs-mode nil)
(setq electric-indent-inhibit nil)

;; Hook for all programming modes to use spaces by default (Go overrides)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'go-mode 'go-ts-mode)
              (setq indent-tabs-mode nil))))

;; Text mode should also use spaces
(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

;; Fill column
(set-fill-column 120)

;; Auto composition mode
(auto-composition-mode t)

;; ----------------------------------------------------------------------------
;; Required Packages
;; ----------------------------------------------------------------------------

(require 'vc-git)
(require 'nix-shell)
(require 'protobuf-mode)
(require 'prettier-js)
(require 'exec-path-from-shell)
(require 'keychain-environment)
(keychain-refresh-environment)

;; SSH environment
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; Force HTML files to use web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

;; ----------------------------------------------------------------------------
;; Magit Todos Exclusions
;; ----------------------------------------------------------------------------

(setq magit-todos-exclude-globs '("*brindle*" "*node_modules*"))

;; ----------------------------------------------------------------------------
;; Smartparens Configuration
;; ----------------------------------------------------------------------------

(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))))

;; ----------------------------------------------------------------------------
;; Editorconfig
;; ----------------------------------------------------------------------------

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level)))

;; ----------------------------------------------------------------------------
;; Go Tag Args
;; ----------------------------------------------------------------------------

(setq go-tag-args (list "-transform" "camelcase"))

;; ----------------------------------------------------------------------------
;; GitHub Review
;; ----------------------------------------------------------------------------

(setq github-review-fetch-top-level-and-review-comments t)

;; ----------------------------------------------------------------------------
;; Savehist
;; ----------------------------------------------------------------------------

(use-package savehist
  :init
  (savehist-mode))

;; ----------------------------------------------------------------------------
;; Load Modular Configuration Files
;; ----------------------------------------------------------------------------

;; Load performance tuning FIRST to set GC thresholds early
(message "Loading config-performance...")
(load! "config-performance")
(message "Loading config-font...")
(load! "config-font")
(message "Loading config-misc...")
(load! "config-misc")
(message "Loading config-bindings...")
(load! "config-bindings")
(message "Loading config-rust...")
(load! "config-rust")
(message "Loading config-rust-advanced...")
(load! "config-rust-advanced")
(message "Loading config-lsp...")
(load! "config-lsp")
(message "Loading config-formatting...")
(load! "config-formatting")
(message "Loading config-treesitter...")
(load! "config-treesitter")
(message "Loading config-completion...")
(load! "config-completion")
(message "Loading config-org...")
(load! "config-org")
(message "Loading config-copilot...")
(load! "config-copilot")
(message "Loading config-magit...")
(load! "config-magit")
(message "Loading config-go...")
(load! "config-go")
(message "Loading config-typescript-js...")
(load! "config-typescript-js")
(message "Loading config-sql...")
(load! "config-sql")
(message "Loading config-uuid...")
(load! "config-uuid")
(message "Loading config-git-utils...")
(load! "config-git-utils")

(message "Doom config loaded successfully!")
;;; config.el ends here
