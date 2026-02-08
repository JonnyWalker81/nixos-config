;;; config-rust.el --- Enhanced Rust development configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; RUST DEVELOPMENT CONFIGURATION
;; ============================================================================
;; Enhanced Rust setup with rust-analyzer, debugging, and productivity features

(message "Loading Rust configuration...")

;; Core Rust configuration
(after! rustic
  ;; Use rust-analyzer as the primary LSP server
  (setq rustic-lsp-server 'rust-analyzer
        rustic-lsp-client 'lsp-mode
        rustic-analyzer-command '("rust-analyzer"))
  
  ;; Format configuration
  (setq rustic-format-on-save t
        rustic-format-trigger 'on-save
        rustic-format-display-method 'pop-to-buffer-without-switch)
  
  ;; Compilation settings
  (setq rustic-compile-backtrace 1)
  
  ;; Clippy configuration
  (setq rustic-flycheck-clippy-params "--message-format=json")
  
  ;; Test configuration
  (setq rustic-test-arguments '("--nocapture"))
  
  ;; Indent configuration
  (setq rustic-indent-method-chain t
        rustic-indent-where-clause t))

;; LSP configuration for Rust
(after! lsp-mode
  ;; Rust-analyzer specific settings
  (setq lsp-rust-analyzer-cargo-watch-enable t
        lsp-rust-analyzer-cargo-watch-command "check"  ; Changed from "clippy" - clippy is slower and runs continuously
        lsp-rust-analyzer-proc-macro-enable t
        lsp-rust-analyzer-cargo-load-out-dirs-from-check t
        lsp-rust-analyzer-display-chaining-hints t
        lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial"
        lsp-rust-analyzer-display-parameter-hints t
        lsp-rust-analyzer-display-reborrow-hints t
        lsp-rust-analyzer-max-inlay-hint-length 25
        lsp-rust-analyzer-display-closure-return-type-hints t
        lsp-rust-analyzer-server-display-inlay-hints t)
  
  ;; Import settings
  (setq lsp-rust-analyzer-import-granularity "module"
        lsp-rust-analyzer-import-group t
        lsp-rust-analyzer-import-prefix "self")
  
  ;; Completion settings
  (setq lsp-rust-analyzer-completion-add-call-parenthesis t
        lsp-rust-analyzer-completion-add-call-argument-snippets t
        lsp-rust-analyzer-completion-postfix-enable t)
  
  ;; Diagnostics
  (setq lsp-rust-analyzer-diagnostics-enable t
        lsp-rust-analyzer-diagnostics-enable-experimental t)
  
  ;; Lens settings (code actions)
  (setq lsp-rust-analyzer-lens-enable t
        lsp-rust-analyzer-lens-debug t
        lsp-rust-analyzer-lens-implementations t
        lsp-rust-analyzer-lens-run t
        lsp-rust-analyzer-lens-method-references t
        lsp-rust-analyzer-lens-references t))

;; DAP (Debug Adapter Protocol) configuration for Rust
(after! dap-mode
  (require 'dap-cpptools)
  (dap-register-debug-template
   "Rust::GDB Run Configuration"
   (list :type "gdb"
         :request "launch"
         :name "GDB::Run"
         :gdbpath "rust-gdb"
         :target nil
         :cwd nil)))

;; Cargo integration
(use-package! cargo
  :after rustic
  :config
  (setq cargo-process--command-flags "--color=always")
  (add-hook 'rustic-mode-hook 'cargo-minor-mode))

;; Enhanced completion for Rust
(after! corfu
  (add-hook 'rustic-mode-hook
            (lambda ()
              (setq-local corfu-auto-delay 0.0
                          corfu-auto-prefix 1))))

;; Rust-specific keybindings
(map! :after rustic
      :map rustic-mode-map
      :localleader
      ;; Build commands
      (:prefix ("b" . "build")
       :desc "Build project" "b" #'rustic-cargo-build
       :desc "Build release" "r" #'rustic-cargo-build-release
       :desc "Build docs" "d" #'rustic-cargo-build-doc
       :desc "Open docs" "D" #'rustic-cargo-doc)
      
      ;; Test commands
      (:prefix ("t" . "test")
       :desc "Run all tests" "t" #'rustic-cargo-test
       :desc "Run current test" "c" #'rustic-cargo-current-test
       :desc "Run tests in file" "f" #'rustic-cargo-test-run
       :desc "Rerun last test" "r" #'rustic-cargo-test-rerun)
      
      ;; Run commands
      (:prefix ("r" . "run")
       :desc "Run project" "r" #'rustic-cargo-run
       :desc "Run release" "R" (cmd! (rustic-cargo-run-command "run --release"))
       :desc "Run example" "e" #'rustic-cargo-run-example
       :desc "Run binary" "b" #'rustic-cargo-run-bin)
      
      ;; Cargo commands
      (:prefix ("c" . "cargo")
       :desc "Add dependency" "a" #'rustic-cargo-add
       :desc "Remove dependency" "r" #'rustic-cargo-rm
       :desc "Upgrade dependencies" "u" #'rustic-cargo-upgrade
       :desc "Update dependencies" "U" #'rustic-cargo-update
       :desc "Cargo check" "c" #'rustic-cargo-check
       :desc "Cargo clippy" "l" #'rustic-cargo-clippy
       :desc "Cargo clippy fix" "f" #'rustic-cargo-clippy-fix
       :desc "Cargo fmt" "F" #'rustic-cargo-fmt
       :desc "Cargo clean" "x" #'rustic-cargo-clean
       :desc "Cargo outdated" "o" #'rustic-cargo-outdated)
      
      ;; Code actions and refactoring
      (:prefix ("a" . "actions")
       :desc "Expand macro" "e" #'lsp-rust-analyzer-expand-macro
       :desc "Join lines" "j" #'lsp-rust-analyzer-join-lines
       :desc "Structural search replace" "s" #'lsp-rust-analyzer-ssr
       :desc "Extract function" "f" #'lsp-rust-extract-function
       :desc "Extract variable" "v" #'lsp-rust-extract-variable
       :desc "Move item up" "k" #'lsp-rust-analyzer-move-item-up
       :desc "Move item down" "j" #'lsp-rust-analyzer-move-item-down)
      
      ;; Debug commands
      (:prefix ("d" . "debug")
       :desc "Debug run" "d" #'dap-debug
       :desc "Debug hydra" "h" #'dap-hydra
       :desc "Debug breakpoint toggle" "b" #'dap-breakpoint-toggle
       :desc "Debug continue" "c" #'dap-continue
       :desc "Debug next" "n" #'dap-next
       :desc "Debug step in" "i" #'dap-step-in
       :desc "Debug step out" "o" #'dap-step-out
       :desc "Debug restart" "r" #'dap-debug-restart
       :desc "Debug quit" "q" #'dap-disconnect))

;; Helper functions for Rust development
(defun rustic-cargo-add ()
  "Add a cargo dependency interactively."
  (interactive)
  (let* ((dependency (read-string "Dependency name: "))
         (version (read-string "Version (leave empty for latest): "))
         (features (read-string "Features (comma-separated, optional): "))
         (cmd (concat "add " dependency)))
    (when (not (string-empty-p version))
      (setq cmd (concat cmd "@" version)))
    (when (not (string-empty-p features))
      (setq cmd (concat cmd " --features " features)))
    (rustic-cargo-run-command cmd)))

(defun rustic-cargo-build-release ()
  "Build project in release mode."
  (interactive)
  (rustic-cargo-run-command "build --release"))

(defun rustic-cargo-run-command (command)
  "Run a cargo command with COMMAND string."
  (let ((rustic-cargo-bin-remote command))
    (rustic-cargo-run)))

;; Rust snippets
(after! yasnippet
  (add-to-list 'yas-snippet-dirs
               (expand-file-name "snippets/rust-mode" doom-private-dir) t))

;; Tree-sitter support for Rust
(after! tree-sitter
  (add-to-list 'tree-sitter-major-mode-language-alist '(rustic-mode . rust)))

;; Additional file associations
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rustic-mode))
(add-to-list 'auto-mode-alist '("\\Cargo.toml\\'" . toml-mode))
(add-to-list 'auto-mode-alist '("\\Cargo.lock\\'" . toml-mode))

;; Performance optimizations for large Rust projects
(defun rust-optimize-lsp-performance ()
  "Optimize LSP performance for large Rust projects."
  (setq-local lsp-enable-file-watchers nil
              lsp-idle-delay 0.5
              lsp-log-io nil))

(add-hook 'rustic-mode-hook #'rust-optimize-lsp-performance)

;; Smart imports
(defun rust-import-module-at-point ()
  "Import the module for the symbol at point."
  (interactive)
  (lsp-execute-code-action-by-kind "quickfix"))

;; Workspace support
(defun rust-switch-to-workspace-member ()
  "Switch to a workspace member in a Cargo workspace."
  (interactive)
  (let* ((members (rust-get-workspace-members))
         (member (completing-read "Switch to member: " members)))
    (find-file (expand-file-name "Cargo.toml" member))))

(defun rust-get-workspace-members ()
  "Get list of workspace members."
  (let ((root (projectile-project-root)))
    (when root
      (let ((cargo-file (expand-file-name "Cargo.toml" root)))
        (when (file-exists-p cargo-file)
          ;; Parse workspace members from Cargo.toml
          ;; This is a simplified version - you might want to use a proper TOML parser
          (with-temp-buffer
            (insert-file-contents cargo-file)
            (when (re-search-forward "\\[workspace\\]" nil t)
              (when (re-search-forward "members = \\[\\([^]]+\\)\\]" nil t)
                (let ((members-str (match-string 1)))
                  (mapcar (lambda (s) (string-trim s "\"" "\""))
                          (split-string members-str ","))))))))))

;; Load advanced features
(load! "config-rust-advanced")

(message "Rust configuration loaded successfully!")

(provide 'config-rust)
;;; config-rust.el ends here