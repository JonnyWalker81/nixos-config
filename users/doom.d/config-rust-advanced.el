;;; config-rust-advanced.el --- Advanced Rust development features -*- lexical-binding: t; -*-

;; ============================================================================
;; ADVANCED RUST DEVELOPMENT FEATURES
;; ============================================================================

;; Rust playground integration
(defun rust-playground ()
  "Create a new Rust playground."
  (interactive)
  (let* ((playground-dir (expand-file-name "~/rust-playground"))
         (project-name (format "playground-%s" (format-time-string "%Y%m%d-%H%M%S")))
         (project-path (expand-file-name project-name playground-dir)))
    (unless (file-exists-p playground-dir)
      (make-directory playground-dir t))
    (let ((default-directory playground-dir))
      (shell-command (format "cargo new %s" project-name))
      (find-file (expand-file-name "src/main.rs" project-path))
      (delete-region (point-min) (point-max))
      (insert "fn main() {\n    println!(\"Hello, playground!\");\n}\n")
      (save-buffer))))

;; Quick benchmarking
(defun rust-quick-bench ()
  "Run quick benchmark for current project."
  (interactive)
  (rustic-cargo-run-command "bench"))

;; Documentation browser
(defun rust-browse-docs ()
  "Browse Rust documentation."
  (interactive)
  (browse-url "https://doc.rust-lang.org/std/"))

;; Crates.io integration
(defun rust-search-crates-io (query)
  "Search for QUERY on crates.io."
  (interactive "sSearch crates.io: ")
  (browse-url (format "https://crates.io/search?q=%s" (url-hexify-string query))))

;; Advanced refactoring helpers
(defun rust-convert-to-use-statement ()
  "Convert fully qualified path to use statement."
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (let* ((symbol (buffer-substring-no-properties (car bounds) (cdr bounds)))
             (parts (split-string symbol "::"))
             (use-stmt (format "use %s;" symbol)))
        (save-excursion
          (goto-char (point-min))
          (if (re-search-forward "^use " nil t)
              (progn
                (end-of-line)
                (newline)
                (insert use-stmt))
            (insert use-stmt "\n\n")))
        (delete-region (car bounds) (cdr bounds))
        (insert (car (last parts)))))))

;; Cargo.toml manipulation
(defun rust-add-workspace-member (member-path)
  "Add MEMBER-PATH to workspace in Cargo.toml."
  (interactive "sMember path: ")
  (let ((cargo-file (expand-file-name "Cargo.toml" (projectile-project-root))))
    (when (file-exists-p cargo-file)
      (with-current-buffer (find-file-noselect cargo-file)
        (goto-char (point-min))
        (if (re-search-forward "members = \\[" nil t)
            (progn
              (forward-char -1)
              (insert (format "\n    \"%s\"," member-path))
              (save-buffer))
          (message "No workspace section found in Cargo.toml"))))))

;; Performance profiling integration
(defun rust-profile-with-perf ()
  "Profile current Rust binary with perf."
  (interactive)
  (let ((binary (read-string "Binary name: " (projectile-project-name))))
    (compile (format "cargo build --release && perf record --call-graph=dwarf target/release/%s && perf report" binary))))

;; Macro expansion improvements
(defun rust-expand-macro-recursively ()
  "Expand Rust macro recursively."
  (interactive)
  (if (fboundp 'lsp-rust-analyzer-expand-macro)
      (lsp-rust-analyzer-expand-macro t)
    (message "LSP not available")))

;; Code generation helpers
(defun rust-generate-builder-pattern ()
  "Generate builder pattern for struct at point."
  (interactive)
  (let ((struct-name (thing-at-point 'symbol)))
    (when struct-name
      (end-of-defun)
      (insert (format "\n\nimpl %s {\n" struct-name))
      (insert (format "    pub fn builder() -> %sBuilder {\n" struct-name))
      (insert (format "        %sBuilder::default()\n" struct-name))
      (insert "    }\n}\n\n")
      (insert (format "#[derive(Default)]\npub struct %sBuilder {\n" struct-name))
      (insert "    // Add fields here\n}\n\n")
      (insert (format "impl %sBuilder {\n" struct-name))
      (insert (format "    pub fn build(self) -> %s {\n" struct-name))
      (insert (format "        %s {\n" struct-name))
      (insert "            // Initialize fields\n")
      (insert "        }\n    }\n}\n"))))

;; Integration with external tools
(defun rust-check-with-miri ()
  "Run Miri on current project."
  (interactive)
  (rustic-cargo-run-command "+nightly miri test"))

(defun rust-check-security ()
  "Run cargo-audit on current project."
  (interactive)
  (rustic-cargo-run-command "audit"))

;; Custom faces for Rust
(defface rust-unsafe-face
  '((t (:foreground "red" :weight bold)))
  "Face for unsafe blocks in Rust."
  :group 'rust-mode)

(defface rust-macro-face
  '((t (:foreground "purple" :slant italic)))
  "Face for macros in Rust."
  :group 'rust-mode)

;; Enhanced syntax highlighting
(font-lock-add-keywords
 'rustic-mode
 '(("\\<unsafe\\>" . 'rust-unsafe-face)
   ("\\w+!" . 'rust-macro-face)))

;; Smart template insertion
(defun rust-insert-common-derives ()
  "Insert common derive macros."
  (interactive)
  (insert "#[derive(Debug, Clone, PartialEq, Eq, Hash)]"))

(defun rust-insert-error-type ()
  "Insert a custom error type template."
  (interactive)
  (insert "#[derive(Debug, thiserror::Error)]\n")
  (insert "pub enum Error {\n")
  (insert "    #[error(\"TODO: error message\")]\n")
  (insert "    Todo,\n")
  (insert "}\n\n")
  (insert "pub type Result<T> = std::result::Result<T, Error>;\n"))

;; Workspace navigation
(defun rust-find-workspace-root ()
  "Find the root Cargo.toml with [workspace] section."
  (let ((root (locate-dominating-file default-directory
                                      (lambda (dir)
                                        (let ((cargo-file (expand-file-name "Cargo.toml" dir)))
                                          (and (file-exists-p cargo-file)
                                               (with-temp-buffer
                                                 (insert-file-contents cargo-file)
                                                 (re-search-forward "\\[workspace\\]" nil t))))))))
    (when root
      (expand-file-name root))))

;; Enhanced keybindings for advanced features
(map! :after rustic
      :map rustic-mode-map
      :localleader
      (:prefix ("g" . "generate")
       :desc "Builder pattern" "b" #'rust-generate-builder-pattern
       :desc "Error type" "e" #'rust-insert-error-type
       :desc "Common derives" "d" #'rust-insert-common-derives)
      
      (:prefix ("p" . "profile")
       :desc "Profile with perf" "p" #'rust-profile-with-perf
       :desc "Check with Miri" "m" #'rust-check-with-miri
       :desc "Security audit" "s" #'rust-check-security)
      
      (:prefix ("w" . "workspace")
       :desc "Add member" "a" #'rust-add-workspace-member
       :desc "Find root" "r" #'rust-find-workspace-root
       :desc "Switch member" "s" #'rust-switch-to-workspace-member)
      
      (:prefix ("h" . "help")
       :desc "Browse docs" "d" #'rust-browse-docs
       :desc "Search crates.io" "c" #'rust-search-crates-io
       :desc "Playground" "p" #'rust-playground))

;; Auto-insert templates
(define-auto-insert
  '("\\.rs\\'" . "Rust source file")
  '(nil
    "//! " (file-name-nondirectory (buffer-file-name)) "\n"
    "//!\n"
    "//! " _ "\n"
    "\n"))

;; Integration with rust-analyzer's assists
(defun rust-apply-all-assists ()
  "Apply all available assists from rust-analyzer."
  (interactive)
  (lsp-execute-code-action-by-kind "refactor"))

;; Load config-rust-advanced when config-rust is loaded
(with-eval-after-load 'config-rust
  (message "Loading advanced Rust features..."))

(provide 'config-rust-advanced)
;;; config-rust-advanced.el ends here