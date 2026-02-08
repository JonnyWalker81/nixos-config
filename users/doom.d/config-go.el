;;; config-go.el --- Go/Golang configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; GO/GOLANG CONFIGURATION WITH GOPLS
;; ============================================================================
;; Configuration matching VSCode Go extension settings

(use-package! go-mode
  :config
  ;; Use goimports as the format tool (matches "go.formatTool": "goimports")
  (setq gofmt-command "goimports")

  ;; Configure golangci-lint as the linter (matches "go.lintTool": "golangci-lint")
  (setq flycheck-go-golint-executable "golangci-lint")

  ;; IMPORTANT: Go uses tabs, not spaces for indentation
  ;; Set proper tab width for Go (standard is 4 or 8)
  (setq go-tab-width 4)

  ;; Hook for Go mode setup
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              ;; Ensure Go uses tabs for indentation
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save (matches "[go]": {"editor.formatOnSave": true})
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports handled by jr/lsp-organize-imports-safe
              ))

  ;; Also configure go-ts-mode (tree-sitter mode)
  (add-hook 'go-ts-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              ;; Ensure Go uses tabs for indentation
              (setq indent-tabs-mode nil)
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports handled by jr/lsp-organize-imports-safe
              )))

;; ----------------------------------------------------------------------------
;; LSP Go Configuration (gopls)
;; ----------------------------------------------------------------------------

(after! lsp-go
  ;; Tell gopls to use goimports
  (setq lsp-go-format-tool "goimports")

  ;; Configure gopls settings to match VSCode config
  (setq lsp-go-gopls-server-args
        '("-remote=auto"))

  ;; Set gopls build directory filters (matches gopls "build.directoryFilters")
  (setq lsp-go-directory-filters
        ["-**/node_modules"
         "-**/testdata"])

  ;; Set local module for import organization (matches gopls "formatting.local")
  (setq lsp-go-imports-local-prefix "github.com/JoinCAD/komodo")

  ;; Enable all analyses
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (shadow . t)
          (unusedparams . t)
          (unusedwrite . t)
          (useany . t)
          (unusedvariable . t))))

;; ----------------------------------------------------------------------------
;; Flycheck/Golangci-lint Integration
;; ----------------------------------------------------------------------------

(after! flycheck
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Set golangci-lint config file path (matches "go.lintFlags")
              (setq-local flycheck-golangci-lint-config "./.golangci-github.toml")
              ;; Enable flycheck mode for linting
              (flycheck-mode 1))))

;; ----------------------------------------------------------------------------
;; Formatter Setup
;; ----------------------------------------------------------------------------

;; Set formatter for tree-sitter Go mode
(set-formatter! 'gofmt '("goimports") :modes '(go-mode go-ts-mode))

;; ----------------------------------------------------------------------------
;; Additional gopls Configuration via LSP
;; ----------------------------------------------------------------------------

(after! lsp-mode
  ;; Add Go-specific LSP settings
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]testdata\\'")

  ;; Configure gopls through LSP initialization options
  (setq lsp-gopls-server-args '("-remote=auto"))
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-gopls-use-placeholders t)

  ;; Custom gopls settings matching VSCode configuration
  (lsp-register-custom-settings
   '(("gopls.formatting.local" "github.com/JoinCAD/komodo")
     ("gopls.build.directoryFilters" ["-**/node_modules" "-**/testdata"])
     ("gopls.analyses.fieldalignment" t)
     ("gopls.analyses.unusedparams" t)
     ("gopls.analyses.unusedwrite" t)
     ("gopls.analyses.useany" t))))

;; ----------------------------------------------------------------------------
;; Go Tag Configuration
;; ----------------------------------------------------------------------------

(setq go-tag-args (list "-transform" "camelcase"))

(provide 'config-go)
;;; config-go.el ends here
