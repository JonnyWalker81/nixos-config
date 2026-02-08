;;; config-lsp.el --- LSP base configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; LSP BASE CONFIGURATION
;; ============================================================================
;; Core LSP-mode settings, performance tuning, and organize imports

;; ----------------------------------------------------------------------------
;; LSP Performance Settings
;; ----------------------------------------------------------------------------

(after! lsp-mode
  ;; Prevent LSP from blocking Emacs
  (setq lsp-response-timeout 5)           ; 5 second timeout instead of default 10
  (setq lsp-enable-on-type-formatting nil) ; Disable formatting on typing
  (setq lsp-before-save-edits nil)        ; Don't wait for edits before save

  ;; IMPORTANT: Do NOT add lsp-organize-imports to before-save-hook!
  ;; The TypeScript language server's organize imports also reformats the code,
  ;; which overrides our Prettier formatting with 4-space indentation.
  ;; If you need to organize imports, do it manually with M-x lsp-organize-imports

  ;; File watcher settings
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 1000))  ; Reduced from 3000 to prevent system overload

;; ----------------------------------------------------------------------------
;; Apheleia Configuration
;; ----------------------------------------------------------------------------
;; NOTE: Do NOT enable apheleia-global-mode here!
;;
;; Doom's (format +onsave) module in init.el already provides format-on-save
;; using apheleia under the hood. Having apheleia-global-mode ALSO enabled
;; creates TWO competing format-on-save mechanisms that use different formatters,
;; causing inconsistent indentation (4 spaces instead of 2).
;;
;; By disabling apheleia-global-mode, format-on-save uses the same code path
;; as +format/buffer, which correctly uses our custom formatters with --tab-width.
;;
;; The formatter definitions in config-formatting.el are still used by Doom's
;; format module via apheleia-mode-alist lookup.

(provide 'config-lsp)
;;; config-lsp.el ends here
