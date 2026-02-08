;;; config-performance.el --- Performance tuning -*- lexical-binding: t; -*-

;; ============================================================================
;; PERFORMANCE TUNING
;; ============================================================================
;; GC tuning, redisplay optimization, and other performance settings

(message "  perf: starting...")

;; ----------------------------------------------------------------------------
;; Garbage Collection Tuning (GCMH)
;; ----------------------------------------------------------------------------
;; The profile showed 24% CPU spent on "Automatic GC" - this is the fix.
;; GCMH (Garbage Collection Magic Hack) is built into Doom, but needs tuning.

(message "  perf: gcmh...")
(after! gcmh
  ;; Increase GC threshold during normal operation (100MB)
  ;; Default is often too low, causing frequent GC pauses
  (setq gcmh-high-cons-threshold (* 100 1024 1024))

  ;; Idle time before GC runs (seconds)
  ;; Longer delay = fewer interruptions during active editing
  (setq gcmh-idle-delay 10)

  ;; Verbose mode off to reduce overhead
  (setq gcmh-verbose nil))

;; ----------------------------------------------------------------------------
;; Read Process Output Tuning
;; ----------------------------------------------------------------------------
;; Improves LSP and async process performance

(message "  perf: read-process-output...")
(setq read-process-output-max (* 1024 1024))  ; 1MB instead of 4KB default

;; ----------------------------------------------------------------------------
;; Redisplay Optimization
;; ----------------------------------------------------------------------------
;; The profile showed 43% CPU in redisplay_internal

(message "  perf: redisplay...")
;; Reduce cursor blink frequency
(setq blink-cursor-interval 0.5)

;; Don't redisplay during input
(setq redisplay-skip-fontification-on-input t)

;; Reduce visual bell overhead
(setq visible-bell nil)

;; Faster scrolling by not redisplaying on every scroll step
(setq fast-but-imprecise-scrolling t)

;; Disable bidirectional text handling for performance
;; (safe if you don't use RTL languages)
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)

;; ----------------------------------------------------------------------------
;; Mode Line Optimization
;; ----------------------------------------------------------------------------
;; Reduce mode-line update frequency

(message "  perf: modeline...")
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
(setq doom-modeline-icon t)
(setq doom-modeline-major-mode-icon t)
(setq doom-modeline-minor-modes nil)  ; Don't show minor modes (expensive)

;; ----------------------------------------------------------------------------
;; LSP Performance
;; ----------------------------------------------------------------------------

(message "  perf: lsp...")
(after! lsp-mode
  ;; Reduce LSP logging overhead
  (setq lsp-log-io nil)

  ;; Disable features that cause redisplay
  (setq lsp-lens-enable nil)           ; Disable code lenses
  (setq lsp-modeline-code-actions-enable nil)  ; Don't show code actions in modeline
  (setq lsp-headerline-breadcrumb-enable nil)  ; Disable breadcrumbs

  ;; Debounce time for LSP requests
  (setq lsp-idle-delay 0.5))

(after! lsp-ui
  ;; Disable sideline (was in profile: lsp-ui-sideline)
  (setq lsp-ui-sideline-enable nil)

  ;; Disable doc hover unless explicitly invoked
  (setq lsp-ui-doc-enable nil)
  (setq lsp-ui-doc-show-with-cursor nil)
  (setq lsp-ui-doc-show-with-mouse nil))

;; ----------------------------------------------------------------------------
;; Which-Key Optimization
;; ----------------------------------------------------------------------------
;; which-key--hide-popup appeared in the profile

(message "  perf: which-key...")
(after! which-key
  (setq which-key-idle-delay 0.5)       ; Wait longer before showing
  (setq which-key-idle-secondary-delay 0.1))

;; ----------------------------------------------------------------------------
;; Evil Optimization
;; ----------------------------------------------------------------------------
;; evil-repeat-post-hook and evil-repeat-pre-hook were in profile

(message "  perf: evil...")
(after! evil
  ;; Reduce undo granularity overhead
  (setq evil-want-fine-undo t))

;; ----------------------------------------------------------------------------
;; Emacs Lisp Indentation Fix
;; ----------------------------------------------------------------------------
;; Doom's +emacs-lisp--calculate-lisp-indent-a advice can freeze on certain
;; elisp constructs. This disables the advice to prevent hangs.

;; Remove the problematic advice after Doom's elisp module loads
(message "  perf: elisp-mode...")
(after! elisp-mode
  (defun +my/remove-lisp-indent-advice ()
    "Remove Doom's lisp indent advice that can cause hangs."
    (when (and (fboundp '+emacs-lisp--calculate-lisp-indent-a)
               (advice-member-p #'+emacs-lisp--calculate-lisp-indent-a #'calculate-lisp-indent))
      (advice-remove #'calculate-lisp-indent #'+emacs-lisp--calculate-lisp-indent-a)))
  (+my/remove-lisp-indent-advice))

;; ----------------------------------------------------------------------------
;; Flycheck Optimization
;; ----------------------------------------------------------------------------
;; Prevent flycheck from running checkers that might not exist in PATH

(message "  perf: flycheck...")
(after! flycheck
  ;; Disable automatic syntax checking to prevent "file-missing" errors
  ;; when checker executables aren't available in current direnv
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  ;; Increase delay before checking
  (setq flycheck-idle-change-delay 2.0)

  ;; Don't show errors in echo area (can block)
  (setq flycheck-display-errors-function nil))

(message "  perf: done!")
(provide 'config-performance)
;;; config-performance.el ends here
