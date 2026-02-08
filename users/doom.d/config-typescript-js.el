;;; config-typescript-js.el --- TypeScript/JavaScript configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; TYPESCRIPT/JAVASCRIPT CONFIGURATION
;; ============================================================================
;; Consolidated TypeScript and JavaScript configuration to avoid conflicts

;; ----------------------------------------------------------------------------
;; Global LSP Formatting Settings
;; ----------------------------------------------------------------------------

;; Disable LSP formatting to avoid conflicts with Prettier
(setq lsp-format-buffer-on-save nil
      lsp-javascript-format-enable nil
      lsp-typescript-format-enable nil
      +format-with-lsp nil)

;; ----------------------------------------------------------------------------
;; Unified Setup Hook
;; ----------------------------------------------------------------------------

(defun typescript-js-setup-hook ()
  "Unified setup function for TypeScript and JavaScript modes."
  (condition-case err
      (progn
        ;; Disable LSP formatting - we use Prettier via apheleia
        ;; Don't set +format-with here - let apheleia-mode-alist select the right formatter
        ;; (prettier-typescript, prettier-tsx, prettier-javascript) which include --tab-width
        (setq-local +format-with-lsp nil)
        ;; Remove LSP formatting hooks to avoid conflicts
        (remove-hook 'before-save-hook #'lsp-format-buffer t)
        (remove-hook 'before-save-hook #'lsp-organize-imports t))
    (error
     (message "Error in typescript-js-setup-hook: %s" err))))

;; ----------------------------------------------------------------------------
;; TypeScript Mode Configuration
;; ----------------------------------------------------------------------------

(after! typescript-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook
            (lambda ()
              (setq-local +format-with 'prettier-typescript)
              (typescript-js-setup-hook))))

(after! typescript-tsx-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-tsx-mode-hook
            (lambda ()
              (setq-local +format-with 'prettier-tsx)
              (typescript-js-setup-hook))))

;; ----------------------------------------------------------------------------
;; Tree-sitter Mode Configuration
;; ----------------------------------------------------------------------------

;; Tree-sitter mode indent offsets (these modes use different variables than legacy modes)
;; Legacy modes use typescript-indent-level, but tree-sitter modes have their own variables
(setq-default typescript-ts-mode-indent-offset 2)
(setq-default js-indent-level 2)

(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook
            (lambda ()
              (setq-local typescript-ts-mode-indent-offset 2)
              ;; Explicitly set formatter to our custom prettier-typescript
              ;; This bypasses apheleia-mode-alist lookup which wasn't working
              (setq-local +format-with 'prettier-typescript)
              (typescript-js-setup-hook))))

(with-eval-after-load 'tsx-ts-mode
  (add-hook 'tsx-ts-mode-hook
            (lambda ()
              (setq-local typescript-ts-mode-indent-offset 2)
              ;; Explicitly set formatter to our custom prettier-tsx
              (setq-local +format-with 'prettier-tsx)
              (typescript-js-setup-hook))))

(with-eval-after-load 'js-ts-mode
  (add-hook 'js-ts-mode-hook
            (lambda ()
              (setq-local js-indent-level 2)
              ;; Explicitly set formatter to our custom prettier-javascript
              (setq-local +format-with 'prettier-javascript)
              (typescript-js-setup-hook))))

;; ----------------------------------------------------------------------------
;; TypeScript Language Server Configuration
;; ----------------------------------------------------------------------------

(after! lsp-mode
  ;; Tell tsserver to prefer non-relative paths and auto-update imports
  (setq lsp-clients-typescript-init-opts
        '(:typescript (:preferences (:importModuleSpecifier "non-relative"))
          :javascript (:preferences (:importModuleSpecifier "non-relative"))
          :preferences (:updateImportsOnFileMove "always")))

  ;; Add formatting indent level for TypeScript/TSX modes
  (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level))
  (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-ts-mode-indent-offset))
  (add-to-list 'lsp--formatting-indent-alist '(typescript-ts-mode . typescript-ts-mode-indent-offset))

  ;; Enable LSP for tree-sitter modes
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'js-ts-mode-hook #'lsp-deferred))

;; ----------------------------------------------------------------------------
;; EditorConfig Integration
;; ----------------------------------------------------------------------------

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level)))

;; ----------------------------------------------------------------------------
;; HTML/Web Configuration
;; ----------------------------------------------------------------------------

;; Force HTML files to use web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

;; HTML setup function with error handling
(defun html-web-setup-hook ()
  "Setup function for HTML and web modes."
  (condition-case err
      (progn
        ;; Don't set +format-with - let apheleia-mode-alist select prettier-html
        (setq-local +format-with-lsp nil)
        ;; Enable Emmet for HTML if available
        (when (featurep 'emmet-mode)
          (emmet-mode 1)))
    (error
     (message "Error in html-web-setup-hook: %s" err))))

;; HTML mode configuration - deferred for performance
(with-eval-after-load 'html-mode
  (add-hook 'html-mode-hook #'lsp-deferred)
  (add-hook 'html-mode-hook #'html-web-setup-hook))

(with-eval-after-load 'html-ts-mode
  (add-hook 'html-ts-mode-hook #'lsp-deferred)
  (add-hook 'html-ts-mode-hook #'html-web-setup-hook))

;; Web mode configuration - deferred for performance
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'lsp-deferred)
  (add-hook 'web-mode-hook #'html-web-setup-hook))

;; HTML Language Server configuration
(after! lsp-mode
  ;; Use the VSCode HTML Language Server instead of the generic one
  (setq lsp-html-server-command '("vscode-html-language-server" "--stdio"))
  ;; Customize formatting settings:
  (setq lsp-html-format-enable t
        lsp-html-format-wrap-line-length 0
        lsp-html-format-unformatted nil)

  (add-hook 'html-ts-mode-hook #'lsp-deferred))

;; ----------------------------------------------------------------------------
;; Debug Helper
;; ----------------------------------------------------------------------------

(defun debug-mode-setup ()
  "Debug function to check current mode setup and configurations."
  (interactive)
  (let ((mode-info (list
                    (cons "Major Mode" major-mode)
                    (cons "Format With" (bound-and-true-p +format-with))
                    (cons "Format With LSP" (bound-and-true-p +format-with-lsp))
                    (cons "LSP Active" (bound-and-true-p lsp-mode))
                    (cons "Tree-sitter Available" (and (fboundp 'treesit-available-p) (treesit-available-p)))
                    (cons "Buffer File" (buffer-file-name)))))
    (message "Mode Debug Info: %s" mode-info)
    (with-current-buffer (get-buffer-create "*Mode Debug*")
      (erase-buffer)
      (insert "Current Mode Debug Information\n")
      (insert "==================================\n\n")
      (dolist (info mode-info)
        (insert (format "%-20s: %s\n" (car info) (cdr info))))
      (display-buffer (current-buffer)))))

(provide 'config-typescript-js)
;;; config-typescript-js.el ends here
