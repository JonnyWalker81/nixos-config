;;; config-formatting.el --- Code formatting configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; CODE FORMATTING CONFIGURATION
;; ============================================================================
;; Apheleia, Prettier, and general formatting settings

;; ----------------------------------------------------------------------------
;; Apheleia Configuration
;; ----------------------------------------------------------------------------

(use-package! apheleia
  :ensure t
  :config
  ;; YAML formatting with Prettier
  (setf (alist-get 'prettier-yaml apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=yaml" "--single-quote=false"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))

  ;; TypeScript/JavaScript formatting with 2-space indentation
  ;; Hardcode --tab-width 2 to ensure consistent formatting
  (setf (alist-get 'prettier-typescript apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=typescript" "--tab-width" "2"))
  (setf (alist-get 'prettier-tsx apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=typescript" "--tab-width" "2"))
  (setf (alist-get 'prettier-javascript apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=babel" "--tab-width" "2"))

  ;; GraphQL formatting
  (setf (alist-get 'prettier-graphql apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=graphql"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  (add-to-list 'apheleia-mode-alist '(graphql-mode . prettier-graphql))

  ;; HTML formatting with Prettier
  (setf (alist-get 'prettier-html apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=html" "--html-whitespace-sensitivity" "strict"
          "--print-width" "120"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width")))
  ;; Associate HTML modes with prettier formatter
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier-html))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier-html))

  ;; Associate tree-sitter modes with formatters
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . prettier-typescript))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . prettier-tsx))
  (add-to-list 'apheleia-mode-alist '(js-ts-mode . prettier-javascript))
  (add-to-list 'apheleia-mode-alist '(html-ts-mode . prettier-html)))

;; ----------------------------------------------------------------------------
;; Prettier Helper Functions
;; ----------------------------------------------------------------------------

(defun jr/prettify-and-save()
  "Run prettier with specific args and save."
  (interactive)
  (let ((current-mode major-mode))
    (fundamental-mode)
    (setq prettier-js-args '("--trailing-comma" "es5"))
    (prettier-js)
    (setq prettier-js-args nil)
    (save-buffer)
    (funcall current-mode)))

(defun jr/pretty-and-save()
  "Save buffer (prettier runs via apheleia)."
  (interactive)
  ;; (prettier-js)
  (save-buffer))

;; Keybinding for prettier
(map! :localleader :desc "jr/prettify" "s" #'jr/prettify-and-save)

;; ----------------------------------------------------------------------------
;; Protobuf Formatting
;; ----------------------------------------------------------------------------

(add-hook 'protobuf-mode-hook (lambda ()
                                (format-all-mode t)))

(provide 'config-formatting)
;;; config-formatting.el ends here
