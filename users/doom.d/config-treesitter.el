;;; config-treesitter.el --- Tree-sitter configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; TREE-SITTER CONFIGURATION
;; ============================================================================
;; Tree-sitter grammars, modes, and treesitter-context

;; ----------------------------------------------------------------------------
;; Treesitter-context (context display)
;; ----------------------------------------------------------------------------

(use-package! treesitter-context
  :after tree-sitter
  ;; :hook ((prog-mode . treesitter-context-mode)
  ;;        (prog-mode . treesitter-context-focus-mode))
  :config
  ;; optional: customize defaults
  (setq
   treesitter-context-idle-time 0.5
   treesitter-context-show-line-number t
   treesitter-context-frame-indent-offset 2))

;; Enable treesitter-context for specific modes
(add-hook 'go-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'go-ts-mode-hook #'treesitter-context-focus-mode)

(add-hook 'typescript-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'typescript-ts-mode-hook #'treesitter-context-focus-mode)

(add-hook 'tsx-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'tsx-ts-mode-hook #'treesitter-context-focus-mode)

;; ----------------------------------------------------------------------------
;; Treesit-auto (automatic grammar installation)
;; ----------------------------------------------------------------------------

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Add all *-ts-modes to `auto-mode-alist'
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

;; ----------------------------------------------------------------------------
;; Treesit Configuration (grammar sources and mode remapping)
;; ----------------------------------------------------------------------------

(use-package treesit
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (when (fboundp 'treesit-available-p)
      (condition-case err
          (dolist (grammar
                   '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                     (bash "https://github.com/tree-sitter/tree-sitter-bash")
                     (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                     (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                     (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                     (make "https://github.com/alemuller/tree-sitter-make")
                     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                     (cmake "https://github.com/uyha/tree-sitter-cmake")
                     (c "https://github.com/tree-sitter/tree-sitter-c")
                     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                     (toml "https://github.com/tree-sitter/tree-sitter-toml")
                     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                     (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                     (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
            (condition-case grammar-err
                (progn
                  (add-to-list 'treesit-language-source-alist grammar)
                  ;; Only install `grammar' if we don't already have it
                  ;; installed. However, if you want to *update* a grammar then
                  ;; this obviously prevents that from happening.
                  (unless (treesit-language-available-p (car grammar))
                    (treesit-install-language-grammar (car grammar))))
              (error
               (message "Failed to install tree-sitter grammar %s: %s" (car grammar) grammar-err))))
        (error
         (message "Error setting up tree-sitter grammars: %s" err)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (typescript-tsx-mode . tsx-ts-mode)
             (js-mode . js-ts-mode)
             (js2-mode . js-ts-mode)
             (javascript-mode . js-ts-mode)
             (html-mode . web-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (jsonc-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

;; ----------------------------------------------------------------------------
;; Tree-sitter Extra Load Path
;; ----------------------------------------------------------------------------

(add-to-list 'treesit-extra-load-path "~/.tree-sitter/bin")

(provide 'config-treesitter)
;;; config-treesitter.el ends here
