;; -*- no-byte-compile: t; -*-
;;; feature/jr/packages.el

(package! emacs-snippets
  :recipe (:host github
                    :repo "hlissner/emacs-snippets"
                    :files ("*")))

(package! deadgrep)

(package! cargo)

(package! format-all)

(package! gradle-mode)

(package! groovy-mode)

(package! cmake-mode)

(package! kotlin-mode)

;; (package! org-pdfview)

;; (package! evil-org :recipe (:fetcher github :repo "Somelauw/evil-org-mode"))

(package! fzf)

(package! kubel)

;; (package! helm-posframe)

;; (package! kaolin-themes)

(package! darkokai-theme)

(package! with-editor)

(package! org-super-agenda)

(package! excorporate)

(package! rg)

(package! ranger)

;; (package! posframe)
;; (package! helm-posframe)

(package! sourcerer-theme)
(package! clues-theme)
(package! blackboard-theme)

(package! graphql-mode)

(package! yaml-mode)

(package! flycheck-inline)

;; (package! flycheck-posframe)
(package! terraform-mode)

;; (package! rjsx-mode)
;; (package! import-mode)
;; (package! prettier-js)

(package! magit-todos)

;; (package! autopair)
;; (package! protobuf-mode)

;; (package! go-mode)

;; (use-package rust-mode
;;     :mode "\\.rs\\'"
;;     :init
;;     (setq rust-format-on-save t))

;; (use-package lsp-mode
;;     :init
;;     (add-hook 'prog-mode-hook 'lsp-mode)
;;     :config
;;     (use-package lsp-flycheck
;;         :ensure f ; comes with lsp-mode
;;         :after flycheck))
;; (package! lsp-rust)
;; (package! lsp-flycheck)
;;; packages.el ends here
