;; -*- no-byte-compile: t; -*-
;;; feature/jr/packages.el

(package! emacs-snippets
  :recipe (:fetcher github
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

(package! protobuf-mode)

(package! kubel)

(package! zenburn-theme)

(package! color-theme-sanityinc-tomorrow)

(package! material)

;; (package! composite
;;   :config
;;   :defer t
;;   :init
;;   (defvar composition-ligature-table (make-char-table nil))
;;   :hook
;;   (((prog-mode conf-mode nxml-mode markdown-mode help-mode)
;;     . (lambda () (setq-local composition-function-table composition-ligature-table))))
;;   :config
;;   ;; support ligatures, some toned down to prevent hang
;;   (when (version<= "27.0" emacs-version)
;;     (let ((alist
;;            '((33 . ".\\(?:\\(==\\|[!=]\\)[!=]?\\)")
;;              (35 . ".\\(?:\\(###?\\|_(\\|[(:=?[_{]\\)[#(:=?[_{]?\\)")
;;              (36 . ".\\(?:\\(>\\)>?\\)")
;;              (37 . ".\\(?:\\(%\\)%?\\)")
;;              (38 . ".\\(?:\\(&\\)&?\\)")
;;              (42 . ".\\(?:\\(\\*\\*\\|[*>]\\)[*>]?\\)")
;;              ;; (42 . ".\\(?:\\(\\*\\*\\|[*/>]\\).?\\)")
;;              (43 . ".\\(?:\\([>]\\)>?\\)")
;;              ;; (43 . ".\\(?:\\(\\+\\+\\|[+>]\\).?\\)")
;;              (45 . ".\\(?:\\(-[->]\\|<<\\|>>\\|[-<>|~]\\)[-<>|~]?\\)")
;;              ;; (46 . ".\\(?:\\(\\.[.<]\\|[-.=]\\)[-.<=]?\\)")
;;              (46 . ".\\(?:\\(\\.<\\|[-=]\\)[-<=]?\\)")
;;              (47 . ".\\(?:\\(//\\|==\\|[=>]\\)[/=>]?\\)")
;;              ;; (47 . ".\\(?:\\(//\\|==\\|[*/=>]\\).?\\)")
;;              (48 . ".\\(?:\\(x[a-fA-F0-9]\\).?\\)")
;;              (58 . ".\\(?:\\(::\\|[:<=>]\\)[:<=>]?\\)")
;;              (59 . ".\\(?:\\(;\\);?\\)")
;;              (60 . ".\\(?:\\(!--\\|\\$>\\|\\*>\\|\\+>\\|-[-<>|]\\|/>\\|<[-<=]\\|=[<>|]\\|==>?\\||>\\||||?\\|~[>~]\\|[$*+/:<=>|~-]\\)[$*+/:<=>|~-]?\\)")
;;              (61 . ".\\(?:\\(!=\\|/=\\|:=\\|<<\\|=[=>]\\|>>\\|[=>]\\)[=<>]?\\)")
;;              (62 . ".\\(?:\\(->\\|=>\\|>[-=>]\\|[-:=>]\\)[-:=>]?\\)")
;;              (63 . ".\\(?:\\([.:=?]\\)[.:=?]?\\)")
;;              (91 . ".\\(?:\\(|\\)[]|]?\\)")
;;              ;; (92 . ".\\(?:\\([\\n]\\)[\\]?\\)")
;;              (94 . ".\\(?:\\(=\\)=?\\)")
;;              (95 . ".\\(?:\\(|_\\|[_]\\)_?\\)")
;;              (119 . ".\\(?:\\(ww\\)w?\\)")
;;              (123 . ".\\(?:\\(|\\)[|}]?\\)")
;;              (124 . ".\\(?:\\(->\\|=>\\||[-=>]\\||||*>\\|[]=>|}-]\\).?\\)")
;;              (126 . ".\\(?:\\(~>\\|[-=>@~]\\)[-=>@~]?\\)"))))
;;       (dolist (char-regexp alist)
;;         (set-char-table-range composition-ligature-table (car char-regexp)
;;                               `([,(cdr char-regexp) 0 font-shape-gstring]))))
;;     (set-char-table-parent composition-ligature-table composition-function-table))
;; )

(package! helm-posframe)


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
