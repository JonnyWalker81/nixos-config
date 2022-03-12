;;; config.el --- description -*- lexical-binding: t; -*-

(def-package! deadgrep
  :config
  (evil-make-overriding-map deadgrep-mode-map 'normal))

(def-package! gradle-mode
  :config
  (gradle-mode 1))

(def-package! evil-org
  :when (featurep! :feature evil +everywhere)
  :hook (org-mode . evil-org-mode)
  :init
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys)
  )

(def-package! helm-posframe
  :init
  (helm-posframe-enable)
  )

;; (def-package! evil-org
;;   :when (featurep! :feature evil +everywhere)
;;   :hook (org-mode . evil-org-mode)
;;   :init
;;   (defvar evil-org-key-theme '(navigation insert textobjects))
;;   (defvar evil-org-special-o/O '(table-row))
;;   ;; (add-hook 'org-load-hook #'+org|setup-evil)
;;   (add-hook 'evil-org-mode-hook #'evil-normalize-keymaps)
;;   :config
;;   ;; change `evil-org-key-theme' instead
;;   (advice-add #'evil-org-set-key-theme :override #'ignore)
;;   (def-package! evil-org-agenda
;;     :after org-agenda
;;     :config (evil-org-agenda-set-keys)))

;; (def-package! org-pdfview
;;   :when (featurep! :tools pdf)
;;   :commands (org-pdfview-open)
;;   :init
;;   (after! org
;;     (delete '("\\.pdf\\'" . default) org-file-apps)
;;     ;; org links to pdf files are opened in pdf-view-mode
;;     (add-to-list 'org-file-apps '("\\.pdf\\'" . (lambda (_file link) (org-pdfview-open link))))
;;     ;; support for links to specific pages
;;     (add-to-list 'org-file-apps '("\\.pdf::\\([[:digit:]]+\\)\\'" . (lambda (_file link) (org-pdfview-open link))))))

(provide 'config)
;;; config.el ends here
