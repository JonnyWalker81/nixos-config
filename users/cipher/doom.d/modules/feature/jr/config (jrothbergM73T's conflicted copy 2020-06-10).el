;;config.el-- - description - *-lexical - binding : t; -*-

(def-package! deadgrep
  :config
  (evil-make-overriding-map deadgrep-mode-map 'normal))

;;(def-package! clang-format
;;  :config
  ;; (add-hook 'c-mode-common-hook (lambda () (add-hook 'before-save-hook 'clang-format-buffer)))
;;  )


(provide 'config)
;;; config.el ends here
