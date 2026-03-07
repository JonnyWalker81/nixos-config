;;; config-org-visual.el --- Org visual presentation ownership -*- lexical-binding: t; -*-

(use-package! org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-label-border 1
        org-modern-table t
        org-modern-timestamp t
        org-modern-priority nil)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(after! org-agenda
  (set-face-attribute 'org-agenda-structure nil :weight 'semibold :height 1.0)
  (set-face-attribute 'org-agenda-date nil :weight 'regular)
  (set-face-attribute 'org-agenda-date-today nil :weight 'semibold))

(after! org-super-agenda
  (set-face-attribute 'org-super-agenda-header nil :weight 'regular :inherit '(shadow default)))

(provide 'config-org-visual)
;;; config-org-visual.el ends here
