;;; config-org-visual.el --- Org visual presentation ownership -*- lexical-binding: t; -*-

(use-package! org-modern
  :after org
  :hook (org-mode . org-modern-mode)
  :config
  (setq org-modern-label-border 1
        org-modern-table t
        org-modern-timestamp t
        org-modern-priority nil
        org-modern-todo-faces nil
        org-modern-priority-faces nil)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

(use-package! org-appear
  :after org
  :hook (org-mode . org-appear-mode)
  :config
  (setq org-appear-autoemphasis t
        org-appear-autolinks t
        org-appear-autosubmarkers t
        org-appear-autoentities t
        org-appear-delay 0.12
        org-appear-trigger 'always))

(after! org
  (setq org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-startup-with-latex-preview t)

  (defun org-life-visual-preview-init-h ()
    "Render inline images and LaTeX previews when opening Org buffers."
    (org-display-inline-images)
    (org-latex-preview '(16)))

  (add-hook 'org-mode-hook #'org-life-visual-preview-init-h))

(after! org-agenda
  (set-face-attribute 'org-agenda-structure nil :weight 'semibold :height 1.0)
  (set-face-attribute 'org-agenda-date nil :weight 'regular)
  (set-face-attribute 'org-agenda-date-today nil :weight 'semibold))

(after! org-super-agenda
  (set-face-attribute 'org-super-agenda-header nil :weight 'regular :inherit '(shadow default)))

(provide 'config-org-visual)
;;; config-org-visual.el ends here
