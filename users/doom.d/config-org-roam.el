;;; config-org-roam.el --- org-roam configuration -*- lexical-binding: t; -*-

(defvar org-life-roam-directory
  (file-truename (expand-file-name "~/org/roam/"))
  "Canonical org-roam directory with symlinks resolved.")

(unless (file-directory-p org-life-roam-directory)
  (make-directory org-life-roam-directory t))

(after! org
  (setq find-file-visit-truename t)
  (setq org-roam-directory org-life-roam-directory))

(defun org-life-roam-node-annotation (node)
  "Show aliases and filename so duplicate titles stay easy to choose."
  (let* ((aliases (org-roam-node-aliases node))
         (alias-text (when aliases (mapconcat #'identity aliases ", ")))
         (file-name (file-name-nondirectory (org-roam-node-file node))))
    (format "%s%s"
            (if alias-text
                (propertize (format " [%s]" alias-text) 'face 'org-tag)
              "")
            (propertize (format " · %s" file-name) 'face 'shadow))))

(defun org-life-roam-ui-open-local ()
  "Open org-roam-ui and focus the current note neighborhood."
  (interactive)
  (org-roam-ui-mode 1)
  (org-roam-ui-open)
  (org-roam-ui-node-local))

(after! org-roam
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "org-roam requires sqlite support. Rebuild Emacs/Nix config with sqlite enabled before continuing."))
  ;; Stable note identity policy:
  ;; - Files are created as timestamp+slug and remain stable when titles change.
  ;; - Similar/duplicate titles are allowed; aliases and filename annotations disambiguate node selection.
  ;; - Discoverability is alias-first via ROAM_ALIASES metadata rather than filename churn.
  (setq org-roam-completion-everywhere t)
  (setq org-roam-node-annotation-function #'org-life-roam-node-annotation)
  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :note:\n")
           :unnarrowed t)
          ("l" "literature" plain
           "* Summary\n%?\n\n* Key ideas\n\n* Quotes\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :literature:\n:PROPERTIES:\n:ROAM_ALIASES: %^{Aliases}\n:SOURCE: %^{Source}\n:AUTHOR: %^{Author}\n:YEAR: %^{Year}\n:END:\n")
           :unnarrowed t)
          ("c" "concept" plain
           "* Definition\n%?\n\n* Why it matters\n\n* Related\n"
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "#+title: ${title}\n#+date: %U\n#+filetags: :concept:\n:PROPERTIES:\n:ROAM_ALIASES: %^{Aliases}\n:END:\n")
           :unnarrowed t)))
  (setq org-roam-mode-sections
        '(org-roam-backlinks-section org-roam-reflinks-section))
  (setq org-roam-backlink-show-context t)
  (setq org-roam-backlinks-sort-by 'mtime)
  (setq org-roam-buffer-window-parameters
        '((no-delete-other-windows . t)
          (mode-line-format . none)))
  (use-package! org-roam-ui
    :commands (org-roam-ui-open org-roam-ui-mode)
    :config
    (setq org-roam-ui-open-on-start nil
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-sync-theme t
          org-roam-ui-sync-mode t
          org-roam-ui-find-ref-title t
          org-roam-ui-retitle-ref-nodes t))
  (map! :leader
        (:prefix ("o r" . "roam")
         :desc "Find roam note" "f" #'org-roam-node-find
         :desc "Insert roam link" "i" #'org-roam-node-insert
         :desc "Open roam graph" "g" #'org-roam-ui-open
         :desc "Open local roam graph" "l" #'org-life-roam-ui-open-local
         :desc "Roam graph mode" "u" #'org-roam-ui-mode)
        (:prefix ("n r" . "roam")
         :desc "Find roam note" "f" #'org-roam-node-find
         :desc "Insert roam link" "i" #'org-roam-node-insert
         :desc "Open roam graph" "g" #'org-roam-ui-open
         :desc "Open local roam graph" "l" #'org-life-roam-ui-open-local
         :desc "Roam graph mode" "u" #'org-roam-ui-mode))
  (org-roam-db-autosync-mode 1))

(provide 'config-org-roam)
;;; config-org-roam.el ends here
