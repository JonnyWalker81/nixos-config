;;; config-org-roam.el --- org-roam configuration -*- lexical-binding: t; -*-

(defvar org-life-roam-directory
  (file-truename (expand-file-name "~/org/roam/"))
  "Canonical org-roam directory with symlinks resolved.")

(unless (file-directory-p org-life-roam-directory)
  (make-directory org-life-roam-directory t))

(after! org
  (setq find-file-visit-truename t)
  (setq org-roam-directory org-life-roam-directory))

(after! org-roam
  (unless (and (fboundp 'sqlite-available-p) (sqlite-available-p))
    (user-error
     "org-roam requires sqlite support. Rebuild Emacs/Nix config with sqlite enabled before continuing."))
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
  (map! :leader
        (:prefix ("o r" . "roam")
         :desc "Find roam note" "f" #'org-roam-node-find
         :desc "Insert roam link" "i" #'org-roam-node-insert))
  (org-roam-db-autosync-mode 1))

(provide 'config-org-roam)
;;; config-org-roam.el ends here
