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
  (org-roam-db-autosync-mode 1))

(provide 'config-org-roam)
;;; config-org-roam.el ends here
