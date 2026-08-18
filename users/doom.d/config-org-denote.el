;;; config-org-denote.el --- denote configuration -*- lexical-binding: t; -*-

(defvar org-life-denote-directory
  (expand-file-name "~/org/denote/")
  "Canonical directory for denote notes.")

(unless (file-directory-p org-life-denote-directory)
  (make-directory org-life-denote-directory t))

(after! denote
  (setq denote-directory org-life-denote-directory)
  (setq denote-known-keywords
        '("reference" "project" "support" "decision" "people" "meeting"))
  (setq denote-infer-keywords nil)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords)))

(map! :leader
      (:prefix ("o n" . "notes")
       :desc "Create denote note" "n" #'denote
       :desc "Open or create denote note" "o" #'denote-open-or-create
       :desc "Link or create denote note" "i" #'denote-link-or-create
       :desc "Show denote backlinks" "b" #'denote-backlinks
       :desc "Rename denote file" "r" #'denote-rename-file))

(provide 'config-org-denote)
;;; config-org-denote.el ends here
