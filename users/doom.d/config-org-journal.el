;;; config-org-journal.el --- org-journal configuration -*- lexical-binding: t; -*-

(defvar org-life-journal-directory
  (expand-file-name "~/org/journal/")
  "Canonical directory for org-journal daily files.")

(unless (file-directory-p org-life-journal-directory)
  (make-directory org-life-journal-directory t))

(provide 'config-org-journal)
;;; config-org-journal.el ends here
