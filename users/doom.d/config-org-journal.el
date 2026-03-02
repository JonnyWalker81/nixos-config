;;; config-org-journal.el --- org-journal configuration -*- lexical-binding: t; -*-

(defvar org-life-journal-directory
  (expand-file-name "~/org/journal/")
  "Canonical directory for org-journal daily files.")

(unless (file-directory-p org-life-journal-directory)
  (make-directory org-life-journal-directory t))

(defun org-life-journal-open-today ()
  "Open today's org-journal entry."
  (interactive)
  (require 'org-journal)
  (org-journal-new-entry))

(after! org
  (setq org-journal-dir org-life-journal-directory)
  (setq org-journal-file-type 'daily)
  (setq org-journal-file-format "%Y-%m-%d.org")
  (setq org-journal-time-format "%H:%M")
  (setq org-journal-time-prefix "* ")
  (setq org-journal-file-header
        (lambda (_time)
          (concat "#+title: Journal " (format-time-string "%Y-%m-%d (%A)") "\n"
                  "#+filetags: :journal:\n\n"
                  "* Tasks\n\n"
                  "* Journal\n\n"
                  "* End-of-day Reflection\n"
                  "- What moved forward today?\n"
                  "- What is still open?\n")))
  (map! :leader
        (:prefix ("o j" . "journal")
         :desc "Open today's journal" "t" #'org-life-journal-open-today)))

(provide 'config-org-journal)
;;; config-org-journal.el ends here
