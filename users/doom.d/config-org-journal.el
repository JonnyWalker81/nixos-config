;;; config-org-journal.el --- org-journal configuration -*- lexical-binding: t; -*-

(defvar org-life-journal-directory
  (expand-file-name "~/org/journal/")
  "Canonical directory for org-journal daily files.")

(defvar org-life-journal-agenda-files
  (list org-life-journal-directory)
  "Journal paths explicitly scoped into agenda journal sections.")

(unless (file-directory-p org-life-journal-directory)
  (make-directory org-life-journal-directory t))

(defun org-life-journal-open-today ()
  "Open today's org-journal entry."
  (interactive)
  (require 'org-journal)
  (org-journal-new-entry nil))

(defun org-life-journal-capture-location ()
  "Return capture location in today's journal without duplicate headings."
  (require 'org-journal)
  (org-journal-new-entry t)
  (goto-char (point-max)))

(defun org-life-journal-search-history ()
  "Search across full org-journal history by default."
  (interactive)
  (require 'org-journal)
  (let ((current-prefix-arg '(4)))
    (call-interactively #'org-journal-search)))

(defun org-life-journal-mark-old-carryover-as-migrated (old-carryover)
  "Mark OLD-CARRYOVER source entries as migrated without deleting them."
  (save-excursion
    (dolist (entry (reverse old-carryover))
      (save-restriction
        (narrow-to-region (car entry) (cadr entry))
        (goto-char (point-min))
        (org-toggle-tag "migrated" 'on)))))

(defun org-life-journal--carryover-source-is-yesterday-p ()
  "Return non-nil only when yesterday is the carry-over source."
  (let* ((today-file (expand-file-name
                      (format-time-string org-journal-file-format (current-time))
                      org-journal-dir))
         (yesterday-file (expand-file-name
                          (format-time-string org-journal-file-format
                                              (time-subtract (current-time) (days-to-time 1)))
                          org-journal-dir)))
    (and (not (file-exists-p today-file))
         (file-exists-p yesterday-file))))

(defun org-life-journal--limit-carryover-to-yesterday-a (orig-fn &rest args)
  "Run ORIG-FN with carry-over enabled only when yesterday is available."
  (let ((org-journal-carryover-items
         (if (org-life-journal--carryover-source-is-yesterday-p)
             org-journal-carryover-items
           nil)))
    (apply orig-fn args)))

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
  (setq org-journal-carryover-items "TODO={.+}")
  (setq org-journal-handle-old-carryover-fn #'org-life-journal-mark-old-carryover-as-migrated)
  (advice-remove 'org-journal-new-entry #'org-life-journal--limit-carryover-to-yesterday-a)
  (advice-add 'org-journal-new-entry :around #'org-life-journal--limit-carryover-to-yesterday-a)
  (map! :leader
        (:prefix ("o j" . "journal")
         :desc "Open today's journal" "t" #'org-life-journal-open-today
         :desc "Search journal history" "s" #'org-life-journal-search-history)))

(provide 'config-org-journal)
;;; config-org-journal.el ends here
