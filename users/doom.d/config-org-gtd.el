;;; config-org-gtd.el --- GTD workflow configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; GTD FOUNDATION
;; ============================================================================
;; Core GTD primitives: org-directory, file structure, TODO states, context tags,
;; refile, priorities, and effort properties.
;;
;; All settings use (after! org) to survive Doom Emacs overrides.

;; --------------------------------------------------------------------------
;; Auto-create GTD directory structure
;; --------------------------------------------------------------------------
;; Ensure directories exist on first run (idempotent)

(dolist (dir '("~/org/" "~/org/gtd/" "~/org/gtd/archive/"))
  (unless (file-directory-p (expand-file-name dir))
    (make-directory (expand-file-name dir) t)))

;; Create GTD files with boilerplate if they don't exist
(dolist (file-spec '(("~/org/gtd/inbox.org"     "Inbox"     "Capture landing zone — process to zero regularly")
                     ("~/org/gtd/projects.org"   "Projects"  "Active projects with sub-tasks")
                     ("~/org/gtd/someday.org"    "Someday"   "Maybe/someday items — review weekly")
                     ("~/org/gtd/reference.org"  "Reference" "Non-actionable reference material")))
  (let ((filepath (expand-file-name (nth 0 file-spec)))
        (title (nth 1 file-spec))
        (desc (nth 2 file-spec)))
    (unless (file-exists-p filepath)
      (with-temp-file filepath
        (insert (format "#+title: %s\n#+filetags:\n\n# %s\n" title desc))))))

(after! org
  ;; --------------------------------------------------------------------------
  ;; Directory & File Structure
  ;; --------------------------------------------------------------------------
  ;; ~/org/ is the umbrella for all org subsystems:
  ;;   ~/org/gtd/     — GTD agenda files (inbox, projects, someday, reference)
  ;;   ~/org/roam/    — org-roam knowledge base (Phase 5)
  ;;   ~/org/journal/ — org-journal daily entries (Phase 6)
  ;;   ~/org/denote/  — denote structured notes (Phase 6)

  (setq org-directory "~/org/")

  ;; Agenda scoped to GTD files only — prevents roam/journal noise in agenda
  (setq org-agenda-files '("~/org/gtd/"))

  ) ;; end after! org

(provide 'config-org-gtd)
;;; config-org-gtd.el ends here
