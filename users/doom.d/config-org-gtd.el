;;; config-org-gtd.el --- GTD workflow configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; GTD FOUNDATION
;; ============================================================================
;; Core GTD primitives: org-directory, file structure, TODO states, context tags,
;; refile, priorities, and effort properties.
;;
;; All settings use (after! org) to survive Doom Emacs overrides.

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
