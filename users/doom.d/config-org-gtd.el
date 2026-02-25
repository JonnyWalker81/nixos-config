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

  ;; --------------------------------------------------------------------------
  ;; TODO States
  ;; --------------------------------------------------------------------------
  ;; 4 active states | 2 closed states
  ;; Fast-selection keys in parentheses: t=TODO, n=NEXT, w=WAITING, s=SOMEDAY, d=DONE, c=CANCELLED
  ;; The pipe (|) separates active from closed states.
  ;; Transitions are unrestricted — any state can move to any other.

  (setq org-todo-keywords
        '((sequence
           "TODO(t)" "NEXT(n)" "WAITING(w@/!)" "SOMEDAY(s)"
           "|"
            "DONE(d!)" "CANCELLED(c!)")))

  ;; --------------------------------------------------------------------------
  ;; State Change Logging
  ;; --------------------------------------------------------------------------
  ;; Log state changes into a LOGBOOK drawer (not inline under heading)
  ;; Log DONE timestamp, log repeat state changes, log refile, log reschedule

  (setq org-log-done 'time)              ;; Log timestamp when task marked DONE
  (setq org-log-into-drawer "LOGBOOK")   ;; All state logs go into LOGBOOK drawer
  (setq org-log-repeat 'time)            ;; Log when repeating task resets state
  (setq org-log-refile 'time)            ;; Log timestamp when item is refiled
  (setq org-log-reschedule 'time)        ;; Log when scheduled date changes
  (setq org-log-redeadline 'time)        ;; Log when deadline changes

  ;; --------------------------------------------------------------------------
  ;; GTD Context Tags
  ;; --------------------------------------------------------------------------
  ;; 6 context tags for GTD next-action filtering.
  ;; Tags are NOT mutually exclusive — a task can have multiple contexts.
  ;; Tags inherit from parent headings (org default behavior).
  ;; Fast-tag selection: press the shortcut key to toggle the tag.

  (setq org-tag-alist
        '(("@home"     . ?h)
          ("@work"     . ?w)
          ("@errands"  . ?e)
          ("@phone"    . ?p)
          ("@computer" . ?c)
          ("@email"    . ?m)))

  ;; --------------------------------------------------------------------------
  ;; Priorities
  ;; --------------------------------------------------------------------------
  ;; A/B/C priorities with visually distinct color-coded faces.
  ;; Colors chosen to match doom-tokyo-night palette.

  (setq org-priority-highest ?A)
  (setq org-priority-lowest  ?C)
  (setq org-priority-default ?B)

  (setq org-priority-faces
        '((?A . (:foreground "#ff6c6b" :weight bold))
          (?B . (:foreground "#ECBE7B" :weight bold))
          (?C . (:foreground "#98be65"))))

  ) ;; end after! org

(provide 'config-org-gtd)
;;; config-org-gtd.el ends here
