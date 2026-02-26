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
                     ("~/org/gtd/meetings.org"   "Meetings"  "Meeting notes and action items")
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
  ;; Capture
  ;; --------------------------------------------------------------------------
  ;; Canonical Phase 2 mnemonic keys:
  ;; t = task, i = idea, p = project, m = meeting

  (setq org-capture-templates
        '(("t" "Task" entry
           (file+headline "~/org/gtd/inbox.org" "Tasks")
           "* TODO %^{Task title} :%^{Context|@home|@work|@errands|@phone|@computer|@email}:\n%U\n%a\n"
           :empty-lines 1)
          ("i" "Idea" entry
           (file+headline "~/org/gtd/inbox.org" "Ideas")
           "* %U %^{Idea}\n"
           :empty-lines 1
           :immediate-finish t)
          ("p" "Project" entry
           (file "~/org/gtd/projects.org")
           "* TODO %^{Project name}\nDEADLINE: %^t\n:PROPERTIES:\n:CREATED: %U\n:END:\n** Outcome\n** Notes\n** Next Actions\n"
           :empty-lines 1)
          ("m" "Meeting" entry
           (file "~/org/gtd/meetings.org")
           "* %^{Meeting title}\nSCHEDULED: %^t\n:PROPERTIES:\n:ATTENDEES: %^{Attendees (optional)|}\n:CREATED: %U\n:END:\n** Notes\n%?\n** Action Items\n*** TODO %^{Action item}\n:PROPERTIES:\n:INBOX_LINK: %^{INBOX_LINK (optional, e.g. id:... from inbox.org)|}\n:END:\n"
           :empty-lines 1)))

  (defun my/org-capture-dwim-key ()
    "Return the capture template key based on current buffer context."
    (let ((current-file (when buffer-file-name
                          (expand-file-name buffer-file-name))))
      (cond
       ((equal current-file (expand-file-name "~/org/gtd/meetings.org")) "m")
       ((equal current-file (expand-file-name "~/org/gtd/projects.org")) "p")
       (t "t"))))

  (defun my/org-capture-dwim ()
    "Start org capture with a context-aware default template."
    (interactive)
    (org-capture nil (my/org-capture-dwim-key)))

  (global-set-key (kbd "C-c c") #'my/org-capture-dwim)
  (global-set-key (kbd "C-c C") #'org-capture)

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

  ;; --------------------------------------------------------------------------
  ;; Effort Estimation
  ;; --------------------------------------------------------------------------
  ;; T-shirt sizes for quick effort estimation.
  ;; Mapped to approximate time values for filtering/sorting.
  ;; Set effort with C-c C-x e (org-set-effort).

  (setq org-effort-durations
        '(("XS" . 15)      ;; 15 minutes
          ("S"  . 30)      ;; 30 minutes
          ("M"  . 60)      ;; 1 hour
          ("L"  . 120)     ;; 2 hours
          ("XL" . 240)))   ;; 4 hours

  ;; Column view format showing effort alongside TODO state and priority
  (setq org-columns-default-format
        "%40ITEM(Task) %TODO %3PRIORITY %10Effort(Effort){:} %TAGS")

  ;; Effort allowed values for quick selection
  (setq org-global-properties
        '(("Effort_ALL" . "XS S M L XL")))

  ;; --------------------------------------------------------------------------
  ;; TODO Keyword Faces
  ;; --------------------------------------------------------------------------
  ;; Color-code TODO states for at-a-glance status recognition.
  ;; Active states are bright/bold, closed states are muted.

  (setq org-todo-keyword-faces
        '(("TODO"      . (:foreground "#ff6c6b" :weight bold))
          ("NEXT"      . (:foreground "#51afef" :weight bold))
          ("WAITING"   . (:foreground "#ECBE7B" :weight bold))
          ("SOMEDAY"   . (:foreground "#a9a1e1"))
          ("DONE"      . (:foreground "#98be65"))
          ("CANCELLED" . (:foreground "#5B6268" :strike-through t))))

  ;; --------------------------------------------------------------------------
  ;; Refile
  ;; --------------------------------------------------------------------------
  ;; Refile targets: all GTD files, max 2 levels deep.
  ;; Shows file + heading path for clarity (e.g., "projects.org/Project A").
  ;; Uses vertico/orderless fuzzy completion (Doom provides this automatically).

  (setq org-refile-targets
        '(("~/org/gtd/inbox.org"     :maxlevel . 2)
          ("~/org/gtd/projects.org"  :maxlevel . 2)
          ("~/org/gtd/someday.org"   :maxlevel . 2)
          ("~/org/gtd/reference.org" :maxlevel . 2)))

  ;; Show full outline path in refile completion (file/heading/subheading)
  (setq org-refile-use-outline-path 'file)

  ;; Don't complete in steps — show full paths in a single completion interface
  ;; This works with vertico/orderless for fuzzy matching
  (setq org-outline-path-complete-in-steps nil)

  ;; Allow creating new parent nodes during refile (confirm first)
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Cache refile targets for speed (clear cache with C-u C-u C-c C-w)
  (setq org-refile-use-cache t)

  ;; --------------------------------------------------------------------------
  ;; Archive
  ;; --------------------------------------------------------------------------
  ;; Per-source archiving: each GTD file archives to its own archive file
  ;; in ~/org/gtd/archive/ directory.
  ;; e.g., inbox.org → archive/inbox.org_archive
  ;; Manual archive: C-c C-x C-a (org-archive-subtree-default)

  (setq org-archive-location "~/org/gtd/archive/%s_archive::")

  ;; Save context info when archiving (where it came from, when, etc.)
  (setq org-archive-save-context-info
        '(time file ltags itags todo category olpath))

  ;; --------------------------------------------------------------------------
  ;; Auto-archive stale items
  ;; --------------------------------------------------------------------------
  ;; Archive DONE/CANCELLED items that have been closed for 30+ days.
  ;; Call manually: M-x org-gtd-archive-stale
  ;; This is intentionally manual, not automatic, to avoid surprise archiving.

  (defvar org-gtd-archive-stale-days 30
    "Number of days after which DONE/CANCELLED items are considered stale for archiving.")

  (defun org-gtd-archive-stale ()
    "Archive all DONE/CANCELLED items closed more than `org-gtd-archive-stale-days' days ago.
Operates on all GTD agenda files."
    (interactive)
    (let ((cutoff (time-subtract (current-time)
                                 (days-to-time org-gtd-archive-stale-days)))
          (archived-count 0))
      (dolist (file (org-agenda-files))
        (with-current-buffer (or (find-buffer-visiting file)
                                 (find-file-noselect file))
          (org-map-entries
           (lambda ()
             (let ((closed-str (org-entry-get nil "CLOSED")))
               (when (and closed-str
                          (time-less-p (org-time-string-to-time closed-str) cutoff))
                 (org-archive-subtree)
                 (setq archived-count (1+ archived-count)))))
           "/DONE|CANCELLED" 'file)))
      (message "Archived %d stale items (closed > %d days ago)"
               archived-count org-gtd-archive-stale-days)))

  ) ;; end after! org

(provide 'config-org-gtd)
;;; config-org-gtd.el ends here
