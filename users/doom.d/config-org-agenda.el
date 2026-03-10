;;; config-org-agenda.el --- Org agenda views -*- lexical-binding: t; -*-

(eval-and-compile
  (unless (fboundp 'after!)
    (defmacro after! (_feature &rest body)
      `(progn ,@body)))
  (unless (fboundp 'map!)
    (defmacro map! (&rest _args)
      nil)))

(after! org-agenda
  ;; Phase 3 owns agenda command definitions in one place.
  (setq org-agenda-start-on-weekday 1)

  (defun my/org-gtd-inbox-open-count ()
    "Return open TODO item count in inbox.org."
    (let* ((inbox-file (expand-file-name "~/org/gtd/inbox.org"))
           (count 0))
      (when (file-exists-p inbox-file)
        (with-current-buffer (or (find-buffer-visiting inbox-file)
                                 (find-file-noselect inbox-file))
          (org-map-entries (lambda () (setq count (1+ count)))
                           "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                           'file)))
      count))

  (defvar my/org-review-prefix-format
    '((agenda . " %-12:c %?-12t %10e ")
      (todo . " %-12:c %?-12t %10e ")
      (tags . " %-12:c %?-12t %10e "))
    "Metadata-rich agenda prefix format used by review commands.")

  (defvar org-life-agenda-default-super-groups
    '((:name "Priority A actionable"
       :and (:todo ("TODO" "NEXT") :priority "A")
       :order 0)
      (:name "Priority B @home"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@home")
       :order 10)
      (:name "Priority B @work"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@work")
       :order 11)
      (:name "Priority B @computer"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@computer")
       :order 12)
      (:name "Priority B @email"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@email")
       :order 13)
      (:name "Priority B @phone"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@phone")
       :order 14)
      (:name "Priority B @errands"
       :and (:todo ("TODO" "NEXT") :priority "B" :tag "@errands")
       :order 15)
      (:name "Priority C @home"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@home")
       :order 20)
      (:name "Priority C @work"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@work")
       :order 21)
      (:name "Priority C @computer"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@computer")
       :order 22)
      (:name "Priority C @email"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@email")
       :order 23)
      (:name "Priority C @phone"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@phone")
       :order 24)
      (:name "Priority C @errands"
       :and (:todo ("TODO" "NEXT") :priority "C" :tag "@errands")
       :order 25)
      (:name "Uncategorized"
       :and (:todo ("TODO" "NEXT")
             :not (:tag ("@home" "@work" "@computer" "@email" "@phone" "@errands")))
       :order 90)
      (:name "WAITING (parked)" :todo "WAITING" :order 98)
      (:name "SOMEDAY (parked)" :todo "SOMEDAY" :order 99))
    "Canonical fallback org-super-agenda groups for OrgLife agenda views.")

  (defvar my/org-gtd-project-files
    '("~/org/gtd/projects.org")
    "GTD files that hold active project definitions for review commands.")

  (defvar my/org-gtd-stuck-projects-definition
    '("+LEVEL=1+TODO=\"TODO\"|+LEVEL=1+TODO=\"NEXT\"" ("NEXT") nil "")
    "Org-native stuck-project criteria for GTD weekly review.
A project is a level-1 TODO/NEXT heading in projects.org with no NEXT child.")

  (defvar org-life-journal-agenda-files
    '("~/org/journal/")
    "Journal agenda scope for dedicated journal sections.")

  (defvar my/org-journal-open-todo-match
    "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
    "Match all open TODO states for journal agenda sections.")

  (defun my/org-gtd-project-has-next-child-p ()
    "Return non-nil when current project subtree contains a NEXT action."
    (save-excursion
      (let ((subtree-end (save-excursion (org-end-of-subtree t)))
            (has-next nil))
        (forward-line 1)
        (while (and (not has-next) (< (point) subtree-end))
          (when (string= (org-get-todo-state) "NEXT")
            (setq has-next t))
          (condition-case nil
              (outline-next-heading)
            (error (goto-char subtree-end))))
        has-next)))

  (defun my/org-agenda-skip-non-stuck-gtd-projects ()
    "Skip any non-stuck project when building the weekly stuck-project section."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (todo-state (org-get-todo-state))
          (level (org-current-level)))
      (cond
       ((not (= level 1)) subtree-end)
       ((not (member todo-state '("TODO" "NEXT"))) subtree-end)
       ((my/org-gtd-project-has-next-child-p) subtree-end)
       (t nil))))

  (defun org-life-agenda-super-groups-safe ()
    "Return runtime-safe org-super-agenda groups or nil fallback."
    (if (boundp 'org-super-agenda-groups)
        (or org-super-agenda-groups org-life-agenda-default-super-groups)
      nil))

  (defun org-life-agenda-prepare-runtime ()
    "Best-effort prepare org-super-agenda before opening OrgLife agendas."
    (when (require 'org-super-agenda nil t)
      (when (fboundp 'org-super-agenda-mode)
        (org-super-agenda-mode 1))
      (unless (boundp 'org-super-agenda-groups)
        (setq org-super-agenda-groups org-life-agenda-default-super-groups))))

  (defun org-life-agenda-dispatch (key)
    "Prepare agenda runtime, then dispatch org-agenda command KEY."
    (org-life-agenda-prepare-runtime)
    (org-agenda nil key))

  (setq org-agenda-custom-commands
        `(("d" "Daily planning"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-overriding-header "Today timeline")))
             (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                        ((org-agenda-overriding-header "Unscheduled actionable")
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                         (org-super-agenda-groups (org-life-agenda-super-groups-safe))))
             (tags-todo my/org-journal-open-todo-match
                        ((org-agenda-files org-life-journal-agenda-files)
                         (org-agenda-overriding-header "Journal (open TODOs, full history)")))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format
              '((agenda . " %-12:c %?-12t %10e ")
                (todo . " %-12:c %?-12t %10e ")
                (tags . " %-12:c %?-12t %10e "))))))
          ("w" "Weekly planning"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header "Week timeline")))
             (tags-todo "DEADLINE<=\"<+7d>\"/!TODO|NEXT|WAITING|SOMEDAY"
                        ((org-agenda-overriding-header "Weekly deadline summary")
                         (org-super-agenda-groups (org-life-agenda-super-groups-safe))))
             (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                        ((org-agenda-overriding-header "Unscheduled actionable")
                         (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                         (org-super-agenda-groups (org-life-agenda-super-groups-safe))))
             (tags-todo my/org-journal-open-todo-match
                        ((org-agenda-files org-life-journal-agenda-files)
                         (org-agenda-overriding-header "Journal (open TODOs, full history)")))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format
              '((agenda . " %-12:c %?-12t %10e ")
                (todo . " %-12:c %?-12t %10e ")
                (tags . " %-12:c %?-12t %10e "))))))
          ("r" "Daily Review (timeline + triage)"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-overriding-header "Daily review timeline (today)")))
            (tags-todo "+PRIORITY=\"A\"/TODO|NEXT"
                       ((org-agenda-overriding-header "Priority A actionable (open)")))
            (tags-todo "TODO=\"NEXT\""
                       ((org-agenda-overriding-header "All NEXT actions (open)")))
            (tags-todo "TODO=\"WAITING\""
                       ((org-agenda-overriding-header "WAITING follow-up (open)")))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-files '("~/org/gtd/inbox.org"))
                        (org-agenda-overriding-header
                         ,(format "Inbox triage (%d open items)" (my/org-gtd-inbox-open-count)))))
            (tags-todo my/org-journal-open-todo-match
                       ((org-agenda-files org-life-journal-agenda-files)
                         (org-agenda-overriding-header "Journal (open TODOs, full history)"))))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format my/org-review-prefix-format)))
          ("R" "Weekly Review (GTD workflow)"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header "1) Week timeline")))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-files '("~/org/gtd/inbox.org"))
                        (org-agenda-overriding-header
                         ,(format "2) Inbox triage (unprocessed: %d open items)" (my/org-gtd-inbox-open-count)))))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
                       ((org-agenda-files my/org-gtd-project-files)
                        (org-stuck-projects my/org-gtd-stuck-projects-definition)
                        (org-agenda-skip-function '(my/org-agenda-skip-non-stuck-gtd-projects))
                        (org-agenda-overriding-header
                         "3) Stuck projects (missing NEXT action)")))
            (tags-todo "TODO=\"WAITING\""
                       ((org-agenda-overriding-header "4) WAITING commitments")))
            (tags-todo "TODO=\"SOMEDAY\""
                       ((org-agenda-overriding-header "5) SOMEDAY/MAYBE parking")))
            (tags-todo my/org-journal-open-todo-match
                       ((org-agenda-files org-life-journal-agenda-files)
                         (org-agenda-overriding-header "6) Journal (open TODOs, full history)"))))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format my/org-review-prefix-format)))
          ("I" "Inbox Dashboard (triage + refile)"
           ((tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-files '("~/org/gtd/inbox.org"))
                        (org-agenda-overriding-header
                         ,(format "Inbox triage (%d open items)" (my/org-gtd-inbox-open-count)))))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-files '("~/org/gtd/projects.org"))
                        (org-agenda-overriding-header "Project next actions (refile targets)"))))
           ((org-agenda-show-log nil)
            (org-agenda-start-with-log-mode nil)
            (org-super-agenda-groups nil)
            (org-agenda-prefix-format my/org-review-prefix-format)))
          ("H" "Context Review: @home"
           ((tags-todo "+@home-@work/TODO|NEXT|WAITING"
                       ((org-agenda-overriding-header "@home actionable + waiting (open)"))))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format my/org-review-prefix-format)))
          ("W" "Context Review: @work"
            ((tags-todo "+@work-@home/TODO|NEXT|WAITING"
                        ((org-agenda-overriding-header "@work actionable + waiting (open)"))))
           ((org-agenda-show-log nil)
             (org-agenda-start-with-log-mode nil)
             (org-super-agenda-groups (org-life-agenda-super-groups-safe))
             (org-agenda-prefix-format my/org-review-prefix-format))))))

(after! org-super-agenda
  (when (fboundp 'org-super-agenda-mode)
    (org-super-agenda-mode))
  (setq org-super-agenda-groups org-life-agenda-default-super-groups))

(map! :leader
      (:prefix ("o a" . "agenda")
        :desc "Daily planning agenda" "d" (cmd! (org-agenda nil "d"))
        :desc "Inbox dashboard" "i" (cmd! (org-agenda nil "I"))
        :desc "Weekly planning agenda" "w" (cmd! (org-agenda nil "w"))
        :desc "Daily review (triage)" "r" (cmd! (org-agenda nil "r"))
        :desc "Weekly review (GTD)" "R" (cmd! (org-agenda nil "R"))
        :desc "Review @home context" "h" (cmd! (org-agenda nil "H"))
       :desc "Review @work context" "W" (cmd! (org-agenda nil "W"))))

(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
