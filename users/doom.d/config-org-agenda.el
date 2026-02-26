;;; config-org-agenda.el --- Org agenda views -*- lexical-binding: t; -*-

(after! org
  ;; Phase 3 owns agenda command definitions in one place.
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("d" "Daily planning"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-overriding-header "Today timeline")))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Unscheduled actionable")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
           ((org-agenda-show-log nil)))
          ("w" "Weekly planning"
           ((agenda ""
                    ((org-agenda-span 'week)
                     (org-agenda-start-on-weekday 1)
                     (org-agenda-overriding-header "Week timeline")))
            (tags-todo "DEADLINE<=\"<+7d>\""
                       ((org-agenda-overriding-header "Weekly deadline summary")))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\""
                       ((org-agenda-overriding-header "Unscheduled actionable")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline)))))
           ((org-agenda-show-log nil))))))

(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
