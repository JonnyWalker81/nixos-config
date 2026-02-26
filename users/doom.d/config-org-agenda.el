;;; config-org-agenda.el --- Org agenda views -*- lexical-binding: t; -*-

(after! org
  ;; Phase 3 owns agenda command definitions in one place.
  (setq org-agenda-custom-commands
        '(("d" "Daily planning"
           ((agenda "")))
          ("w" "Weekly planning"
           ((agenda ""))))))

(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
