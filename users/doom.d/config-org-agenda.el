;;; config-org-agenda.el --- Org agenda views -*- lexical-binding: t; -*-

(after! org
  ;; Phase 3 owns agenda command definitions in one place.
  (setq org-agenda-start-on-weekday 1)

  (setq org-agenda-custom-commands
        '(("d" "Daily planning"
           ((agenda ""
                    ((org-agenda-span 'day)
                     (org-agenda-overriding-header "Today timeline")))
           (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-overriding-header "Unscheduled actionable")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                        (org-super-agenda-groups org-super-agenda-groups))))
           ((org-agenda-show-log nil)
            (org-agenda-start-with-log-mode nil)
            (org-super-agenda-groups org-super-agenda-groups)
            (org-agenda-prefix-format
             '((agenda . " %(priority) %-12:c %?-12t %10e ")
                (todo . " %(priority) %-12:c %?-12t %10e ")
                (tags . " %(priority) %-12:c %?-12t %10e ")))))
          ("w" "Weekly planning"
           ((agenda ""
                     ((org-agenda-span 'week)
                      (org-agenda-start-on-weekday 1)
                      (org-agenda-overriding-header "Week timeline")))
            (tags-todo "DEADLINE<=\"<+7d>\"/!TODO|NEXT|WAITING|SOMEDAY"
                       ((org-agenda-overriding-header "Weekly deadline summary")
                        (org-super-agenda-groups org-super-agenda-groups)))
            (tags-todo "TODO=\"TODO\"|TODO=\"NEXT\"|TODO=\"WAITING\"|TODO=\"SOMEDAY\""
                       ((org-agenda-overriding-header "Unscheduled actionable")
                        (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'deadline))
                        (org-super-agenda-groups org-super-agenda-groups))))
           ((org-agenda-show-log nil)
            (org-agenda-start-with-log-mode nil)
            (org-super-agenda-groups org-super-agenda-groups)
            (org-agenda-prefix-format
             '((agenda . " %(priority) %-12:c %?-12t %10e ")
               (todo . " %(priority) %-12:c %?-12t %10e ")
               (tags . " %(priority) %-12:c %?-12t %10e "))))))))

(after! org-super-agenda
  (org-super-agenda-mode)
  (setq org-super-agenda-groups
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
          (:name "Priority B actionable"
           :and (:todo ("TODO" "NEXT") :priority "B")
           :order 19)
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
          (:name "Priority C actionable"
           :and (:todo ("TODO" "NEXT") :priority "C")
           :order 29)
          (:name "Uncategorized"
           :and (:todo ("TODO" "NEXT")
                 :not (:priority ("A" "B" "C"))
                 :not (:tag ("@home" "@work" "@computer" "@email" "@phone" "@errands")))
           :order 90)
          (:name "WAITING (parked)" :todo "WAITING" :order 98)
          (:name "SOMEDAY (parked)" :todo "SOMEDAY" :order 99))))

(map! :leader
      (:prefix ("o A" . "agenda")
       :desc "Daily planning agenda" "d" (cmd! (org-agenda nil "d"))
       :desc "Weekly planning agenda" "w" (cmd! (org-agenda nil "w"))))

(provide 'config-org-agenda)
;;; config-org-agenda.el ends here
