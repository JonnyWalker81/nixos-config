;;; config-bindings.elj -*- lexical-binding: t; -*-

(map! :leader
      (:desc "buffer" :prefix "b"
        :desc "Switch workspace buffer" :n "B" #'persp-switch-to-buffer
        :desc "Switch buffer"           :n "b" #'switch-to-buffer))

(map! :leader
      (:desc "file" :prefix "f"
        :desc "Find File" :n "f" #'find-file))

(map! :leader
      (:desc "git" :prefix "g"
        (:desc "diff" :prefix "d"
          :desc "Diff file at point vs origin/master" :n "f" #'jr/git-diff-file-at-point-vs-master)))


(provide 'config-bindings)
;;; config-bindings.el ends here
