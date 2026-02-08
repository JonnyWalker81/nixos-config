;;; config-bindings.el --- Global keybindings -*- lexical-binding: t; -*-

;; ============================================================================
;; GLOBAL KEYBINDINGS
;; ============================================================================
;; Centralized keybinding configuration
;; Note: Mode-specific keybindings are kept in their respective config files

;; ----------------------------------------------------------------------------
;; Evil Mode Behavior
;; ----------------------------------------------------------------------------

;; Preserve paste buffer when pasting over visual selection
;; When nil, replaced text won't overwrite the default register
(setq evil-kill-on-visual-paste nil)

;; ----------------------------------------------------------------------------
;; Buffer Navigation
;; ----------------------------------------------------------------------------

(map! :leader
      (:desc "buffer" :prefix "b"
       :desc "Switch workspace buffer" :n "B" #'persp-switch-to-buffer
       :desc "Switch buffer"           :n "b" #'switch-to-buffer
       :desc "FZF switch buffer"       :n "h" #'fzf-switch-buffer))

;; ----------------------------------------------------------------------------
;; File Navigation
;; ----------------------------------------------------------------------------

(map! :leader
      (:desc "file" :prefix "f"
       :desc "Find File"      :n "f" #'find-file
       :desc "FZF find files" :n "h" #'my/fzf-git-files
       :desc "Find in buffer" :n "/" #'consult-line))

;; Override SPC SPC to use consult-fd for better file finding
(map! :leader
      :desc "Find file (fd)" "SPC" #'consult-fd)

;; ----------------------------------------------------------------------------
;; Git Keybindings
;; ----------------------------------------------------------------------------

(map! :leader
      (:desc "git" :prefix "g"
       (:desc "diff" :prefix "d"
        :desc "Diff file at point vs origin/master" :n "f" #'jr/git-diff-file-at-point-vs-master)
       (:desc "file" :prefix "f"
        :desc "Browse files in branch" :n "b" #'jr/git-branch-files)
       :desc "Copy GitHub file link"      :n "y" #'jr/copy-github-file-link
       :desc "Copy GitHub file+line link" :n "Y" #'jr/copy-github-file-line-link
       :desc "Generate AI commit message" :n "m" #'jr/ai-generate-commit-message))

;; ----------------------------------------------------------------------------
;; Code/LSP Navigation
;; ----------------------------------------------------------------------------

(map! :leader :desc "dumb-jump-go" "d" #'dumb-jump-go)
(map! :leader :desc "dumb-jump-go other window" "D" #'dumb-jump-go-other-window)

(map! :leader
      (:desc "code" :prefix "c"
       :desc "Fold code"   :n "f" #'fold-this
       :desc "Unfold code" :n "u" #'fold-this-unfold-at-point))

;; ----------------------------------------------------------------------------
;; Toggle Commands
;; ----------------------------------------------------------------------------

(map! :leader
      (:desc "toggle" :prefix "t"
       :desc "Toggle LSP doc" :n "d" #'lsp-ui-doc-toggle))

;; ----------------------------------------------------------------------------
;; LSP Prefix
;; ----------------------------------------------------------------------------

(map! :leader
      (:desc "lsp" :prefix "l"
       :desc "LSP mode" :n "l" #'lsp-mode
       :desc "Costs/reporting PRs" :n "p" #'(lambda ()
                                               (interactive)
                                               (forge-list-labeled-pullreqs
                                                (forge-get-repository t)
                                                "costs-and-reporting-pod"))))

;; ----------------------------------------------------------------------------
;; Window Navigation
;; ----------------------------------------------------------------------------

;; Window navigation with Ctrl + vim movement keys
(map! :n "C-h" #'evil-window-left
      :n "C-j" #'evil-window-down
      :n "C-k" #'evil-window-up
      :n "C-l" #'evil-window-right)

;; ----------------------------------------------------------------------------
;; Path/Branch Copy Commands
;; ----------------------------------------------------------------------------

(map! :leader
      (:prefix ("y" . "yank/copy")
       :desc "Copy file path"   :n "f" #'jr/copy-file-path
       :desc "Copy branch name" :n "b" #'jr/copy-branch-name))

;; ----------------------------------------------------------------------------
;; UUID Commands
;; ----------------------------------------------------------------------------

(map! :leader
      (:prefix ("i u" . "UUID")
       :desc "Insert UUIDv4"          "4" #'jr/insert-uuidv4
       :desc "Insert UUIDv7"          "7" #'jr/insert-uuidv7
       :desc "Insert readable UUIDv4" "r" #'jr/insert-readable-uuidv4
       :desc "Insert readable UUIDv7" "R" #'jr/insert-readable-uuidv7
       :desc "Copy UUIDv4"            "c" #'jr/copy-uuidv4
       :desc "Copy UUIDv7"            "C" #'jr/copy-uuidv7
       :desc "Replace with UUIDv4"    "x" #'jr/replace-uuid-at-point-with-v4
       :desc "Replace with UUIDv7"    "X" #'jr/replace-uuid-at-point-with-v7))

;; ----------------------------------------------------------------------------
;; SQL Commands
;; ----------------------------------------------------------------------------

(map! :leader
      (:prefix ("c q" . "SQL/Query")
       :desc "Edit SQL string"       "e" #'jr/edit-sql-string-at-point
       :desc "Format SQL string"     "f" #'jr/format-sql-string-at-point
       :desc "Format all SQL strings" "F" #'jr/format-all-sql-strings
       :desc "Toggle polymode"       "p" #'jr/toggle-polymode))

(provide 'config-bindings)
;;; config-bindings.el ends here
