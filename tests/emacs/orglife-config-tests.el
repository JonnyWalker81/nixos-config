;;; orglife-config-tests.el --- Automated OrgLife config checks -*- lexical-binding: t; -*-

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'org)

(defconst orglife-test-repo-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(defun orglife-test-path (relative)
  (expand-file-name relative orglife-test-repo-root))

(defmacro orglife-test-with-temp-home (&rest body)
  `(let* ((temp-home (make-temp-file "orglife-test-home-" t))
          (process-environment (copy-sequence process-environment))
          (user-emacs-directory (expand-file-name ".emacs.d/" temp-home))
          (org-directory nil)
          (org-agenda-files nil))
     (setenv "HOME" temp-home)
     (setq user-home-directory temp-home)
     (unwind-protect
         (progn ,@body)
       (delete-directory temp-home t))))

(defvar orglife-test-map-calls nil)
(defvar orglife-test-roam-autosync-arg nil)
(defvar orglife-test-dashboard-open-calls nil)
(defvar orglife-test-dashboard-reload-calls nil)
(defvar orglife-test-dashboard-reload-slash-calls nil)

(defmacro after! (_feature &rest body)
  `(progn ,@body))

(defmacro map! (&rest args)
  `(push ',args orglife-test-map-calls))

(defmacro cmd! (&rest body)
  `(lambda () (interactive) ,@body))

(defun orglife-test--normalize-hook-spec (hook-spec)
  (cond
   ((null hook-spec) nil)
   ((and (consp hook-spec) (symbolp (car hook-spec))) (list hook-spec))
   ((and (listp hook-spec) (consp (car hook-spec))) hook-spec)
   (t nil)))

(defun orglife-test--hook-var (hook)
  (let ((name (symbol-name hook)))
    (if (string-suffix-p "-hook" name)
        hook
      (intern (format "%s-hook" name)))))

(defun orglife-test--eval-use-package (_pkg args)
  (let ((hook-spec nil)
        (config-forms nil)
        (init-forms nil)
        key val)
    (while args
      (setq key (pop args))
      (pcase key
        (:hook
         (setq hook-spec (pop args)))
        (:init
         (let (forms)
           (while (and args (not (keywordp (car args))))
             (push (pop args) forms))
           (setq init-forms (append init-forms (nreverse forms)))))
        (:config
         (let (forms)
           (while (and args (not (keywordp (car args))))
             (push (pop args) forms))
           (setq config-forms (append config-forms (nreverse forms)))))
        (_
         (setq val (car args))
         (when (or (null val) (not (keywordp val)))
           (pop args)))))
    (dolist (form init-forms)
      (eval form t))
    (dolist (hook-pair (orglife-test--normalize-hook-spec hook-spec))
      (add-hook (orglife-test--hook-var (car hook-pair)) (cdr hook-pair)))
    (dolist (form config-forms)
      (eval form t))))

(defmacro use-package! (pkg &rest args)
  `(orglife-test--eval-use-package ',pkg ',args))

(defun orglife-test-reset-state ()
  (setq orglife-test-map-calls nil)
  (setq orglife-test-roam-autosync-arg nil)
  (setq orglife-test-dashboard-open-calls nil)
  (setq orglife-test-dashboard-reload-calls nil)
  (setq orglife-test-dashboard-reload-slash-calls nil)
  (setq org-agenda-custom-commands nil)
  (setq org-super-agenda-groups nil)
  (setq org-capture-templates nil)
  (setq org-journal-dir nil)
  (setq denote-directory nil)
  (setq org-roam-capture-templates nil)
  (setq initial-buffer-choice nil)
  (setq +doom-dashboard-functions '(doom-dashboard-banner-widget))
  (setq org-mode-hook nil)
  (setq org-agenda-finalize-hook nil)
  (dolist (open-fn '(+doom-dashboard/open doom/open-dashboard))
    (when (fboundp open-fn)
      (advice-remove open-fn #'org-life-dashboard--refresh-after-open)))
  (dolist (sym '(org-life-journal-directory
                 org-life-journal-agenda-files
                 org-life-denote-directory
                 org-life-roam-directory))
    (when (boundp sym)
      (makunbound sym))))

(defun orglife-test-install-stubs ()
  (dolist (fn '(org-modern-agenda
                org-super-agenda-mode
                org-roam-ui-open
                org-roam-ui-node-local
                org-roam-ui-mode
                org-roam-node-find
                org-roam-node-insert
                org-journal-search
                org-capture
                my/org-capture-dwim
                org-display-inline-images
                org-latex-preview))
    (unless (fboundp fn)
      (defalias fn (lambda (&rest _args) nil))))
  (unless (fboundp 'org-life-roam-node-find)
    (defalias 'org-life-roam-node-find (lambda (&rest _args) nil)))
  (unless (fboundp 'org-life-journal-open-today)
    (defalias 'org-life-journal-open-today (lambda (&rest _args) nil)))
  (unless (fboundp 'org-life-journal-search-history)
    (defalias 'org-life-journal-search-history (lambda (&rest _args) nil)))
  (unless (fboundp '+doom-dashboard/open)
    (defalias '+doom-dashboard/open
      (lambda (&rest _args)
        (setq orglife-test-dashboard-open-calls (1+ (or orglife-test-dashboard-open-calls 0))))))
  (unless (fboundp 'doom/open-dashboard)
    (defalias 'doom/open-dashboard
      (lambda (&rest _args)
        (setq orglife-test-dashboard-open-calls (1+ (or orglife-test-dashboard-open-calls 0))))))
  (unless (fboundp '+doom-dashboard-reload)
    (defalias '+doom-dashboard-reload
      (lambda (&rest _args)
        (setq orglife-test-dashboard-reload-calls (1+ (or orglife-test-dashboard-reload-calls 0))))))
  (unless (fboundp '+doom-dashboard/reload)
    (defalias '+doom-dashboard/reload
      (lambda (&rest _args)
        (setq orglife-test-dashboard-reload-slash-calls (1+ (or orglife-test-dashboard-reload-slash-calls 0))))))
  (defalias 'sqlite-available-p (lambda () t))
  (unless (facep 'org-super-agenda-header)
    (defface org-super-agenda-header '((t :inherit default)) "Test face stub."))
  (defalias 'org-roam-db-autosync-mode
    (lambda (&optional arg)
      (setq orglife-test-roam-autosync-arg arg)))
  (unless (fboundp 'org-journal-new-entry)
    (defalias 'org-journal-new-entry (lambda (&rest _args) nil))))

(defun orglife-test-load (file)
  (load (orglife-test-path file) nil t))

(defun orglife-test-template-by-key (key)
  (seq-find (lambda (tpl) (string= (car tpl) key)) org-capture-templates))

(defun orglife-test-map-call-contains (snippet)
  (seq-some
   (lambda (call)
     (string-match-p (regexp-quote snippet) (prin1-to-string call)))
   orglife-test-map-calls))

(defun orglife-test-write-file (path content)
  (make-directory (file-name-directory path) t)
  (with-temp-file path
    (insert content)))

(defun orglife-test-heading-id (file)
  (with-current-buffer (find-file-noselect file)
    (goto-char (point-min))
    (org-back-to-heading t)
    (prog1 (org-id-get-create)
      (save-buffer))))

(ert-deftest orglife-init-enables-roam-and-journal-modules ()
  (let ((content (with-temp-buffer
                   (insert-file-contents (orglife-test-path "users/doom.d/init.el"))
                   (buffer-string))))
    (should (string-match-p "\\+roam" content))
    (should (string-match-p "\\+journal" content))))

(ert-deftest orglife-config-load-order-includes-visual-last-in-org-chain ()
  (let* ((content (with-temp-buffer
                    (insert-file-contents (orglife-test-path "users/doom.d/config.el"))
                    (buffer-string)))
         (gtd-pos (string-match "(load! \"config-org-gtd\")" content))
         (agenda-pos (string-match "(load! \"config-org-agenda\")" content))
         (roam-pos (string-match "(load! \"config-org-roam\")" content))
         (journal-pos (string-match "(load! \"config-org-journal\")" content))
         (denote-pos (string-match "(load! \"config-org-denote\")" content))
         (visual-pos (string-match "(load! \"config-org-visual\")" content)))
    (should gtd-pos)
    (should agenda-pos)
    (should roam-pos)
    (should journal-pos)
    (should denote-pos)
    (should visual-pos)
    (should (< gtd-pos visual-pos))
    (should (< agenda-pos visual-pos))
    (should (< roam-pos visual-pos))
    (should (< journal-pos visual-pos))
    (should (< denote-pos visual-pos))))

(ert-deftest orglife-gtd-foundation-settings-are-canonical ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-gtd.el")
   (should (equal org-directory "~/org/"))
   (should (equal org-agenda-files '("~/org/gtd/")))
   (should (equal org-log-into-drawer "LOGBOOK"))
   (should (equal org-priority-default ?B))
   (should (equal org-effort-durations '(("XS" . 15) ("S" . 30) ("M" . 60) ("L" . 120) ("XL" . 240))))
   (should (equal (alist-get "Effort_ALL" org-global-properties nil nil #'string=) "XS S M L XL"))
   (should (assoc "NEXT" org-todo-keyword-faces))
   (should (assoc "WAITING" org-todo-keyword-faces))
   (should (assoc "CANCELLED" org-todo-keyword-faces))
   (should (equal org-archive-location "~/org/gtd/archive/%s_archive::"))
   (should (fboundp 'org-gtd-archive-stale))
   (should (equal org-gtd-archive-stale-days 30))))

(ert-deftest orglife-gtd-bootstrap-creates-org-files ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-gtd.el")
   (dolist (path '("~/org/gtd/inbox.org"
                   "~/org/gtd/projects.org"
                   "~/org/gtd/meetings.org"
                   "~/org/gtd/someday.org"
                   "~/org/gtd/reference.org"))
     (should (file-exists-p (expand-file-name path))))))

(ert-deftest orglife-capture-templates-and-entrypoints-exist ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-journal.el")
   (orglife-test-load "users/doom.d/config-org-gtd.el")
   (dolist (key '("t" "i" "p" "m" "j"))
     (should (orglife-test-template-by-key key)))
   (should (fboundp 'my/org-capture-dwim))
   (should (fboundp 'my/org-capture-dwim-key))
   (should (fboundp 'my/org-gtd-open-inbox))
   (should (eq (key-binding (kbd "C-c c")) #'my/org-capture-dwim))
   (should (eq (key-binding (kbd "C-c C")) #'org-capture))))

(ert-deftest orglife-agenda-commands-and-super-groups-are-configured ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-agenda.el")
   (let ((keys (mapcar #'car org-agenda-custom-commands)))
     (dolist (key '("d" "w" "r" "R" "I" "H" "W"))
       (should (member key keys))))
   (should (plist-member (car org-super-agenda-groups) :name))
   (should (seq-find (lambda (group) (equal (plist-get group :name) "WAITING (parked)")) org-super-agenda-groups))
   (should (seq-find (lambda (group) (equal (plist-get group :name) "SOMEDAY (parked)")) org-super-agenda-groups))
   (should (fboundp 'my/org-agenda-skip-non-stuck-gtd-projects))
   (should (equal my/org-gtd-project-files '("~/org/gtd/projects.org")))))

(ert-deftest orglife-roam-settings-and-wrappers-are-loaded ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-roam.el")
   (should (string-match-p "/org/roam/$" org-life-roam-directory))
   (should (equal org-roam-completion-everywhere t))
   (should (equal (mapcar #'car org-roam-capture-templates) '("d" "l" "c")))
   (should (equal org-roam-backlinks-sort-by 'mtime))
   (should (equal orglife-test-roam-autosync-arg 1))
   (should (fboundp 'org-life-roam-node-find))
   (should (fboundp 'org-life-roam-node-insert))
   (should (fboundp 'org-life-roam-ui-open-local))))

(ert-deftest orglife-journal-carryover-and-keybindings-are-wired ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-journal.el")
   (should (equal org-journal-dir (expand-file-name "~/org/journal/")))
   (should (equal org-journal-file-format "%Y-%m-%d.org"))
   (should (equal org-journal-carryover-items "TODO={.+}"))
   (should (eq org-journal-handle-old-carryover-fn #'org-life-journal-mark-old-carryover-as-migrated))
   (should (fboundp 'org-life-journal-capture-location))
   (should (fboundp 'org-life-journal-open-today))
   (should (fboundp 'org-life-journal-search-history))))

(ert-deftest orglife-denote-directory-and-keywords-are-locked ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-denote.el")
   (should (equal denote-directory (expand-file-name "~/org/denote/")))
   (should (equal denote-infer-keywords nil))
   (should (equal denote-prompts '(title keywords)))
   (should (equal denote-known-keywords '("reference" "project" "support" "decision" "people" "meeting")))))

(ert-deftest orglife-visual-polish-hooks-and-semantics-are-configured ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-gtd.el")
   (orglife-test-load "users/doom.d/config-org-visual.el")
   (should (memq #'org-modern-mode org-mode-hook))
   (should (memq #'org-appear-mode org-mode-hook))
   (should (memq #'org-life-visual-preview-init-h org-mode-hook))
   (should (memq #'org-modern-agenda org-agenda-finalize-hook))
   (should org-hide-emphasis-markers)
   (should org-startup-with-inline-images)
   (should org-startup-with-latex-preview)
   (should (= org-appear-delay 0.12))
   (should (equal org-modern-priority nil))
   (should (equal org-modern-todo-faces nil))
   (let ((todo-face (cdr (assoc "TODO" org-todo-keyword-faces)))
         (next-face (cdr (assoc "NEXT" org-todo-keyword-faces)))
         (waiting-face (cdr (assoc "WAITING" org-todo-keyword-faces))))
     (should (equal (plist-get todo-face :foreground) "#e06c75"))
     (should (equal (plist-get next-face :foreground) "#61afef"))
     (should (equal (plist-get waiting-face :foreground) "#d19a66")))))

(ert-deftest orglife-keybinding-declarations-cover-orglife-workflows ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-journal.el")
   (orglife-test-load "users/doom.d/config-org-denote.el")
   (orglife-test-load "users/doom.d/config-org-gtd.el")
   (orglife-test-load "users/doom.d/config-org-agenda.el")
   (orglife-test-load "users/doom.d/config-org-roam.el")
   (dolist (snippet '("\"o g\""
                      "\"o a\""
                      "\"o r\""
                      "\"n r\""
                      "\"o j\""
                      "\"o n\""))
     (should (orglife-test-map-call-contains snippet)))
   (dolist (snippet '("\"d\"" "\"w\"" "\"r\"" "\"R\"" "\"i\"" "\"h\"" "\"W\""))
     (should (orglife-test-map-call-contains snippet)))
   (dolist (snippet '("\"f\"" "\"g\"" "\"l\"" "\"u\""))
     (should (orglife-test-map-call-contains snippet)))
    (dolist (snippet '("\"t\"" "\"s\""))
      (should (orglife-test-map-call-contains snippet)))))

(ert-deftest orglife-dashboard-registers-widgets-startup-and-refresh-wiring ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (should (equal initial-buffer-choice #'org-life-dashboard-open))
   (should (equal
            (last +doom-dashboard-functions 4)
            '(org-life-dashboard-widget-today
              org-life-dashboard-widget-inbox
              org-life-dashboard-widget-deadlines
              org-life-dashboard-widget-quick-actions)))
   (should (advice-member-p #'org-life-dashboard--refresh-after-open #'+doom-dashboard/open))
   (should (advice-member-p #'org-life-dashboard--refresh-after-open #'doom/open-dashboard))
   (setq orglife-test-dashboard-reload-calls 0)
   (setq orglife-test-dashboard-open-calls 0)
   (org-life-dashboard-open)
   (should (= orglife-test-dashboard-open-calls 1))
   (should (= orglife-test-dashboard-reload-calls 2))))

(ert-deftest orglife-dashboard-quick-actions-dispatch-core-workflows-deterministically ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (let (capture-policy daily-policy weekly-policy roam-policy)
     (cl-letf (((symbol-function 'org-life-dashboard--with-display-policy)
                (lambda (policy fn)
                  (pcase policy
                    ('capture (setq capture-policy policy))
                    ('agenda (if daily-policy
                                 (setq weekly-policy policy)
                               (setq daily-policy policy)))
                    ('roam (setq roam-policy policy)))
                  (funcall fn)))
               ((symbol-function 'my/org-capture-dwim)
                (lambda ()
                  (interactive)
                  (setq capture-policy (or capture-policy 'missing))))
               ((symbol-function 'org-agenda)
                (lambda (_prefix key)
                  (cond
                   ((string= key "r") (setq daily-policy (or daily-policy 'missing)))
                   ((string= key "R") (setq weekly-policy (or weekly-policy 'missing))))))
               ((symbol-function 'org-life-roam-node-find)
                (lambda ()
                  (interactive)
                  (setq roam-policy (or roam-policy 'missing)))))
       (org-life-dashboard-action-capture)
       (org-life-dashboard-action-daily-review)
       (org-life-dashboard-action-weekly-review)
       (org-life-dashboard-action-roam-find))
     (should (eq capture-policy 'capture))
     (should (eq daily-policy 'agenda))
     (should (eq weekly-policy 'agenda))
     (should (eq roam-policy 'roam))
     (with-temp-buffer
       (org-life-dashboard-widget-quick-actions)
       (let ((rendered (buffer-string)))
         (should (string-match-p "Capture" rendered))
         (should (string-match-p "Daily Review" rendered))
         (should (string-match-p "Weekly Review" rendered))
         (should (string-match-p "Roam Find" rendered)))))))

(ert-deftest orglife-dashboard-manual-refresh-and-keypath-are-present ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (setq orglife-test-dashboard-reload-calls 0)
   (org-life-dashboard-refresh)
     (should (= orglife-test-dashboard-reload-calls 1))
     (should (orglife-test-map-call-contains "\"d\" . \"dashboard\""))
     (should (orglife-test-map-call-contains "\"r\" #'org-life-dashboard-reload"))))

(ert-deftest orglife-spc-o-coverage-helper-enforces-single-key-depth ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (cl-letf (((symbol-function 'where-is-internal)
              (lambda (command &optional _first _keymap)
                (cond
                 ((eq command #'my/org-capture-dwim) (list (kbd "SPC o c")))
                 ((eq command #'org-life-agenda-daily-review) (list (kbd "SPC o d")))
                 ((eq command #'org-life-agenda-weekly-review) (list (kbd "SPC o a R")))
                 ((eq command #'org-life-roam-node-find) (list (kbd "SPC o r")))
                 ((eq command #'org-life-journal-open-today) (list (kbd "SPC o j")))
                 ((eq command #'org-life-agenda-inbox-dashboard) (list (kbd "SPC o i")))
                 ((eq command #'org-life-dashboard-open) (list (kbd "SPC o o")))
                 ((eq command #'org-life-dashboard-refresh) (list (kbd "SPC o R")))
                 (t nil)))))
     (let* ((err (should-error (org-life-verify-spc-o-coverage) :type 'user-error))
            (message (error-message-string err)))
       (should (string-match-p "weekly-review" message))
       (should (string-match-p "SPC o a R" message))))))

(ert-deftest orglife-spc-o-coverage-helper-accepts-direct-bindings ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (cl-letf (((symbol-function 'where-is-internal)
              (lambda (command &optional _first _keymap)
                (cond
                 ((eq command #'my/org-capture-dwim) (list (kbd "SPC o c")))
                 ((eq command #'org-life-agenda-daily-review) (list (kbd "SPC o d") (kbd "SPC o a r")))
                 ((eq command #'org-life-agenda-weekly-review) (list (kbd "SPC o w") (kbd "SPC o a R")))
                 ((eq command #'org-life-roam-node-find) (list (kbd "SPC o r") (kbd "SPC o r f")))
                 ((eq command #'org-life-journal-open-today) (list (kbd "SPC o j") (kbd "SPC o j t")))
                 ((eq command #'org-life-agenda-inbox-dashboard) (list (kbd "SPC o i") (kbd "SPC o a i")))
                 ((eq command #'org-life-dashboard-open) (list (kbd "SPC o o") (kbd "SPC o d o")))
                 ((eq command #'org-life-dashboard-refresh) (list (kbd "SPC o R") (kbd "SPC o d r")))
                 (t nil)))))
     (let ((results (org-life-verify-spc-o-coverage)))
       (should (= (length results) 8))
       (dolist (row results)
         (should (plist-get row :direct-spc-o-keys)))))))

(ert-deftest orglife-integration-task-to-roam-persists-bidirectional-backlinks ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (let* ((source-file (expand-file-name "~/org/gtd/inbox.org"))
          (target-file (expand-file-name "~/org/roam/target.org"))
          source-id
          target-id
          backlinks)
     (orglife-test-write-file source-file "* TODO Link roam note\n")
     (orglife-test-write-file target-file "* Roam Target\n")
     (setq target-id (orglife-test-heading-id target-file))
     (with-current-buffer (find-file-noselect source-file)
       (goto-char (point-min))
       (org-back-to-heading t)
       (setq source-id (org-id-get-create))
       (cl-letf (((symbol-function 'org-life-integration--select-roam-node)
                  (lambda ()
                    (list :id target-id
                          :title "Roam Target"
                          :file target-file
                          :type "roam")))
                 ((symbol-function 'message)
                  (lambda (&rest _args) nil)))
         (save-window-excursion
           (org-life-link-task-to-roam)
           (org-life-link-task-to-roam)))
       (save-buffer)
       (should (string= (org-entry-get nil "ORGLIFE_LINK_KIND" t) "task-to-roam"))
       (should (string= (org-entry-get nil "ORGLIFE_LINK_TARGET_ID" t) target-id))
       (should (save-excursion
                 (goto-char (point-min))
                 (re-search-forward (regexp-quote (format "[[id:%s][Roam Target]]" target-id)) nil t))))
     (setq backlinks (org-life-integration-get-backlinks-for-target-id target-id))
     (should (= (length backlinks) 1))
     (should (equal (car backlinks)
                    (list :kind "task-to-roam"
                          :source-id source-id
                          :source-title "Link roam note"
                          :source-file source-file))))))

(ert-deftest orglife-integration-journal-to-heading-persists-bidirectional-backlinks ()
  (orglife-test-with-temp-home
   (orglife-test-reset-state)
   (orglife-test-install-stubs)
   (orglife-test-load "users/doom.d/config-org-integration.el")
   (let* ((journal-file (expand-file-name "~/org/journal/2026-03-08.org"))
          (target-file (expand-file-name "~/org/gtd/projects.org"))
          source-id
          target-id
          backlinks)
     (orglife-test-write-file journal-file "* TODO Journal follow-up\n")
     (orglife-test-write-file target-file "* TODO Project Alpha\n")
     (setq target-id (orglife-test-heading-id target-file))
     (with-current-buffer (find-file-noselect journal-file)
       (goto-char (point-min))
       (org-back-to-heading t)
       (setq source-id (org-id-get-create))
       (cl-letf (((symbol-function 'org-life-integration--select-gtd-heading)
                  (lambda ()
                    (list :marker (copy-marker 1)
                          :title "Project Alpha"
                          :file target-file
                          :type "gtd-heading"
                          :id target-id)))
                 ((symbol-function 'message)
                  (lambda (&rest _args) nil)))
         (save-window-excursion
           (org-life-link-journal-to-heading)))
       (save-buffer)
       (should (string= (org-entry-get nil "ORGLIFE_LINK_KIND" t) "journal-to-heading"))
       (should (string= (org-entry-get nil "ORGLIFE_LINK_TARGET_ID" t) target-id))
       (should (save-excursion
                 (goto-char (point-min))
                 (re-search-forward (regexp-quote (format "[[id:%s][Project Alpha]]" target-id)) nil t))))
     (setq backlinks (org-life-integration-get-backlinks-for-target-id target-id))
     (should (= (length backlinks) 1))
     (should (equal (car backlinks)
                    (list :kind "journal-to-heading"
                          :source-id source-id
                          :source-title "Journal follow-up"
                          :source-file journal-file))))))

(provide 'orglife-config-tests)

;;; orglife-config-tests.el ends here
