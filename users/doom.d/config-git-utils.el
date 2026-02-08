;;; config-git-utils.el --- Git utilities and branch browser -*- lexical-binding: t; -*-

;; ============================================================================
;; GIT UTILITY FUNCTIONS
;; ============================================================================
;; Helper functions for working with Git repos

(defun jr/copy-file-path ()
  "Copy relative file path from project root to kill ring."
  (interactive)
  (let ((project-root (projectile-project-root))
        root-str
        file-path)
    (if project-root (setq root-str project-root) (setq root-str ""))
    (setq file-path (string-remove-prefix root-str (buffer-file-name)))
    (message "%s" file-path)
    (kill-new file-path)))

(defun jr/magit-add-current-buffer-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is no current branch"))))

(defun jr/copy-branch-name ()
  "Copy the current Git branch name to the clipboard."
  (interactive)
  (require 'magit)
  (let ((branch-name (magit-get-current-branch)))
    (if branch-name
        (progn
          (kill-new branch-name)
          (message "%s" branch-name))
      (message "Not in a Git repository or not on a branch"))))

;; ============================================================================
;; GITHUB LINK FUNCTIONS
;; ============================================================================
;; Generate and copy GitHub URLs for files

(defun jr/github-file-link ()
  "Return a GitHub URL pointing to the current file on the current branch."
  (interactive)
  (let* ((remote-url
          (string-trim
           (shell-command-to-string
            "git config --get remote.origin.url")))
         ;; Handle different formats of remote URLs
         (https-url
          (cond
           ;; Case: SSH URL like git@github.com:user/repo.git
           ((string-match "\\`git@\\([^:]+\\):\\([^/]+\\)/\\([^.]+\\)\\(?:\\.git\\)?\\'" remote-url)
            (format "https://%s/%s/%s"
                    (match-string 1 remote-url)
                    (match-string 2 remote-url)
                    (match-string 3 remote-url)))
           ;; Case: HTTPS URL like https://github.com/user/repo.git
           ((string-match "\\`https://\\([^/]+\\)/\\([^/]+\\)/\\([^.]+\\)\\(?:\\.git\\)?\\'" remote-url)
            (format "https://%s/%s/%s"
                    (match-string 1 remote-url)
                    (match-string 2 remote-url)
                    (match-string 3 remote-url)))
           ;; Fallback case
           (t
            (message "Warning: Unrecognized remote URL format: %s" remote-url)
            remote-url)))
         (branch (magit-get-current-branch))
         (root
          (or (magit-toplevel default-directory)
              (vc-call-backend (vc-responsible-backend default-directory)
                               'root default-directory)))
         (relative-path (file-relative-name buffer-file-name root))
         (full-url (concat https-url "/blob/" branch "/" relative-path)))
    (kill-new full-url)
    (message "GitHub URL: %s" full-url)
    full-url))

(defun jr/github-file-line-link ()
  "Return a GitHub URL pointing to the current file and line at point."
  (interactive)
  (let* ((base (jr/github-file-link))
         (line (number-to-string (line-number-at-pos))))
    (let ((line-url (concat base "#L" line)))
      (kill-new line-url)
      (message "GitHub URL with line: %s" line-url)
      line-url)))

(defun jr/copy-github-file-link ()
  "Copy GitHub URL for current file to kill ring."
  (interactive)
  (jr/github-file-link))

(defun jr/copy-github-file-line-link ()
  "Copy GitHub URL for current file and line to kill ring."
  (interactive)
  (jr/github-file-line-link))

;; ============================================================================
;; GIT DIFF FUNCTIONS
;; ============================================================================

(defun jr/git-diff-file-at-point-vs-master ()
  "Show diff of file at point against origin/master."
  (interactive)
  (let ((file (or (buffer-file-name)
                  (when (eq major-mode 'dired-mode)
                    (dired-get-filename nil t)))))
    (if file
        (magit-diff-range "origin/master" nil (list file))
      (user-error "No file at point"))))

;; ============================================================================
;; COMMIT HELPERS
;; ============================================================================

(defun jr/get-last-commit ()
  "Get the last commit hash on the current branch using Magit."
  (interactive)
  (let ((commit-hash (magit-git-string "rev-parse" "HEAD")))
    (message "%s" commit-hash)
    commit-hash))

(defun jr/magit-get-last-commit-for-branch (branch)
  "Get the last commit hash for a given BRANCH using Magit."
  (interactive (list (magit-read-branch "Branch")))
  (let ((commit-hash (magit-git-string "rev-parse" branch)))
    (message "%s" commit-hash)
    commit-hash))

(defun jr/dired-untracked (dir)
  "Open dired buffer with untracked files in git repository."
  (interactive "DUntracked in directory: ")
  (cd (projectile-project-root))
  (switch-to-buffer (get-buffer-create "*untracked*"))
  (shell-command "git ls-files --others --exclude-standard | xargs ls -l" (current-buffer))
  (dired-mode dir)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))

;; ============================================================================
;; GIT BRANCH FILE BROWSER
;; ============================================================================
;; Browse and checkout files from another git branch

(defvar-local jr/git-branch-files--source-branch nil
  "The source branch being browsed.")

(defvar-local jr/git-branch-files--marked-files nil
  "List of marked file paths.")

(defvar-local jr/git-branch-files--all-files nil
  "All files in the branch (cached).")

;; Progressive loading state variables
(defvar-local jr/git-branch-files--loading nil
  "Non-nil if loading is in progress.")

(defvar-local jr/git-branch-files--loading-index 0
  "Current index in file list being processed.")

(defvar-local jr/git-branch-files--status-cache nil
  "Hash table mapping file paths to their status.")

(defvar-local jr/git-branch-files--modified-files nil
  "Hash table of files that differ from branch (for O(1) lookup).")

(defvar-local jr/git-branch-files--uncommitted-files nil
  "Hash table of files with uncommitted local changes.")

(defvar-local jr/git-branch-files--spinner-index 0
  "Current spinner animation frame.")

(defvar-local jr/git-branch-files--spinner-timer nil
  "Timer for spinner animation.")

(defvar-local jr/git-branch-files--load-timer nil
  "Timer for progressive loading.")

(defconst jr/git-branch-files--spinner-frames
  ["⣾" "⣽" "⣻" "⢿" "⡿" "⣟" "⣯" "⣷"]
  "Spinner animation frames using braille pattern.")

(defface jr/git-branch-files-marked-face
  '((t :inherit font-lock-warning-face :weight bold))
  "Face for marked files in git branch files buffer.")

(defface jr/git-branch-files-new-face
  '((t :inherit font-lock-string-face))
  "Face for files that don't exist locally.")

(defface jr/git-branch-files-modified-face
  '((t :inherit font-lock-keyword-face))
  "Face for files that differ from local version.")

(defface jr/git-branch-files-same-face
  '((t :inherit font-lock-comment-face))
  "Face for files that are same as local version.")

(defface jr/git-branch-files-loading-face
  '((t :inherit font-lock-comment-face :slant italic))
  "Face for files whose status is still loading.")

(defvar jr/git-branch-files-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'jr/git-branch-files-checkout-file)
    (define-key map (kbd "?") #'jr/git-branch-files-help)
    map)
  "Keymap for `jr/git-branch-files-mode'.")

(defun jr/git-branch-files--cleanup ()
  "Clean up timers when buffer is killed."
  (when jr/git-branch-files--spinner-timer
    (cancel-timer jr/git-branch-files--spinner-timer))
  (when jr/git-branch-files--load-timer
    (cancel-timer jr/git-branch-files--load-timer)))

(define-derived-mode jr/git-branch-files-mode tabulated-list-mode "GitBranchFiles"
  "Major mode for browsing and checking out files from another git branch.
\\{jr/git-branch-files-mode-map}"
  (setq tabulated-list-format [("Mark" 4 t)
                               ("File" 70 t)
                               ("Status" 10 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key '("File" . nil))
  (tabulated-list-init-header)
  (add-hook 'kill-buffer-hook #'jr/git-branch-files--cleanup nil t))

;; Evil keybindings - must override Evil's default bindings for m, u, d, etc.
(after! evil
  (evil-define-key 'normal jr/git-branch-files-mode-map
    (kbd "m") #'jr/git-branch-files-mark
    (kbd "u") #'jr/git-branch-files-unmark
    (kbd "U") #'jr/git-branch-files-unmark-all
    (kbd "t") #'jr/git-branch-files-toggle-mark
    (kbd "x") #'jr/git-branch-files-checkout-marked
    (kbd "p") #'jr/git-branch-files-preview-file
    (kbd "d") #'jr/git-branch-files-diff-file
    (kbd "gr") #'jr/git-branch-files-refresh
    (kbd "q") #'quit-window
    (kbd "s") #'jr/git-branch-files-filter
    (kbd "S") #'jr/git-branch-files-clear-filter
    (kbd "o") #'jr/git-branch-files-sort-by-status
    (kbd "RET") #'jr/git-branch-files-checkout-file))

(defun jr/git-branch-files (&optional branch)
  "Browse and checkout files from another git BRANCH.
If BRANCH is not provided, prompt for selection.
Shows buffer immediately and loads status progressively."
  (interactive)
  (require 'magit)
  (let ((git-root (magit-toplevel)))
    (unless git-root
      (user-error "Not in a git repository"))
    (let* ((branch (or branch (magit-read-branch "Browse files from branch")))
           (buffer-name (format "*git-branch-files: %s*" branch)))
      (with-current-buffer (get-buffer-create buffer-name)
        (jr/git-branch-files-mode)
        (setq default-directory git-root)
        (setq jr/git-branch-files--source-branch branch)
        (setq jr/git-branch-files--marked-files nil)
        (setq jr/git-branch-files--all-files nil)
        (setq jr/git-branch-files--status-cache (make-hash-table :test 'equal))
        (setq jr/git-branch-files--modified-files (make-hash-table :test 'equal))
        (setq jr/git-branch-files--uncommitted-files (make-hash-table :test 'equal))
        (setq jr/git-branch-files--loading t)
        (setq jr/git-branch-files--loading-index 0)
        (setq jr/git-branch-files--spinner-index 0)
        ;; Show buffer immediately with loading state
        (switch-to-buffer (current-buffer))
        (jr/git-branch-files--update-header-loading 0 "?")
        ;; Force redisplay so user sees the loading state immediately
        (redisplay t)
        ;; Start loading (uses timer to allow redisplay during git operations)
        (run-with-timer 0.01 nil #'jr/git-branch-files--start-loading-async
                        (current-buffer) branch)))))

(defun jr/git-branch-files--get-file-list (branch)
  "Get list of files in BRANCH using git ls-tree."
  (magit-git-lines "ls-tree" "-r" "--name-only" branch))

(defun jr/git-branch-files--get-modified-files (branch)
  "Get hash table of files that differ between HEAD and BRANCH."
  (let ((table (make-hash-table :test 'equal))
        (files (magit-git-lines "diff" "--name-only" "HEAD" branch)))
    (dolist (file files)
      (puthash file t table))
    table))

(defun jr/git-branch-files--get-uncommitted-files ()
  "Get hash table of files with uncommitted local changes."
  (let ((table (make-hash-table :test 'equal))
        (files (magit-git-lines "diff" "--name-only" "HEAD")))
    (dolist (file files)
      (puthash file t table))
    table))

(defun jr/git-branch-files--spinner-char ()
  "Return current spinner character."
  (aref jr/git-branch-files--spinner-frames
        (mod jr/git-branch-files--spinner-index
             (length jr/git-branch-files--spinner-frames))))

(defun jr/git-branch-files--update-header-loading (current total)
  "Update header line with loading progress."
  (setq header-line-format
        (format " Git Branch: %s | Loading %s %d/%s files..."
                jr/git-branch-files--source-branch
                (jr/git-branch-files--spinner-char)
                current
                (if (stringp total) total (number-to-string total)))))

(defun jr/git-branch-files--start-loading-async (buffer branch)
  "Start progressive loading of files from BRANCH in BUFFER.
Called via timer to allow initial redisplay."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (jr/git-branch-files--start-loading branch))))

(defun jr/git-branch-files--start-loading (branch)
  "Start progressive loading of files from BRANCH."
  (let ((buffer (current-buffer)))
    ;; Start spinner timer first so user sees activity
    (setq jr/git-branch-files--spinner-timer
          (run-with-timer 0.1 0.1
                          (lambda ()
                            (when (buffer-live-p buffer)
                              (with-current-buffer buffer
                                (when jr/git-branch-files--loading
                                  (setq jr/git-branch-files--spinner-index
                                        (1+ jr/git-branch-files--spinner-index))
                                  (jr/git-branch-files--update-header-loading
                                   jr/git-branch-files--loading-index
                                   (if jr/git-branch-files--all-files
                                       (length jr/git-branch-files--all-files)
                                     "?"))
                                  (force-mode-line-update)))))))
    ;; Get file list
    (setq jr/git-branch-files--all-files (jr/git-branch-files--get-file-list branch))
    (jr/git-branch-files--update-header-loading 0 (length jr/git-branch-files--all-files))
    (force-mode-line-update)
    (redisplay t)
    ;; Get modified files in batch
    (setq jr/git-branch-files--modified-files (jr/git-branch-files--get-modified-files branch))
    ;; Get uncommitted files in batch
    (setq jr/git-branch-files--uncommitted-files (jr/git-branch-files--get-uncommitted-files))
    ;; Show files with "..." status
    (jr/git-branch-files--refresh-loading)
    (redisplay t)
    ;; Start loading timer for batch status computation
    (setq jr/git-branch-files--load-timer
          (run-with-timer 0.01 nil
                          #'jr/git-branch-files--load-next-batch buffer))))

(defun jr/git-branch-files--load-next-batch (buffer)
  "Load status for next batch of files in BUFFER."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (when jr/git-branch-files--loading
        (let* ((batch-size 25)  ; Smaller batches for more visible progress
               (files jr/git-branch-files--all-files)
               (total (length files))
               (index jr/git-branch-files--loading-index))
          (if (>= index total)
              ;; Done loading
              (jr/git-branch-files--finish-loading)
            ;; Process batch
            (let ((end (min (+ index batch-size) total)))
              (while (< index end)
                (let ((file (nth index files)))
                  (puthash file (jr/git-branch-files--compute-status file)
                           jr/git-branch-files--status-cache))
                (setq index (1+ index)))
              (setq jr/git-branch-files--loading-index index)
              ;; Update display
              (jr/git-branch-files--refresh-loading)
              (force-mode-line-update)
              ;; Schedule next batch with slight delay for visible progress
              (setq jr/git-branch-files--load-timer
                    (run-with-timer 0.02 nil
                                    #'jr/git-branch-files--load-next-batch buffer)))))))))

(defun jr/git-branch-files--compute-status (file)
  "Compute status for FILE using cached modified/uncommitted sets."
  (cond
   ((not (file-exists-p file)) "new")
   ((gethash file jr/git-branch-files--uncommitted-files) "uncommitted")
   ((gethash file jr/git-branch-files--modified-files) "modified")
   (t "same")))

(defun jr/git-branch-files--finish-loading ()
  "Finish loading and clean up timers."
  (setq jr/git-branch-files--loading nil)
  (when jr/git-branch-files--spinner-timer
    (cancel-timer jr/git-branch-files--spinner-timer)
    (setq jr/git-branch-files--spinner-timer nil))
  (when jr/git-branch-files--load-timer
    (cancel-timer jr/git-branch-files--load-timer)
    (setq jr/git-branch-files--load-timer nil))
  (jr/git-branch-files--refresh)
  (message "Loaded %d files. Press '?' for help, 'm' to mark, 'x' to checkout marked"
           (length jr/git-branch-files--all-files)))

(defun jr/git-branch-files--file-status (file)
  "Return status of FILE from cache, or compute if not cached."
  (or (gethash file jr/git-branch-files--status-cache)
      (if jr/git-branch-files--loading
          "..."
        (jr/git-branch-files--compute-status file))))

(defun jr/git-branch-files--make-entry (file)
  "Create a tabulated-list entry for FILE."
  (let* ((marked (member file jr/git-branch-files--marked-files))
         (status (jr/git-branch-files--file-status file))
         (mark-str (if marked "*" " "))
         (status-face (pcase status
                        ("new" 'jr/git-branch-files-new-face)
                        ("modified" 'jr/git-branch-files-modified-face)
                        ("uncommitted" 'font-lock-warning-face)
                        ("..." 'jr/git-branch-files-loading-face)
                        (_ 'jr/git-branch-files-same-face))))
    (list file
          (vector (propertize mark-str 'face (if marked 'jr/git-branch-files-marked-face 'default))
                  file
                  (propertize status 'face status-face)))))

(defun jr/git-branch-files--refresh ()
  "Refresh the file list display."
  (interactive)
  (let ((pos (point)))
    (setq tabulated-list-entries
          (mapcar #'jr/git-branch-files--make-entry jr/git-branch-files--all-files))
    (tabulated-list-print t)
    (setq header-line-format
          (format " Git Branch: %s | %d files | %d marked"
                  jr/git-branch-files--source-branch
                  (length jr/git-branch-files--all-files)
                  (length jr/git-branch-files--marked-files)))
    (goto-char (min pos (point-max)))))

(defun jr/git-branch-files--refresh-loading ()
  "Refresh display during loading, preserving cursor position."
  (let ((pos (point)))
    (setq tabulated-list-entries
          (mapcar #'jr/git-branch-files--make-entry jr/git-branch-files--all-files))
    (tabulated-list-print t)
    (jr/git-branch-files--update-header-loading
     jr/git-branch-files--loading-index
     (length jr/git-branch-files--all-files))
    (goto-char (min pos (point-max)))))

(defun jr/git-branch-files-refresh ()
  "Refresh the file list (re-read from git)."
  (interactive)
  ;; Cancel any running timers
  (when jr/git-branch-files--spinner-timer
    (cancel-timer jr/git-branch-files--spinner-timer)
    (setq jr/git-branch-files--spinner-timer nil))
  (when jr/git-branch-files--load-timer
    (cancel-timer jr/git-branch-files--load-timer)
    (setq jr/git-branch-files--load-timer nil))
  ;; Reset and reload
  (setq jr/git-branch-files--status-cache (make-hash-table :test 'equal))
  (setq jr/git-branch-files--loading t)
  (setq jr/git-branch-files--loading-index 0)
  (jr/git-branch-files--start-loading jr/git-branch-files--source-branch)
  (message "Refreshing file list..."))

(defun jr/git-branch-files--current-file ()
  "Get the file path at point."
  (tabulated-list-get-id))

(defun jr/git-branch-files-mark ()
  "Mark the file at point."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (unless (member file jr/git-branch-files--marked-files)
      (push file jr/git-branch-files--marked-files))
    (jr/git-branch-files--refresh)
    (forward-line 1)))

(defun jr/git-branch-files-unmark ()
  "Unmark the file at point."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (setq jr/git-branch-files--marked-files
          (delete file jr/git-branch-files--marked-files))
    (jr/git-branch-files--refresh)
    (forward-line 1)))

(defun jr/git-branch-files-unmark-all ()
  "Unmark all files."
  (interactive)
  (setq jr/git-branch-files--marked-files nil)
  (jr/git-branch-files--refresh)
  (message "All marks cleared"))

(defun jr/git-branch-files-toggle-mark ()
  "Toggle mark on file at point."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (if (member file jr/git-branch-files--marked-files)
        (setq jr/git-branch-files--marked-files
              (delete file jr/git-branch-files--marked-files))
      (push file jr/git-branch-files--marked-files))
    (jr/git-branch-files--refresh)
    (forward-line 1)))

(defun jr/git-branch-files--check-uncommitted (files)
  "Return list of FILES that have uncommitted local changes."
  (seq-filter
   (lambda (file)
     (and (file-exists-p file)
          (not (string-empty-p
                (shell-command-to-string
                 (format "git diff --name-only -- %s 2>/dev/null"
                         (shell-quote-argument file)))))))
   files))

(defun jr/git-branch-files-checkout-file ()
  "Checkout the file at point from the source branch."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (let ((uncommitted (jr/git-branch-files--check-uncommitted (list file))))
      (when (and uncommitted
                 (not (yes-or-no-p (format "File '%s' has uncommitted changes. Overwrite? " file))))
        (user-error "Checkout cancelled"))
      (if (= 0 (shell-command
                (format "git checkout %s -- %s"
                        (shell-quote-argument jr/git-branch-files--source-branch)
                        (shell-quote-argument file))))
          (progn
            (message "Checked out '%s' from %s" file jr/git-branch-files--source-branch)
            (jr/git-branch-files--refresh)
            (when (fboundp 'magit-refresh-all)
              (magit-refresh-all)))
        (user-error "Checkout failed for '%s'" file)))))

(defun jr/git-branch-files-checkout-marked ()
  "Checkout all marked files from the source branch."
  (interactive)
  (unless jr/git-branch-files--marked-files
    (user-error "No files marked. Use 'm' to mark files"))
  (let* ((files jr/git-branch-files--marked-files)
         (uncommitted (jr/git-branch-files--check-uncommitted files)))
    (when uncommitted
      (unless (yes-or-no-p
               (format "These files have uncommitted changes: %s. Overwrite? "
                       (string-join uncommitted ", ")))
        (user-error "Checkout cancelled")))
    (let ((cmd (format "git checkout %s -- %s"
                       (shell-quote-argument jr/git-branch-files--source-branch)
                       (mapconcat #'shell-quote-argument files " "))))
      (if (= 0 (shell-command cmd))
          (progn
            (message "Checked out %d files from %s"
                     (length files)
                     jr/git-branch-files--source-branch)
            (setq jr/git-branch-files--marked-files nil)
            (jr/git-branch-files--refresh)
            (when (fboundp 'magit-refresh-all)
              (magit-refresh-all)))
        (user-error "Checkout failed. Check *Messages* for details")))))

(defun jr/git-branch-files-preview-file ()
  "Preview the content of file at point from the source branch."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (let* ((branch jr/git-branch-files--source-branch)
           (buffer-name (format "*preview: %s:%s*" branch (file-name-nondirectory file)))
           (content (shell-command-to-string
                     (format "git show %s:%s 2>/dev/null"
                             (shell-quote-argument branch)
                             (shell-quote-argument file)))))
      (with-current-buffer (get-buffer-create buffer-name)
        (erase-buffer)
        (insert content)
        ;; Try to set appropriate mode based on file extension
        (let ((buffer-file-name file))
          (ignore-errors (set-auto-mode)))
        (goto-char (point-min))
        (setq buffer-read-only t)
        (display-buffer (current-buffer))))))

(defun jr/git-branch-files-diff-file ()
  "Show diff between source branch version and local version of file at point."
  (interactive)
  (when-let ((file (jr/git-branch-files--current-file)))
    (let* ((branch jr/git-branch-files--source-branch)
           (buffer-name (format "*diff: %s vs %s*" branch file)))
      (if (not (file-exists-p file))
          (message "File doesn't exist locally - use 'p' to preview branch version")
        (let ((diff-output (shell-command-to-string
                           (format "git diff HEAD:%s %s:%s 2>/dev/null"
                                   (shell-quote-argument file)
                                   (shell-quote-argument branch)
                                   (shell-quote-argument file)))))
          (if (string-empty-p diff-output)
              (message "No differences between local and branch version")
            (with-current-buffer (get-buffer-create buffer-name)
              (erase-buffer)
              (insert diff-output)
              (diff-mode)
              (goto-char (point-min))
              (setq buffer-read-only t)
              (display-buffer (current-buffer)))))))))

(defun jr/git-branch-files-filter (pattern)
  "Filter displayed files to those matching PATTERN.
Empty pattern clears the filter."
  (interactive "sFilter pattern (regexp, empty to clear): ")
  (if (string-empty-p pattern)
      (progn
        (setq jr/git-branch-files--all-files
              (jr/git-branch-files--get-file-list jr/git-branch-files--source-branch))
        (jr/git-branch-files--refresh)
        (message "Filter cleared. Showing all %d files" (length jr/git-branch-files--all-files)))
    (setq jr/git-branch-files--all-files
          (seq-filter (lambda (f) (string-match-p pattern f))
                      (jr/git-branch-files--get-file-list jr/git-branch-files--source-branch)))
    (jr/git-branch-files--refresh)
    (message "Showing %d files matching '%s'" (length jr/git-branch-files--all-files) pattern)))

(defun jr/git-branch-files-clear-filter ()
  "Clear the current filter and show all files."
  (interactive)
  (setq jr/git-branch-files--all-files
        (jr/git-branch-files--get-file-list jr/git-branch-files--source-branch))
  (jr/git-branch-files--refresh)
  (message "Filter cleared. Showing all %d files" (length jr/git-branch-files--all-files)))

(defvar-local jr/git-branch-files--sort-by-status nil
  "If non-nil, sort files by status instead of name.")

(defun jr/git-branch-files--status-priority (status)
  "Return sort priority for STATUS (lower = first)."
  (pcase status
    ("new" 0)
    ("modified" 1)
    ("uncommitted" 2)
    ("..." 3)
    ("same" 4)
    (_ 5)))

(defun jr/git-branch-files-sort-by-status ()
  "Toggle sorting files by status (new/modified first) vs by name."
  (interactive)
  (setq jr/git-branch-files--sort-by-status
        (not jr/git-branch-files--sort-by-status))
  (if jr/git-branch-files--sort-by-status
      (progn
        ;; Disable tabulated-list auto-sorting so our custom order is preserved
        (setq tabulated-list-sort-key nil)
        ;; Sort by status priority, then by name within same status
        (setq jr/git-branch-files--all-files
              (sort (copy-sequence jr/git-branch-files--all-files)
                    (lambda (a b)
                      (let ((sa (jr/git-branch-files--status-priority
                                 (jr/git-branch-files--file-status a)))
                            (sb (jr/git-branch-files--status-priority
                                 (jr/git-branch-files--file-status b))))
                        (if (= sa sb)
                            (string< a b)
                          (< sa sb))))))
        (jr/git-branch-files--refresh)
        (message "Sorted by status (new → modified → uncommitted → same)"))
    ;; Re-enable tabulated-list sorting by File column
    (setq tabulated-list-sort-key '("File" . nil))
    ;; Sort alphabetically by name
    (setq jr/git-branch-files--all-files
          (sort (copy-sequence jr/git-branch-files--all-files) #'string<))
    (jr/git-branch-files--refresh)
    (message "Sorted by file name")))

(defun jr/git-branch-files-help ()
  "Show help for git branch files mode."
  (interactive)
  (message "Keys: RET=checkout, m=mark, u=unmark, U=unmark-all, t=toggle, x=checkout-marked, p=preview, d=diff, s=filter, S=clear, o=sort, gr=refresh, /=search, q=quit"))

;; ============================================================================
;; KEYBINDINGS
;; ============================================================================

;; GitHub link keybindings
(map! :leader
      (:prefix ("g" . "git")
       :desc "Copy GitHub file link" "y" #'jr/copy-github-file-link
       :desc "Copy GitHub file+line link" "Y" #'jr/copy-github-file-line-link
       (:desc "file" :prefix "f"
        :desc "Browse files in branch" "b" #'jr/git-branch-files)))

(provide 'config-git-utils)
;;; config-git-utils.el ends here
