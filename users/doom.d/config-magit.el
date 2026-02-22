;;; config-magit.el --- Magit configuration and optimizations -*- lexical-binding: t; -*-

;; ============================================================================
;; MAGIT CONFIGURATION
;; ============================================================================
;; Performance optimizations and customizations for Magit

(after! magit
  (evil-collection-init 'magit)
  (setq git-commit-summary-max-length 72)

  ;; ============================================================================
  ;; MAGIT PERFORMANCE OPTIMIZATIONS
  ;; ============================================================================
  ;; These optimizations improve Magit performance for large repositories

  ;; Core performance settings
  (setq magit-refresh-status-buffer nil          ; Only refresh status buffer when current
        magit-refresh-verbose nil                 ; Disable verbose refresh by default
        magit-auto-revert-mode-lighter ""        ; Hide mode lighter
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Disable Magit's own long line warnings and optimizations
  (setq magit-show-long-lines-warning nil       ; Disable the long lines warning message
        magit-long-line-threshold 100000        ; Increase threshold to 100k chars (very high)
        magit-process-popup-time -1)            ; Don't auto-show process buffers

  ;; Set git executable path for better performance (especially on macOS)
  (when-let ((git-path (executable-find "git")))
    (setq magit-git-executable git-path))

  ;; Remove slow status sections (uncomment as needed for large repos)
  ;; These sections can be very slow in large repositories
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)      ; Usually the slowest
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files) ; For many untracked files
  ;; (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  ;; (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)             ; Remove tags from refs buffer)

  ;; Diff performance optimizations
  ;; Disable expensive diff highlighting features for better performance
  (setq magit-diff-highlight-indentation nil        ; Disable indentation highlighting
        magit-diff-highlight-trailing nil           ; Disable trailing whitespace highlighting
        magit-diff-paint-whitespace nil             ; Disable whitespace painting
        magit-diff-highlight-hunk-body nil          ; Disable hunk body highlighting
        magit-diff-refine-hunk nil)                 ; Disable hunk refinement (word-level diffs)

  ;; Log and graph performance optimizations
  ;; Disable colored and graphical log output which can be very slow
  (setq magit-log-arguments '("--graph" "--color" "--decorate" "-n256")  ; Limit initial commits
        magit-log-select-arguments '("-n2048" "--decorate")              ; Limit log selection
        magit-log-show-refname-after-summary t)                         ; Show refnames efficiently

  ;; Disable slow graph coloring for large histories
  (remove-hook 'magit-log-mode-hook 'magit-show-commit-buffer-in-revision-buffer)

  ;; Performance monitoring and debugging helpers
  ;; Functions to help identify performance bottlenecks
  (defun magit-toggle-performance-mode ()
    "Toggle Magit performance optimizations on/off."
    (interactive)
    (if magit-refresh-verbose
        (progn
          (setq magit-refresh-verbose nil)
          (message "Magit verbose refresh disabled"))
      (progn
        (setq magit-refresh-verbose t)
        (message "Magit verbose refresh enabled - check *Messages* for timing info"))))

  ;; Keybinding for performance toggle
  (define-key magit-mode-map (kbd "C-c C-p") #'magit-toggle-performance-mode)

  ;; Repository size detection for conditional optimizations
  (defun magit-repo-is-large-p ()
    "Check if current repository is large (>1000 files in status)."
    (when (magit-toplevel)
      (let ((file-count (length (magit-untracked-files))))
        (> file-count 1000))))

  ;; Conditional optimization based on repo size
  (when (magit-repo-is-large-p)
    (remove-hook 'magit-status-sections-hook 'magit-insert-untracked-files)
    (message "Large repository detected: disabled untracked files section"))

  ;; ============================================================================
  ;; LARGE FILE AND LONG LINE OPTIMIZATIONS
  ;; ============================================================================
  ;; Address "Enabling long lines shortcuts" messages and large file performance

  ;; Configure so-long-mode for Magit buffers
  (with-eval-after-load 'so-long
    ;; Prevent so-long from interfering with Magit buffers
    (add-to-list 'so-long-minor-modes 'magit-diff-mode)
    (add-to-list 'so-long-minor-modes 'magit-status-mode)
    (add-to-list 'so-long-minor-modes 'magit-log-mode)

    ;; Increase thresholds before so-long kicks in
    (setq so-long-threshold 2000              ; Increase line length threshold
          so-long-max-lines 500               ; Check fewer lines initially
          so-long-skip-leading-comments t))   ; Skip comment-only lines

  ;; Large file handling in diffs
  (setq magit-diff-buffer-file-p nil          ; Don't show full file content in diff buffers
        magit-revision-insert-related-refs nil ; Don't show related refs for performance
        large-file-warning-threshold 50000000) ; 50MB threshold for warnings

  ;; Optimize diff display for large files
  (defun magit-diff-visit-file-with-large-file-check ()
    "Visit file but check for large files first."
    (interactive)
    (let ((file (magit-file-at-point)))
      (when file
        (if (and (file-exists-p file)
                 (> (file-attribute-size (file-attributes file)) 10000000)) ; 10MB
            (when (y-or-n-p (format "File %s is large (>10MB). Open anyway? "
                                    (file-name-nondirectory file)))
              (find-file file))
          (magit-diff-visit-file-worktree file)))))

  ;; Diff context and hunk size limits for performance
  (setq magit-diff-context-lines 3             ; Reduce context lines shown
        magit-diff-hunk-region-functions        ; Optimize hunk processing
        '(magit-diff-hunk-region-dim-outside
          magit-diff-hunk-region-highlight-inside))

  ;; Suppress long line warnings in Magit buffers specifically
  (defun magit-suppress-long-lines-warning ()
    "Suppress long lines warnings in Magit buffers."
    (when (derived-mode-p 'magit-mode)
      (setq-local so-long-function 'so-long-mode)
      (setq-local so-long-threshold 5000)      ; Higher threshold for Magit
      (setq-local warning-suppress-types
                  (append warning-suppress-types '((so-long))))))

  (add-hook 'magit-mode-hook #'magit-suppress-long-lines-warning)

  ;; Additional file size and diff limits
  (setq magit-diff-max-stat-graph-width 50    ; Limit stat graph width
        magit-log-margin-show-committer-date t ; Show committer date efficiently
        magit-revision-show-gravatars nil)     ; Disable gravatars for performance

  ;; Skip large binary files in diffs entirely
  (define-advice magit-insert-diff (:around (orig-fn &rest args) skip-large-files)
    "Skip showing diffs for very large files."
    (let ((file (magit-file-at-point)))
      (if (and file
               (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 5000000)) ; 5MB limit
          (insert (propertize (format "Diff skipped for large file: %s (>5MB)\n"
                                      (file-name-nondirectory file))
                              'face 'magit-diff-file-heading))
        (apply orig-fn args))))

  ;; Optimize buffer display and refresh behavior
  (setq magit-display-buffer-noselect t        ; Don't auto-select Magit windows
        magit-bury-buffer-function 'quit-window ; Quick buffer disposal
        magit-save-repository-buffers 'dontask) ; Auto-save without asking

  ;; Custom advice to prevent long line processing in status buffer
  (defun magit-status-around-advice (orig-fun &rest args)
    "Advice around magit-status to optimize for large files."
    (let ((so-long-threshold 10000)            ; Much higher threshold
          (inhibit-field-text-motion t)        ; Faster text navigation
          (jit-lock-defer-time 0.1))          ; Faster syntax highlighting
      (apply orig-fun args)))

  (advice-add 'magit-status :around #'magit-status-around-advice))

;; ============================================================================
;; FORGE CONFIGURATION
;; ============================================================================

(after! forge
  ;; Default to creating draft pull requests
  ;; Must use advice because forge--setup-post-buffer sets draft-p AFTER mode hooks run
  (defun jr/forge-default-to-draft (&rest _)
    "Default new pull requests to draft status."
    (when (and buffer-file-name
               (string-match "new-pullreq$" buffer-file-name))
      (setq forge--buffer-draft-p t)))
  (advice-add 'forge--setup-post-buffer :after #'jr/forge-default-to-draft)

  (setq forge-topic-list-limit '(60 . 0))     ; show more PRs in list buffers
  (setq forge-owned-queries
        '(("Waiting For My Review" . "is:open review-requested:@me")
          ("Nightshade"            . "is:open label:nightshade-pod"))))

;; Fix forge-post-mode keybindings being shadowed by markdown-mode
(map! :map forge-post-mode-map
      :desc "Submit"     "C-c C-c" #'forge-post-submit
      :desc "Cancel"     "C-c C-k" #'forge-post-cancel
      :desc "Forge Menu" "C-c C-e" #'forge-post-menu)

;; ============================================================================
;; MAGIT TODOS CONFIGURATION
;; ============================================================================

(setq magit-todos-exclude-globs '(
                                  "*brindle*"
                                  "*node_modules*"
                                  ))

;; ============================================================================
;; GITHUB REVIEW CONFIGURATION
;; ============================================================================

(setq github-review-fetch-top-level-and-review-comments t)
(setq code-review-auth-login-marker 'forge)

(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

(provide 'config-magit)
;;; config-magit.el ends here
