;;; config-copilot.el --- Copilot AI completion configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; COPILOT CONFIGURATION
;; ============================================================================
;; GitHub Copilot and AI-assisted coding

;; ----------------------------------------------------------------------------
;; Helper Functions
;; ----------------------------------------------------------------------------

(defun jr/set-copilot-node-executable ()
  "Set copilot node executable from current environment."
  (interactive)
  (let ((node-path (executable-find "node")))
    (if node-path
        (progn
          (message "Setting copilot node executable to %s" node-path)
          (setq copilot-node-executable node-path))
      (message "Warning: Node.js not found in PATH. Cannot set copilot node executable."))))

(defun jr/copilot-maybe-enable ()
  "Enable copilot-mode if node is available in the current environment."
  (when (and (executable-find "node")
             (fboundp 'copilot-mode))
    (setq copilot-node-executable (executable-find "node"))
    (copilot-mode 1)
    (message "Copilot enabled with node: %s" copilot-node-executable)))

;; ----------------------------------------------------------------------------
;; Copilot Package Configuration
;; ----------------------------------------------------------------------------

(use-package! copilot
  :hook ((prog-mode . jr/copilot-maybe-enable)
         (yaml-mode . jr/copilot-maybe-enable)
         (yaml-ts-mode . jr/copilot-maybe-enable)
         (json-mode . jr/copilot-maybe-enable)
         (json-ts-mode . jr/copilot-maybe-enable)
         (xml-mode . jr/copilot-maybe-enable))
  :bind (:map copilot-completion-map
              ;; ("M-C-<return>" . 'copilot-accept-completion)
              ;; ("M-C-<tab>" . 'copilot-accept-completion)
              ;; ("C-TAB" . 'copilot-accept-completion)
              ("C-y" . 'copilot-accept-completion)
              ;; ("C-M-TAB" . 'copilot-accept-completion-by-word)
              ;; ("C-M-<tab>" . 'copilot-accept-completion-by-word)
              )
  :config
  (setq copilot-indent-offset-warning-disable nil)

  ;; Check node availability when package loads
  (if (executable-find "node")
      (message "Node executable found at load time: %s" (executable-find "node"))
    (message "Warning: Node.js not found at load time. Copilot will activate if node becomes available.")))

;; ----------------------------------------------------------------------------
;; Copilot Tab Function
;; ----------------------------------------------------------------------------

(defun jr/copilot-tab ()
  "Tab command that will complete with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (if (and (fboundp 'copilot-accept-completion)
           (executable-find "node"))
      (copilot-accept-completion)
    (message "Copilot not available (Node.js might be missing)")))

;; ----------------------------------------------------------------------------
;; Nix Shell Integration
;; ----------------------------------------------------------------------------

(defun jr/nix-post-activate-check ()
  "Check for node availability after entering nix-shell and enable copilot if appropriate."
  (jr/set-copilot-node-executable)
  ;; If we're in a prog-mode buffer and copilot isn't active, try to enable it
  (when (and (derived-mode-p 'prog-mode 'yaml-mode 'yaml-ts-mode 'json-mode 'json-ts-mode 'xml-mode)
             (not (bound-and-true-p copilot-mode))
             (executable-find "node"))
    (jr/copilot-maybe-enable)))

(add-hook 'nix-shell-post-activate-hooks 'jr/nix-post-activate-check)

;; ----------------------------------------------------------------------------
;; gptel AI Configuration
;; ----------------------------------------------------------------------------

(use-package! gptel
  :config
  ;; (setq! gptel-api-key "your key")
  (gptel-make-gh-copilot "Copilot"))

;; ----------------------------------------------------------------------------
;; AI Commit Message Generation
;; ----------------------------------------------------------------------------

(defun jr/ai-generate-commit-message ()
  "Generate a commit message using AI based on the current git diff."
  (interactive)
  (let* ((diff-output (shell-command-to-string "git diff --staged"))
         (prompt (concat "Generate a concise commit message for these changes. Focus on what was changed and why. Keep it under 72 characters for the first line:\n\n" diff-output)))
    (if (string-empty-p (string-trim diff-output))
        (message "No staged changes found. Stage some changes first.")
      (gptel-request prompt
        :callback (lambda (response info)
                    (if response
                        (let ((commit-msg (string-trim response)))
                          (with-current-buffer (get-buffer-create "*AI Commit Message*")
                            (erase-buffer)
                            (insert commit-msg)
                            (display-buffer (current-buffer)))
                          (message "AI commit message generated! Check *AI Commit Message* buffer."))
                      (message "Failed to generate commit message: %s" info)))))))

(defun jr/ai-insert-commit-message ()
  "Insert AI-generated commit message into current buffer."
  (interactive)
  (let* ((diff-output (shell-command-to-string "git diff --staged"))
         (prompt (concat "Generate a concise commit message for these changes. Focus on what was changed and why. Keep it under 72 characters for the first line:\n\n" diff-output)))
    (if (string-empty-p (string-trim diff-output))
        (message "No staged changes found. Stage some changes first.")
      (gptel-request prompt
        :callback (lambda (response info)
                    (if response
                        (let ((commit-msg (string-trim response)))
                          (insert commit-msg))
                      (message "Failed to generate commit message: %s" info)))))))

;; Claude Code commit message generation functions
(defun jr/claude-code-generate-commit-message ()
  "Generate a commit message using Claude Code based on the current git diff."
  (interactive)
  (if (not (executable-find "claude"))
      (progn
        (message "Claude Code not found. Falling back to gptel.")
        (jr/ai-generate-commit-message))
    (let* ((diff-output (shell-command-to-string "git diff --staged"))
           (prompt (format "Generate a concise git commit message for these changes using Conventional Commits format. Output ONLY the commit message text, nothing else. No explanations, no quotes, no formatting. Focus on what was changed and why. Keep it under 72 characters for the first line. If you need multiple lines, add a blank line after the first line and then continue with more details.\n\nChanges:\n%s" diff-output)))
      (if (string-empty-p (string-trim diff-output))
          (message "No staged changes found. Stage some changes first.")
        (let* ((project-root (or (projectile-project-root) default-directory))
               (has-flake (file-exists-p (expand-file-name "flake.nix" project-root)))
               (nix-command (if has-flake
                                "cd %s && nix develop -c claude --print '%s' 2>/dev/null"
                              "nix-shell -p nodejs --run \"claude --print '%s'\" 2>&1"))
               (formatted-command (if has-flake
                                      (format nix-command
                                              (shell-quote-argument project-root)
                                              (replace-regexp-in-string "'" "'\"'\"'" prompt))
                                    (format nix-command
                                            (replace-regexp-in-string "'" "'\"'\"'" prompt))))
               (claude-output (shell-command-to-string formatted-command))
               ;; Clean up any nix warnings that might have slipped through
               (output-lines (split-string claude-output "\n"))
               (cleaned-lines (seq-remove
                               (lambda (line)
                                 (or (string-match-p "^\\(path\\|warning\\):" line)
                                     (string-match-p "does not contain a 'flake.nix'" line)
                                     (string-match-p "Git tree.*is dirty" line)
                                     (string-match-p "^searching up" line)))
                               output-lines))
               (cleaned-output (string-join cleaned-lines "\n"))
               (commit-msg (string-trim cleaned-output)))
          (if (or (string-empty-p commit-msg)
                  (string-match-p "^Error:" commit-msg))
              (message "Failed to generate commit message: %s" commit-msg)
            (with-current-buffer (get-buffer-create "*AI Commit Message*")
              (erase-buffer)
              (insert commit-msg)
              (display-buffer (current-buffer)))
            (message "AI commit message generated! Check *AI Commit Message* buffer.")))))))

(defun jr/claude-code-insert-commit-message ()
  "Insert Claude Code-generated commit message into current buffer."
  (interactive)
  (if (not (executable-find "claude"))
      (progn
        (message "Claude Code not found. Falling back to gptel.")
        (jr/ai-insert-commit-message))
    (let* ((diff-output (shell-command-to-string "git diff --staged"))
           (prompt (format "Generate a concise git commit message for these changes using Conventional Commits format. Output ONLY the commit message text, nothing else. No explanations, no quotes, no formatting. Focus on what was changed and why. Keep it under 72 characters for the first line. If you need multiple lines, add a blank line after the first line and then continue with more details.\n\nChanges:\n%s" diff-output)))
      (if (string-empty-p (string-trim diff-output))
          (message "No staged changes found. Stage some changes first.")
        (let* ((project-root (or (projectile-project-root) default-directory))
               (has-flake (file-exists-p (expand-file-name "flake.nix" project-root)))
               (nix-command (if has-flake
                                "cd %s && nix develop -c claude --print '%s' 2>/dev/null"
                              "nix-shell -p nodejs --run \"claude --print '%s'\" 2>&1"))
               (formatted-command (if has-flake
                                      (format nix-command
                                              (shell-quote-argument project-root)
                                              (replace-regexp-in-string "'" "'\"'\"'" prompt))
                                    (format nix-command
                                            (replace-regexp-in-string "'" "'\"'\"'" prompt))))
               (claude-output (shell-command-to-string formatted-command))
               ;; Clean up any nix warnings
               (output-lines (split-string claude-output "\n"))
               (cleaned-lines (seq-remove
                               (lambda (line)
                                 (or (string-match-p "^\\(path\\|warning\\):" line)
                                     (string-match-p "does not contain a 'flake.nix'" line)
                                     (string-match-p "Git tree.*is dirty" line)
                                     (string-match-p "^searching up" line)))
                               output-lines))
               (cleaned-output (string-join cleaned-lines "\n"))
               (commit-msg (string-trim cleaned-output)))
          (if (or (string-empty-p commit-msg)
                  (string-match-p "^Error:" commit-msg))
              (message "Failed to generate commit message: %s" commit-msg)
            (insert commit-msg)))))))

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

(map! :leader
      (:prefix ("g" . "git")
       :desc "Generate AI commit message" "m" #'jr/ai-generate-commit-message))

;; Add magit-specific keybinding
(after! magit
  (transient-append-suffix 'magit-commit "c"
    '("m" "AI commit message (Claude Code)" jr/claude-code-insert-commit-message))
  (transient-append-suffix 'magit-commit "m"
    '("M" "AI commit message (gptel)" jr/ai-insert-commit-message))

  ;; Add keybinding in commit message buffer
  (define-key git-commit-mode-map (kbd "C-c C-a") #'jr/claude-code-insert-commit-message)
  (define-key git-commit-mode-map (kbd "C-c C-g") #'jr/ai-insert-commit-message))

(provide 'config-copilot)
;;; config-copilot.el ends here
