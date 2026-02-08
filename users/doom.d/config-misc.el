;;; config-misc.el --- Miscellaneous utilities and minor modes -*- lexical-binding: t; -*-

;; ============================================================================
;; MISCELLANEOUS UTILITIES
;; ============================================================================
;; Various utility functions, minor modes, and small package configurations

;; ----------------------------------------------------------------------------
;; Helper Functions
;; ----------------------------------------------------------------------------

(defun set-go-path ()
  "Set GOPATH to the current git repository root."
  (interactive)
  (setenv "GOPATH" (expand-file-name (vc-git-root (pwd)))))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun my-create-newline-and-enter-sexp-kotlin (&rest _ignored)
  "Open a new brace or bracket expression for Kotlin."
  (message "kotlin begin")
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode)
  (message "kotlin end"))

;; Smartparens auto-newline for braces
(sp-local-pair 'rust-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'go-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

;; ----------------------------------------------------------------------------
;; Copyright Headers
;; ----------------------------------------------------------------------------

(defun my-insert-copyright-header()
  "Insert copyright header to file."
  (interactive)
  (insert "/*\n")
  (insert (format-time-string " * Copyright © 2002-%Y Bluebeam, Inc. All Rights Reserved.\n"))
  (insert " * Creator: Jonathan Rothberg\n")
  (insert " */\n"))

(defun my-insert-copyright-header-elm()
  "Insert copyright header to file (Elm style)."
  (interactive)
  (insert "{-\n")
  (insert (format-time-string "  Copyright © 2002-%Y Bluebeam, Inc. All Rights Reserved.\n"))
  (insert "  Creator: Jonathan Rothberg\n")
  (insert "-}\n"))

;; ----------------------------------------------------------------------------
;; Buffer Utilities
;; ----------------------------------------------------------------------------

(defun close-buffers-with-pattern (PATTERN)
  "Close buffers with a certain PATTERN prefix."
  (interactive "sPattern:")
  (cl-loop for buffer in (buffer-list)
           do (if (string-prefix-p PATTERN (buffer-name buffer))
                  (kill-buffer buffer))))

(defun close-request-buffers()
  "Close *request* buffers."
  (interactive)
  (close-buffers-with-pattern " *request"))

(defun jr-setup-dev-workspace()
  "Set up a development workspace with split window and eshell."
  (interactive)
  (split-window-horizontally)
  (+eshell/open-popup default-directory))

;; ----------------------------------------------------------------------------
;; Evil Little Word Mode
;; ----------------------------------------------------------------------------

(require 'evil-little-word)
(define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word)

;; ----------------------------------------------------------------------------
;; Blamer (Git Blame Inline)
;; ----------------------------------------------------------------------------

(use-package! blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background nil
                   :height 1.0
                   :italic t)))
  :config
  (map! :localleader :desc "blamer-show" "b s" #'blamer-show-posframe-commit-info)
  (set-face-attribute 'blamer-face nil :height 1.0)
  ;; (global-blamer-mode 1)
  )

;; ----------------------------------------------------------------------------
;; FZF Configuration
;; ----------------------------------------------------------------------------

(use-package! fzf
  :ensure
  :init
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll --"))

(defun my/fzf-git-files ()
  "Search for all files in a Git repository, including those ignored and hidden."
  (interactive)
  (let ((fzf/args "-x --tiebreak=index --print-query --preview '([[ -f {} ]] && (bat --color=always {} || cat {})) 2> /dev/null | head -200'"))
    (fzf/start (concat "git ls-files --cached --others --ignored --exclude-standard; "
                       "git ls-files --others --exclude-standard; "
                       "find . -type f -not -path '*/.git/*'") nil)))

(map! :leader :desc "fzf-find-files" "f h" #'my/fzf-git-files)
(map! :leader :desc "fzf-switch-buffer" "b h" #'fzf-switch-buffer)

;; ----------------------------------------------------------------------------
;; Whitespace Configuration
;; ----------------------------------------------------------------------------

(use-package! whitespace
  :config
  ;; Simplified whitespace style - only show trailing whitespace
  ;; Showing all spaces/tabs is expensive for redisplay
  (setq whitespace-style '(face trailing))

  ;; Set faces for whitespace
  (face-spec-set 'whitespace-trailing '((t (:background "red1" :foreground "yellow"))))

  ;; Enable whitespace mode in all programming modes
  (add-hook 'prog-mode-hook #'whitespace-mode))

;; Show trailing whitespace in prog modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

;; ----------------------------------------------------------------------------
;; Smooth Scrolling
;; ----------------------------------------------------------------------------
;; NOTE: smooth-scroll-margin was set to 999 which caused massive redisplay overhead.
;; A reasonable value is 5-10 lines.

(smooth-scrolling-mode 1)
(setq scroll-step 1)
(setq smooth-scroll-margin 5)  ; Was 999 - caused constant redisplay!

(setq scroll-preserve-screen-position t
      scroll-conservatively 101
      maximum-scroll-margin 0.25
      scroll-margin 5)

;; ----------------------------------------------------------------------------
;; Lusty Explorer
;; ----------------------------------------------------------------------------

(use-package! lusty-explorer
  :ensure
  :init)

;; ----------------------------------------------------------------------------
;; Eyebrowse (Workspace Management)
;; ----------------------------------------------------------------------------

(use-package! eyebrowse
  :ensure
  :config
  (eyebrowse-mode t))

;; ----------------------------------------------------------------------------
;; Topsy (Sticky Function Headers)
;; ----------------------------------------------------------------------------

(use-package topsy
  :hook
  (prog-mode . topsy-mode))

;; ----------------------------------------------------------------------------
;; DAP Mode (Debugging)
;; ----------------------------------------------------------------------------

(use-package! dap-mode
  :config
  (dap-mode 1)
  (setq dap-print-io t)
  (require 'dap-hydra)
  (require 'dap-dlv-go)

  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1)))

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; ----------------------------------------------------------------------------
;; Zig Mode
;; ----------------------------------------------------------------------------

(use-package! zig-mode
  :hook ((zig-mode . lsp-deferred))
  :custom (zig-format-on-save t)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (message (concat (getenv "HOME") "/zls/zls"))
    (lsp-register-client
     (make-lsp-client
      :new-connection (lsp-stdio-connection (executable-find "zls"))
      :major-modes '(zig-mode)
      :server-id 'zls))))

;; ----------------------------------------------------------------------------
;; Gleam Mode
;; ----------------------------------------------------------------------------

(use-package! gleam-mode
  :bind (:map gleam-mode-map
              ("C-c g f" . gleam-format)))

;; ----------------------------------------------------------------------------
;; Jenkinsfile Mode
;; ----------------------------------------------------------------------------

(use-package! jenkinsfile-mode
  :config
  (add-to-list 'auto-mode-alist '(".*Jenkinsfile.*\\'" . jenkinsfile-mode)))

;; ----------------------------------------------------------------------------
;; Highlight Indent Guides
;; ----------------------------------------------------------------------------
;; NOTE: 'bitmap' method is expensive for redisplay. Using 'character' instead.
;; If still causing issues, consider disabling entirely.

(use-package! highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  ;; 'character' is much cheaper than 'bitmap' for redisplay
  (setq highlight-indent-guides-method 'character)
  (setq highlight-indent-guides-character ?\|)  ; Use pipe character
  (setq highlight-indent-guides-auto-character-face-perc 30)
  (setq highlight-indent-guides-auto-enabled nil)
  (setq highlight-indent-guides-responsive nil)  ; Disable responsive highlighting
  (set-face-foreground 'highlight-indent-guides-character-face "gray30"))

;; ----------------------------------------------------------------------------
;; Shell Formatting (shfmt)
;; ----------------------------------------------------------------------------

(use-package! shfmt
  :config
  (add-hook 'sh-mode-hook 'shfmt-on-save-mode))

;; ----------------------------------------------------------------------------
;; Affe (Async Fuzzy Finder)
;; ----------------------------------------------------------------------------

(defun affe-orderless-regexp-compiler (input _type)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(use-package! affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

;; ----------------------------------------------------------------------------
;; Consult Toggle Preview
;; ----------------------------------------------------------------------------

(defvar-local consult-toggle-preview-orig nil)

(defun consult-toggle-preview ()
  "Command to enable/disable preview."
  (interactive)
  (if consult-toggle-preview-orig
      (setq consult--preview-function consult-toggle-preview-orig
            consult-toggle-preview-orig nil)
    (setq consult-toggle-preview-orig consult--preview-function
          consult--preview-function #'ignore)))

(define-key vertico-map (kbd "C-.") #'consult-toggle-preview)

;; ----------------------------------------------------------------------------
;; Code Review Mode
;; ----------------------------------------------------------------------------

(setq code-review-auth-login-marker 'forge)

(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

;; ----------------------------------------------------------------------------
;; Jinx (Spell Checking)
;; ----------------------------------------------------------------------------

(setq ispell-program-name "hunspell")
(setq ispell-dictionary "en_US")

(use-package! jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-C-c" . jinx-correct)
         ("C-M-l" . jinx-languages)))

;; ----------------------------------------------------------------------------
;; Elfeed (RSS Reader)
;; ----------------------------------------------------------------------------

(require 'elfeed-org)
(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
(elfeed-org)
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
(setq browse-url-browser-function 'eww-browse-url)

(after! elfeed
  (set-popup-rule! "^\\*elfeed-entry"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)

  (set-popup-rule! "^\\*eww"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t))

(defun my/elfeed-show-eww (&optional link)
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (if link link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

;; ----------------------------------------------------------------------------
;; Projectile Configuration
;; ----------------------------------------------------------------------------

(after! projectile
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-file-exists-remote-cache-expire nil)

  ;; For Consult
  (setq consult-project-function #'projectile-project-root)
  (setq consult-find-args "find . -not ( -path '*/.*' -prune )")

  ;; For ripgrep
  (setq rg-ignore-case 'smart)
  (setq rg-group-result t)
  (setq rg-show-columns t)

  ;; Refresh projectile cache
  (define-key projectile-mode-map (kbd "C-c p I") 'projectile-invalidate-cache))

;; ----------------------------------------------------------------------------
;; Git Utilities (Simple)
;; ----------------------------------------------------------------------------

(defun jr/dired-untracked (dir)
  "Open dired buffer showing untracked files in DIR."
  (interactive "DUntracked in directory: ")
  (cd (projectile-project-root))
  (switch-to-buffer (get-buffer-create "*untracked*"))
  (shell-command "git ls-files --others --exclude-standard | xargs ls -l" (current-buffer))
  (dired-mode dir)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))

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

;; ----------------------------------------------------------------------------
;; Debug Mode Setup
;; ----------------------------------------------------------------------------

(defun debug-mode-setup ()
  "Debug function to check current mode setup and configurations."
  (interactive)
  (let ((mode-info (list
                    (cons "Major Mode" major-mode)
                    (cons "Format With" (bound-and-true-p +format-with))
                    (cons "Format With LSP" (bound-and-true-p +format-with-lsp))
                    (cons "LSP Active" (bound-and-true-p lsp-mode))
                    (cons "Tree-sitter Available" (and (fboundp 'treesit-available-p) (treesit-available-p)))
                    (cons "Buffer File" (buffer-file-name)))))
    (message "Mode Debug Info: %s" mode-info)
    (with-current-buffer (get-buffer-create "*Mode Debug*")
      (erase-buffer)
      (insert "Current Mode Debug Information\n")
      (insert "==================================\n\n")
      (dolist (info mode-info)
        (insert (format "%-20s: %s\n" (car info) (cdr info))))
      (display-buffer (current-buffer)))))

;; ----------------------------------------------------------------------------
;; Host-specific Configuration
;; ----------------------------------------------------------------------------

(let ((host-init-file (format "~/.doom.d/hosts/init-%s.el" (system-name))))
  (message host-init-file)
  (when (file-exists-p host-init-file)
    (message "found local file, loading it...")
    (load-file host-init-file)))

;; ----------------------------------------------------------------------------
;; Komodo Custom Configuration
;; ----------------------------------------------------------------------------

(when (file-exists-p "~/Repositories/komodo")
  (message "loading komodo folder...")
  (add-to-list 'load-path "~/Repositories/komodo")
  (load-directory "~/Repositories/komodo"))

(provide 'config-misc)
;;; config-misc.el ends here
