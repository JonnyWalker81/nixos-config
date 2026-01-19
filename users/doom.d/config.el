;;-*- lexical-binding: t -*-
;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(message "loading...")

;; Fc498fed98a6df8adca33e87433b4084c0340fb4aorce HTML files to use web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun load-directory (dir)
  (let ((load-it (lambda (f)
                   (load-file (concat (file-name-as-directory dir) f)))
                 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Add personal doom config directory to load-path
(add-to-list 'load-path "~/.doom.d")
(add-to-list 'load-path "~/.elisp")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'load-path "~/Repositories/org-reveal")

(message "loaded path config...")

;; Ensure exec-path-from-shell is available early
(when (memq window-system '(mac ns x))
  (require 'exec-path-from-shell nil t))

(if (eq system-type 'darwin)
    (progn
      ;; Add nix paths to exec-path for macOS using dynamic home directory
      (let ((home (expand-file-name "~")))
        (add-to-list 'exec-path (concat home "/.local/state/nix/profiles/home-manager/home-path/bin"))
        (add-to-list 'exec-path (concat home "/.nix-profile/bin")))
      (add-to-list 'exec-path "/nix/var/nix/profiles/default/bin")
      (add-to-list 'exec-path "/run/current-system/sw/bin")
      ;; Initialize exec-path-from-shell if available
      (when (fboundp 'exec-path-from-shell-initialize)
        (exec-path-from-shell-initialize))
      ;; Copy environment variables
      (when (fboundp 'exec-path-from-shell-copy-env)
        (exec-path-from-shell-copy-env "GOPATH")
        (exec-path-from-shell-copy-env "RUST_SRC_PATH")
        (exec-path-from-shell-copy-env "RUSTUP_HOME")
        (exec-path-from-shell-copy-env "CARGO_HOME")
        (exec-path-from-shell-copy-env "PATH"))))

(if (eq system-type 'linux)
    (progn
      (exec-path-from-shell-copy-env "GOPATH")
      (exec-path-from-shell-copy-env "RUST_SRC_PATH")
      (exec-path-from-shell-copy-env "RUSTUP_HOME")
      (exec-path-from-shell-copy-env "CARGO_HOME")))

(add-to-list 'treesit-extra-load-path "~/.tree-sitter/bin")

(load! "config-misc")
(load! "config-font")
(load! "config-bindings")
(load! "config-rust")

(require 'vc-git)

;; Global setting - use spaces instead of tabs
(setq-default indent-tabs-mode nil)  ; Use spaces instead of tabs globally

;; Ensure electric-indent uses spaces
(setq electric-indent-inhibit nil)

;; Hook for all programming modes to use spaces by default
;; (Go mode will override this with its own settings)
(add-hook 'prog-mode-hook
          (lambda ()
            (unless (derived-mode-p 'go-mode 'go-ts-mode)
              (setq indent-tabs-mode nil))))

;; Text mode should also use spaces
(add-hook 'text-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)))

(toggle-frame-maximized)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(require 'nix-shell)

(require 'evil-little-word)
(define-key evil-normal-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-normal-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-operator-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-operator-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "w") 'evil-forward-little-word-begin)
(define-key evil-visual-state-map (kbd "b") 'evil-backward-little-word-begin)
(define-key evil-visual-state-map (kbd "i w") 'evil-inner-little-word)

(setq auth-sources '("~/.authinfo"))

(message "loaded authinfo...")
(setq visual-fill-column-width 80)

(setq user-full-name "Jonathan Rothberg"
      user-mail-address "jon@join.build")

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
  (map! :localleader  :desc "blamer-show" "b s" #'blamer-show-posframe-commit-info)
  (set-face-attribute 'blamer-face nil :height 1.0)
  ;; (global-blamer-mode 1)
  )

(setq magit-todos-exclude-globs '(
                                  "*brindle*"
                                  "*node_modules*"
                                  ))

(message "loaded blamer...")
(map! :leader  :desc "dumb-jump-go" "d" #'dumb-jump-go)
(map! :leader  :desc "dumb-jump-go other window" "D" #'dumb-jump-go-other-window)

(map! :leader :desc "fold code" "c f" #'fold-this)
(map! :leader :desc "fold code" "c u" #'fold-this-unfold-at-point)

(map! :leader :desc "toggle lsp doc" "t d" #'lsp-ui-doc-toggle)

(setq +popup-buffer-mode 'toggle)
(setq doom-line-numbers-style 'relative)
(setq ns-use-thin-smoothing t)

(require 'protobuf-mode)
(require 'prettier-js)

(message "loadded prettier-js...")

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

;; Let Doom handle file associations automatically

(set-fill-column 120)

(message "before lsp-mode...")

;; (add-hook 'go-mode-hook #'lsp-deferred)
(after! lsp-mode
  (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level))
  (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-ts-indent-level))
  (add-hook 'go-ts-mode-hook #'lsp-deferred)
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (add-hook 'js-ts-mode-hook #'lsp-deferred)
  (add-hook 'html-ts-mode-hook #'lsp-deferred)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 3000)

  ;; Use the VSCode HTML Language Server instead of the generic one
  (setq lsp-html-server-command '("vscode-html-language-server" "--stdio"))
  ;; You can also customize formatting settings:
  (setq lsp-html-format-enable t         ; ensure formatting is active
        lsp-html-format-wrap-line-length 0
        lsp-html-format-unformatted      nil)

  ;; These formatting hooks are now handled by the unified typescript-js-setup-hook

  (add-hook 'graphql-mode-hook #'lsp)

  )

(after! forge
  (setq forge-topic-list-limit '(60 . 0))     ; show more PRs in list buffers
  (setq forge-owned-queries
        '(("Waiting For My Review" . "is:open review-requested:@me")
          ("Nightshade"            . "is:open label:nightshade-pod"))))

;; ============================================================================
;; HTML/WEB CONFIGURATION
;; ============================================================================
;; HTML and web mode configuration with LSP support

;; HTML setup function with error handling
(defun html-web-setup-hook ()
  "Setup function for HTML and web modes."
  (condition-case err
      (progn
        (setq-local +format-with 'prettier)
        ;; Enable Emmet for HTML if available
        (when (featurep 'emmet-mode)
          (emmet-mode 1)))
    (error
     (message "Error in html-web-setup-hook: %s" err))))

;; HTML mode configuration - deferred for performance
(with-eval-after-load 'html-mode
  (add-hook 'html-mode-hook #'lsp-deferred)
  (add-hook 'html-mode-hook #'html-web-setup-hook))
(with-eval-after-load 'html-ts-mode
  (add-hook 'html-ts-mode-hook #'lsp-deferred)
  (add-hook 'html-ts-mode-hook #'html-web-setup-hook))

;; Web mode configuration - deferred for performance
(with-eval-after-load 'web-mode
  (add-hook 'web-mode-hook #'lsp-deferred)
  (add-hook 'web-mode-hook #'html-web-setup-hook))

(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level)))

(message "after lsp-mode...")

(message "after lsp...")

(setq doom-line-numbers-style 'relative)

(after! org

  ;; Enable PlantUML, D2, and Mermaid in org-babel
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((plantuml . t)
     (d2 . t)
     (mermaid . t)))

  ;; Use plantuml command (works with NixOS flake environment)
  (setq org-plantuml-exec-mode 'plantuml)

  ;; Don't ask for confirmation when executing PlantUML/D2/Mermaid blocks
  (setq org-confirm-babel-evaluate nil)

  (require 'org-re-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@5")

  (require 'ox-presenterm)
  (add-to-list 'org-export-backends 'presenterm)

  (add-to-list 'org-capture-templates
               '("w" "Work Todo"  entry
                 (file "work.org")
                 "* TODO %T %?  :work:" :empty-lines 1))

  (setq org-agenda-custom-commands
        '(("w" "Work Todos"
           ((agenda "")
            (tags-todo "work")
            ))
          ("a" "Work Todos"
           ((agenda "")
            (alltodo "")
            ))
          ))

  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

  (map! :map org-mode-map
        :localleader
        (:prefix ("e" . "export")
         :desc "Export to Presenterm" "P" #'org-presenterm-export-to-markdown))

  )

;; D2 diagram support for org-babel
(use-package! ob-d2
  :after org
  :config
  ;; Ensure D2 is available in org-babel
  (add-to-list 'org-babel-load-languages '(d2 . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; Mermaid diagram support for org-babel
(use-package! ob-mermaid
  :after org
  :config
  ;; Set the command to use mermaid-cli (mmdc)
  (setq ob-mermaid-cli-path "mmdc")
  ;; Set default arguments for mmdc (avoid the -i issue)
  (setq org-babel-default-header-args:mermaid
        '((:results . "file")
          (:exports . "results")))
  ;; Ensure the execute function is available
  (require 'ob-mermaid)
  ;; Ensure Mermaid is available in org-babel
  (add-to-list 'org-babel-load-languages '(mermaid . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))))

(setq doom-line-numbers-style 'relative)
(setq nlinum-highlight-current-line t)

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
  (defadvice magit-insert-diff (around skip-large-files activate)
    "Skip showing diffs for very large files."
    (let ((file (magit-file-at-point)))
      (if (and file
               (file-exists-p file)
               (> (file-attribute-size (file-attributes file)) 5000000)) ; 5MB limit
          (insert (propertize (format "Diff skipped for large file: %s (>5MB)\n"
                                      (file-name-nondirectory file))
                              'face 'magit-diff-file-heading))
        ad-do-it)))

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

(add-hook 'protobuf-mode-hook (lambda ()
                                (format-all-mode t)
                                ))

(message "after magit...")

;; ============================================================================
;; TYPESCRIPT/JAVASCRIPT CONFIGURATION
;; ============================================================================
;; Consolidated TypeScript and JavaScript configuration to avoid conflicts

;; Global LSP formatting settings - disable LSP formatting to avoid conflicts
(setq lsp-format-buffer-on-save nil
      lsp-javascript-format-enable nil
      lsp-typescript-format-enable nil
      +format-with-lsp nil)

;; Unified setup function for TypeScript/JavaScript modes with error handling
(defun typescript-js-setup-hook ()
  "Unified setup function for TypeScript and JavaScript modes."
  (condition-case err
      (progn
        (setq-local +format-with-lsp nil
                    +format-with 'prettier)
        ;; Remove LSP formatting hooks to avoid conflicts
        (remove-hook 'before-save-hook #'lsp-format-buffer t)
        (remove-hook 'before-save-hook #'lsp-organize-imports t))
    (error
     (message "Error in typescript-js-setup-hook: %s" err))))

;; TypeScript mode configuration
(after! typescript-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'typescript-js-setup-hook))

;; TypeScript TSX mode configuration
(after! typescript-tsx-mode
  (setq typescript-indent-level 2)
  (add-hook 'typescript-tsx-mode-hook #'typescript-js-setup-hook))

;; Tree-sitter mode configuration - added with defer for performance
(with-eval-after-load 'typescript-ts-mode
  (add-hook 'typescript-ts-mode-hook #'typescript-js-setup-hook))
(with-eval-after-load 'tsx-ts-mode
  (add-hook 'tsx-ts-mode-hook #'typescript-js-setup-hook))
(with-eval-after-load 'js-ts-mode
  (add-hook 'js-ts-mode-hook #'typescript-js-setup-hook))

;; TypeScript Language Server configuration
(after! lsp-mode
  ;; Tell tsserver to prefer non-relative paths and auto-update imports
  (setq lsp-clients-typescript-init-opts
        '(:typescript (:preferences (:importModuleSpecifier "non-relative"))
          :javascript (:preferences (:importModuleSpecifier "non-relative"))
          :preferences (:updateImportsOnFileMove "always"))))

;; These settings are now handled by the unified setup function

;; ============================================================================
;; DEBUGGING AND TROUBLESHOOTING HELPERS
;; ============================================================================

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

(message "Configuration setup completed - TypeScript/JavaScript/HTML modes optimized")

;; (add-hook 'typescript-mode-hook
;;           (lambda ()
;;             ;; Remove LSP’s format and organize-imports on save
;;             (remove-hook 'before-save-hook #'lsp-format-buffer t)
;;             (remove-hook 'before-save-hook #'lsp-organize-imports t)))
;;
;; (add-hook 'typescript-tsx-mode-hook
;;           (lambda ()
;;             ;; Remove LSP’s format and organize-imports on save
;;             (remove-hook 'before-save-hook #'lsp-format-buffer t)
;;             (remove-hook 'before-save-hook #'lsp-organize-imports t)))
;;
;; (setq +format-with-lsp nil)
;;
;; (setq lsp-format-buffer-on-save nil)
;;
;; (setq lsp-javascript-format-enable nil
;;       lsp-typescript-format-enable nil)

(auto-composition-mode t)

;; This line seems to be a typo - commenting it out
;; doom-font (font-spec :family "JetBrains Mono" :size 5)

;; Font configuration that works on both macOS and Linux
(let ((font-family (if (member "JetBrains Mono" (font-family-list))
                       "JetBrains Mono"
                     "JetBrainsMono"))) ; Fallback for systems where it's registered without space
  (setq doom-font (font-spec :family font-family :size 16)
        doom-big-font (font-spec :family font-family :size 24)))

;; Load elfeed-org
(require 'elfeed-org)

(require 'elfeed-goodies)
(elfeed-goodies/setup)
(setq elfeed-goodies/entry-pane-size 0.5)
;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files (list "~/.doom.d/elfeed.org"))
(setq browse-url-browser-function 'eww-browse-url)

(after! elfeed
  (set-popup-rule! "^\\*elfeed-entry"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)

  (set-popup-rule! "^\\*eww"
    :size 0.75 :actions '(display-buffer-below-selected)
    :select t :quit nil :ttl t)
  )

(defun my/elfeed-show-eww (&optional link)
  (interactive)
  (let* ((entry (if (eq major-mode 'elfeed-show-mode)
                    elfeed-show-entry
                  (elfeed-search-selected :ignore-region)))
         (link (if link link (elfeed-entry-link entry))))
    (eww link)
    (add-hook 'eww-after-render-hook 'eww-readable nil t)))

(let ((host-init-file (format "~/.doom.d/hosts/init-%s.el" (system-name))))
  (message host-init-file)
  (when (file-exists-p host-init-file)
    (message "found local file, loading it...")
    (load-file host-init-file)))

(add-hook 'org-mode-hook (lambda ()
                           (electric-indent-local-mode -1)
                           (setq org-adapt-indentation t)
                           ))

(setq go-tag-args (list "-transform" "camelcase"))

(setq github-review-fetch-top-level-and-review-comments t)

(require 'exec-path-from-shell)

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(require 'keychain-environment)
(keychain-refresh-environment)

(use-package! vertico
  :custom
  (vertico-count 15)    ; show 15 candidates at once
  (vertico-resize t)    ; grow and shrink the minibuffer
  (vertico-cycle t)    ; wrap around at the top/bottom
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode)
  )

(use-package! corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  (corfu-separator ?\s))

(after! vertico
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

(use-package! marginalia
  :init
  (marginalia-mode)
  )

(use-package! consult
  :after vertico)

(use-package! embark
  :bind
  (("C-." . embark-act)    ; pick action for candidate
   ("M-." . embark-dwim))) ; do-what-I-mean

(use-package! cape
  :init
  ;; add common backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  )

;; ;; Use the `orderless' completion style.
;; ;; Enable `partial-completion' for files to allow path expansion.
;; ;; You may prefer to use `initials' instead of `partial-completion'.
;; (use-package orderless
;;   :init
;;   (setq completion-styles '(orderless)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles . (partial-completion))))))

;; ;; A few more useful configurations...
;; (use-package emacs
;;   :init
;;   ;; Add prompt indicator to `completing-read-multiple'.
;;   (defun crm-indicator (args)
;;     (cons (concat "[CRM] " (car args)) (cdr args)))
;;   (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

;;   ;; Grow and shrink minibuffer
;;   ;;(setq resize-mini-windows t)

;;   ;; Do not allow the cursor in the minibuffer prompt
;;   (setq minibuffer-prompt-properties
;;         '(read-only t cursor-intangible t face minibuffer-prompt))
;;   (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;;   ;; Enable recursive minibuffers
;;   (setq enable-recursive-minibuffers t))

(message "after vertico...")
(use-package savehist
  :init
  (savehist-mode))

(message "after savehist...")

;; Enable richer annotations using the Marginalia package
;; (use-package marginalia
;;   ;; Either bind `marginalia-cycle` globally or only in the minibuffer
;;   :after vertico
;;   :ensure t
;;   :bind (("M-A" . marginalia-cycle)
;;          :map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))
;;   :custom
;;   (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))

;;   ;; The :init configuration is always executed (Not lazy!)
;;   :init

;;   ;; Must be in the :init section of use-package such that the mode gets
;;   ;; enabled right away. Note that this forces loading the package.
;;   (marginalia-mode))

;; (use-package! selectrum
;;   :ensure t
;;   :init
;;   (selectrum-mode +1))

;; (use-package modus-themes
;; :ensure t
;; :config
;; ;; Add all your customizations prior to loading the themes
;; (setq modus-themes-italic-constructs t
;;       modus-themes-bold-constructs nil)
;;
;; ;; Maybe define some palette overrides, such as by using our presets
;; (setq modus-themes-common-palette-overrides
;;       modus-themes-preset-overrides-intense)
;;
;; (setq modus-themes-italic-constructs t
;;     modus-themes-bold-constructs nil
;;     modus-themes-mixed-fonts t
;;     modus-themes-variable-pitch-ui nil
;;     modus-themes-custom-auto-reload t
;;     modus-themes-disable-other-themes t)
;;
;; ;; Load the theme of your choice.
;; (load-theme 'modus-vivendi)
;;
;; (define-key global-map (kbd "<f5>") #'modus-themes-toggle)
;;
;; )

;; (setq doom-theme 'modus-vivendi-deuteranopia)
(setq doom-theme 'doom-tokyo-night)

(message "after doom-theme modus...")

;; (use-package window-stool
;;   :config
;;   (add-hook 'prog-mode-hook #'window-stool-mode))

(use-package! treesitter-context
  :after tree-sitter
  ;; :hook ((prog-mode . treesitter-context-mode)
  ;;        (prog-mode . treesitter-context-focus-mode))
  :config
  ;; optional: customize defaults
  (setq
   treesitter-context-idle-time 0.5
   treesitter-context-show-line-number t
   treesitter-context-frame-indent-offset 2)
  )

(add-hook 'go-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'go-ts-mode-hook #'treesitter-context-focus-mode)

(add-hook 'typescript-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'typescript-ts-mode-hook #'treesitter-context-focus-mode)

(add-hook 'tsx-ts-mode-hook #'treesitter-context-mode)
;; (add-hook 'tsx-ts-mode-hook #'treesitter-context-focus-mode)

(use-package topsy
  :hook
  (prog-mode . topsy-mode)
  )

(message "after topsy...")

;; ============================================================================
;; WHITESPACE CONFIGURATION
;; ============================================================================
;; Show whitespace characters in programming modes

;; Configure whitespace display
(use-package! whitespace
  :config
  ;; Configure which whitespace to show
  (setq whitespace-style '(face tabs tab-mark spaces space-mark trailing))

  ;; Configure whitespace display characters
  (setq whitespace-display-mappings
        '((space-mark 32 [183] [46])    ; 32 SPACE → · (middle dot) or .
          (space-mark 160 [164] [95])   ; 160 NO-BREAK SPACE → ¤ or _
          (tab-mark 9 [187 9] [92 9]))) ; 9 TAB → » followed by tab or \ followed by tab

  ;; Set faces for whitespace using face-spec-set instead of custom-set-faces
  (face-spec-set 'whitespace-space '((t (:foreground "gray30"))))
  (face-spec-set 'whitespace-tab '((t (:foreground "gray30"))))
  (face-spec-set 'whitespace-trailing '((t (:background "red1" :foreground "yellow"))))

  ;; Enable whitespace mode in all programming modes
  (add-hook 'prog-mode-hook #'whitespace-mode))

;; Alternative simpler configuration - just show trailing whitespace
(add-hook 'prog-mode-hook
          (lambda ()
            (setq show-trailing-whitespace t)))

(use-package! fzf
  :ensure
  :init
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll --")
  )

(message "after fzf...")

(defun my/fzf-git-files ()
  "Search for all files in a Git repository, including those ignored and hidden."
  (interactive)
  (let ((fzf/args "-x --tiebreak=index --print-query --preview '([[ -f {} ]] && (bat --color=always {} || cat {})) 2> /dev/null | head -200'"))
    (fzf/start (concat "git ls-files --cached --others --ignored --exclude-standard; "
                       "git ls-files --others --exclude-standard; "
                       "find . -type f -not -path '*/.git/*'") nil)))

;; (global-set-key (kbd "C-c f") 'my/fzf-git-files)
;; (map! :leader  :desc "fzf-projectile" "f h" #'fzf-projectile)
(map! :leader  :desc "fzf-find-files" "f h" #'my/fzf-git-files)
(map! :leader  :desc "fzf-switch-buffer" "b h" #'fzf-switch-buffer)

(map! :leader :desc "cr-pull-reqs" "l p" #'(lambda () (forge-list-labeled-pullreqs (forge-get-repository t) "costs-and-reporting-pod"
                                                                                   )))
(map! :leader :desc "lsp-mode" "l l" #'lsp-mode)
(map! :leader :desc "find in buffer" "f /" #'consult-line)

(message "after key-mappings...")

(use-package! lusty-explorer
  :ensure
  :init
  )

(smooth-scrolling-mode 1)
(setq scroll-step 1)
(setq smooth-scroll-margin 999)

(message "after scroll stuff...")

(setq scroll-preserve-screen-position t
      scroll-conservatively 0
      maximum-scroll-margin 0.5
      scroll-margin 99999)

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

(message "after zig-mode...")

(defun affe-orderless-regexp-compiler (input _type)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(use-package! affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(message "after affe...")

;; (use-package! consult
;; :hook (completion-list-mode . consult-preview-at-point-mode)
;; :config
;; (consult-fd-args "-I -H")

;; (setq consult--customize-alist nil)
;;   (consult-customize
;;    consult-ripgrep consult-git-grep consult-grep
;;    consult-bookmark consult-recent-file
;;    +default/search-project +default/search-other-project
;;    +default/search-project-for-symbol-at-point
;;    +default/search-cwd +default/search-other-cwd
;;    +default/search-notes-for-symbol-at-point
;;    +default/search-emacsd
;;    consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
;;    :preview-key 'any)
;; )

(message "after consult...")

(use-package! eyebrowse
  :ensure
  :config
  (eyebrowse-mode t)
  )

(message "after eyebrowse...")

(use-package! dap-mode
  ;;:custom
  ;;(dap-go-debug-program `("node" "~/extension/out/src/debugAdapter/goDebug.js"))
  :config
  (dap-mode 1)
  (setq dap-print-io t)
  ;;(setq fit-window-to-buffer-horizontally t)
  ;;(setq window-resize-pixelwise t)
  (require 'dap-hydra)
  ;; old version
  ;;  (require 'dap-go)                ; download and expand vscode-go-extenstion to the =~/.extensions/go=
  ;;  (dap-go-setup)
  ;; new version
  (require 'dap-dlv-go)

  (use-package dap-ui
    :ensure nil
    :config
    (dap-ui-mode 1)
    )
  )

(message "after dap-ui...")

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

(message "after ocamlformat...")

;; (use-package! dired-preview
;;   :hook ((dired-mode . dired-preview-mode))
;;   )
;; (after! tuareg-mode
;;   (add-hook 'before-save-hook #'ocamlformat-before-save)
;; )

;; ============================================================================
;; GO/GOLANG CONFIGURATION WITH GOPLS
;; ============================================================================
;; Configuration matching VSCode Go extension settings

(use-package! go-mode
  :config
  ;; Use goimports as the format tool (matches "go.formatTool": "goimports")
  (setq gofmt-command "goimports")

  ;; Configure golangci-lint as the linter (matches "go.lintTool": "golangci-lint")
  (setq flycheck-go-golint-executable "golangci-lint")

  ;; IMPORTANT: Go uses tabs, not spaces for indentation
  ;; Set proper tab width for Go (standard is 4 or 8)
  (setq go-tab-width 4)

  ;; Hook for Go mode setup
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              ;; Ensure Go uses tabs for indentation
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save (matches "[go]": {"editor.formatOnSave": true})
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports on save (matches "source.organizeImports": true)
              (add-hook 'before-save-hook #'lsp-organize-imports nil t)))

  ;; Also configure go-ts-mode (tree-sitter mode)
  (add-hook 'go-ts-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              ;; Ensure Go uses tabs for indentation
              (setq indent-tabs-mode nil)
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports on save
              (add-hook 'before-save-hook #'lsp-organize-imports nil t))))

(after! lsp-go
  ;; Tell gopls to use goimports
  (setq lsp-go-format-tool "goimports")

  ;; Configure gopls settings to match VSCode config
  (setq lsp-go-gopls-server-args
        '("-remote=auto"))

  ;; Set gopls build directory filters (matches gopls "build.directoryFilters")
  (setq lsp-go-directory-filters
        ["-**/node_modules"
         "-**/testdata"])

  ;; Set local module for import organization (matches gopls "formatting.local")
  (setq lsp-go-imports-local-prefix "github.com/JoinCAD/komodo")

  ;; Enable all analyses
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (shadow . t)
          (unusedparams . t)
          (unusedwrite . t)
          (useany . t)
          (unusedvariable . t))))

;; Configure golangci-lint integration
(after! flycheck
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Set golangci-lint config file path (matches "go.lintFlags")
              (setq-local flycheck-golangci-lint-config "./.golangci-github.toml")
              ;; Enable flycheck mode for linting
              (flycheck-mode 1))))

;; Set formatter for tree-sitter Go mode
(set-formatter! 'gofmt '("goimports") :modes '(go-mode go-ts-mode))

;; Additional gopls configuration via LSP
(after! lsp-mode
  ;; Add Go-specific LSP settings
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]testdata\\'")

  ;; Configure gopls through LSP initialization options
  (setq lsp-gopls-server-args '("-remote=auto"))
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-gopls-use-placeholders t)

  ;; Custom gopls settings matching VSCode configuration
  (lsp-register-custom-settings
   '(("gopls.formatting.local" "github.com/JoinCAD/komodo")
     ("gopls.build.directoryFilters" ["-**/node_modules" "-**/testdata"])
     ("gopls.analyses.fieldalignment" t)
     ("gopls.analyses.unusedparams" t)
     ("gopls.analyses.unusedwrite" t)
     ("gopls.analyses.useany" t))))

;; (after! go-mode
;;   (setq gofmt-command "goimports")
;;   (setq-hook! 'go-mode-hook +format-with-lsp t)
;;   )
;;
;;
(setq display-line-numbers-type 'relative)

(use-package! jenkinsfile-mode
  :config
  (add-to-list 'auto-mode-alist '(".*Jenkinsfile.*\\'" . jenkinsfile-mode))
  )

(defun jr/prettify-and-save()
  (interactive)

  (let ((current-mode major-mode))
    (fundamental-mode)
    (setq prettier-js-args '("--trailing-comma" "es5"))
    (prettier-js)
    (setq prettier-js-args nil)
    (save-buffer)
    (funcall current-mode)
    )

  )

(defun jr/pretty-and-save()
  (interactive)
  ;; (prettier-js)
  (save-buffer)

  )

(defun jr/set-copilot-node-executable ()
  (interactive)
  (let ((node-path (executable-find "node")))
    (if node-path
        (progn
          (message "Setting copilot node executable to %s" node-path)
          (setq copilot-node-executable node-path))
      (message "Warning: Node.js not found in PATH. Cannot set copilot node executable."))))

(map! :localleader  :desc "jr/prettify" "s" #'jr/prettify-and-save)

;; Define function to conditionally enable copilot
(defun jr/copilot-maybe-enable ()
  "Enable copilot-mode if node is available in the current environment."
  (when (and (executable-find "node")
             (fboundp 'copilot-mode))
    (setq copilot-node-executable (executable-find "node"))
    (copilot-mode 1)
    (message "Copilot enabled with node: %s" copilot-node-executable)))

;; accept completion from copilot and fallback to company
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
    (message "Warning: Node.js not found at load time. Copilot will activate if node becomes available."))
  )

;; Copilot package is now always loaded, but only activates when node is available
(message "after copilot... (Package loaded, will activate when node is available)")
(defun jr/copilot-tab ()
  "Tab command that will complete with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (if (and (fboundp 'copilot-accept-completion)
           (executable-find "node"))
      (copilot-accept-completion)
    (message "Copilot not available (Node.js might be missing)"))
  ;; (or (copilot-accept-completion)
  ;;     (and (company--active-p) (company-complete))
  ;;     (evil-insert 1)
  ;;     ;; (indent-for-tab-command)
  ;;     )
  )

(defun jr/nix-post-activate-check ()
  "Check for node availability after entering nix-shell and enable copilot if appropriate."
  (jr/set-copilot-node-executable)
  ;; If we're in a prog-mode buffer and copilot isn't active, try to enable it
  (when (and (derived-mode-p 'prog-mode 'yaml-mode 'yaml-ts-mode 'json-mode 'json-ts-mode 'xml-mode)
             (not (bound-and-true-p copilot-mode))
             (executable-find "node"))
    (jr/copilot-maybe-enable))
  )

(add-hook 'nix-shell-post-activate-hooks 'jr/nix-post-activate-check)

(setq doom-line-numbers-style 'relative)

(setq code-review-auth-login-marker 'forge)

(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

; (use-package! gleam-mode
;   :bind (:map gleam-mode-map
;               ("C-c g f" . gleam-format)))

(use-package! highlight-indent-guides
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'bitmap)
  (setq highlight-indent-guides-bitmap-function 'highlight-indent-guides--bitmap-line)
  (setq highlight-indent-guides-auto-character-face-perc 50)
  (setq highlight-indent-guides-auto-enabled nil)
  (set-face-background 'highlight-indent-guides-odd-face "darkgray")
  (set-face-background 'highlight-indent-guides-even-face "dimgray")
  (set-face-foreground 'highlight-indent-guides-character-face "dimgray")
  )

(use-package! shfmt
  :config
  (add-hook 'sh-mode-hook 'shfmt-on-save-mode)
  )

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

(defun jr/copy-file-path ()
  (interactive)
  (let ((project-root (projectile-project-root))
        root-str
        file-path)
    (if project-root (setq root-str project-root) (setq root-str ""))
    (setq file-path (string-remove-prefix root-str (buffer-file-name)))
    (message "%s" file-path)
    (kill-new file-path))
  )

(defun jr/magit-add-current-buffer-to-kill-ring ()
  "Show the current branch in the echo-area and add it to the `kill-ring'."
  (interactive)
  (let ((branch (magit-get-current-branch)))
    (if branch
        (progn (kill-new branch)
               (message "%s" branch))
      (user-error "There is no current branch")))
  )

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
         (line (number-to-string (line-number-at-pos))))               ; line number at point :contentReference[oaicite:17]{index=17}
    (let ((line-url (concat base "#L" line)))                           ; append line anchor :contentReference[oaicite:18]{index=18}
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

;; Keybindings for GitHub link functions using Doom conventions
(map! :leader
      (:prefix ("g" . "git")
       :desc "Copy GitHub file link" "y" #'jr/copy-github-file-link
       :desc "Copy GitHub file+line link" "Y" #'jr/copy-github-file-line-link))

;; (use-package! treesit-auto
;;   :config

;;   (setq jr/js-tsauto-config
;;         (make-treesit-auto-recipe
;;          :lang 'javascript
;;          :ts-mode 'js-ts-mode
;;          :remap '(js2-mode js-mode javascript-mode rjsx-mode typescript-mode typescript-tsx-mode)
;;          :url "https://github.com/tree-sitter/tree-sitter-javascript"
;;          :revision "f1e5a09"
;;          :source-dir "src"
;;          :ext "\\.js\\'"))

;;   ;; (add-to-list 'treesit-auto-recipe-list jr/js-tsauto-config)

;;   (global-treesit-auto-mode)

;;   )

(use-package! treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  ;; Add all *-ts-modes to `auto-mode-alist'
  (global-treesit-auto-mode 1)
  (treesit-auto-add-to-auto-mode-alist 'all))

(use-package treesit
  :if (and (fboundp 'treesit-available-p) (treesit-available-p))
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.js\\'"  . js-ts-mode)
         ("\\.mjs\\'" . js-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.html\\'" . web-mode)
         ("\\.htm\\'" . web-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ("\\.yml\\'" . yaml-ts-mode)
         ("\\.yaml\\'" . yaml-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
    (when (fboundp 'treesit-available-p)
      (condition-case err
          (dolist (grammar
                   '((css . ("https://github.com/tree-sitter/tree-sitter-css" "v0.20.0"))
                     (bash "https://github.com/tree-sitter/tree-sitter-bash")
                     (html . ("https://github.com/tree-sitter/tree-sitter-html" "v0.20.1"))
                     (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript" "v0.21.2" "src"))
                     (json . ("https://github.com/tree-sitter/tree-sitter-json" "v0.20.2"))
                     (python . ("https://github.com/tree-sitter/tree-sitter-python" "v0.20.4"))
                     (go "https://github.com/tree-sitter/tree-sitter-go" "v0.20.0")
                     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
                     (make "https://github.com/alemuller/tree-sitter-make")
                     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
                     (cmake "https://github.com/uyha/tree-sitter-cmake")
                     (c "https://github.com/tree-sitter/tree-sitter-c")
                     (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
                     (toml "https://github.com/tree-sitter/tree-sitter-toml")
                     (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "tsx/src"))
                     (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "v0.20.3" "typescript/src"))
                     (yaml . ("https://github.com/ikatyang/tree-sitter-yaml" "v0.5.0"))
                     (prisma "https://github.com/victorhqc/tree-sitter-prisma")))
            (condition-case grammar-err
                (progn
                  (add-to-list 'treesit-language-source-alist grammar)
                  ;; Only install `grammar' if we don't already have it
                  ;; installed. However, if you want to *update* a grammar then
                  ;; this obviously prevents that from happening.
                  (unless (treesit-language-available-p (car grammar))
                    (treesit-install-language-grammar (car grammar))))
              (error
               (message "Failed to install tree-sitter grammar %s: %s" (car grammar) grammar-err))))
        (error
         (message "Error setting up tree-sitter grammars: %s" err)))))

  ;; Optional, but recommended. Tree-sitter enabled major modes are
  ;; distinct from their ordinary counterparts.
  ;;
  ;; You can remap major modes with `major-mode-remap-alist'. Note
  ;; that this does *not* extend to hooks! Make sure you migrate them
  ;; also
  (dolist (mapping
           '((python-mode . python-ts-mode)
             (css-mode . css-ts-mode)
             (typescript-mode . typescript-ts-mode)
             (typescript-tsx-mode . tsx-ts-mode)
             (js-mode . js-ts-mode)
             (js2-mode . js-ts-mode)
             (javascript-mode . js-ts-mode)
             (html-mode . web-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (jsonc-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)
             (yaml-mode . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping))
  :config
  (os/setup-install-grammars))

(message "after treeesit-auto...")

(defun jr/dired-untracked (dir)
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
    commit-hash)
  )

(setq ispell-program-name "hunspell")
;; Set the default dictionary
(setq ispell-dictionary "en_US")  ;; Change "en_US" to your preferred dictionary, e.g., "en_GB" for British English

(use-package! jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-C-c" . jinx-correct)
         ("C-M-l" . jinx-languages)))

(use-package! apheleia
  :ensure t
  :config
  (setf (alist-get 'prettier-yaml apheleia-formatters)
        '("apheleia-npx" "prettier" "--stdin-filepath" filepath
          "--parser=yaml" "--single-quote=false"
          (apheleia-formatters-js-indent "--use-tabs" "--tab-width"))
        )
  (setf (alist-get 'typescript-mode apheleia-formatters)
        '("prettier" "--stdin-filepath" input-file))
  (setf (alist-get 'typescript-tsx-mode apheleia-formatters)
        '("prettier" "--stdin-filepath" input-file))

  (setf (alist-get 'prettier-graphql apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser" "graphql"))
  ;; Associate graphql-mode with our formatter
  (add-to-list 'apheleia-mode-alist '(graphql-mode . prettier-graphql))

  ;; HTML formatting with Prettier
  (setf (alist-get 'prettier-html apheleia-formatters)
        '("prettier" "--stdin-filepath" filepath "--parser" "html"
          "--html-whitespace-sensitivity" "strict" "--print-width" "120"))
  ;; Associate HTML modes with prettier formatter
  (add-to-list 'apheleia-mode-alist '(html-mode . prettier-html))
  (add-to-list 'apheleia-mode-alist '(web-mode . prettier-html))

  ;; Associate tree-sitter modes with formatters
  (add-to-list 'apheleia-mode-alist '(typescript-ts-mode . typescript-mode))
  (add-to-list 'apheleia-mode-alist '(tsx-ts-mode . typescript-tsx-mode))
  (add-to-list 'apheleia-mode-alist '(html-ts-mode . prettier-html))
  )

(message "after apheleia config...")

;; (after! apheleia
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (apheleia-mode -1))))

(message "starting SQL configuration section...")

;; SQL in String Formatting Configuration
;; Support for formatting SQL queries within string constants

(message "SQL config: after initial comments...")

;; Polymode configuration for SQL in strings
;; NOTE: Temporarily disabled due to loading issues
;; TODO: Re-enable once polymode package is properly configured

(message "SQL config: before disabled polymode block...")

(when nil ;; Disabled for now
  (message "This should never print")
  (with-eval-after-load 'polymode
    (message "Configuring polymode for SQL strings...")

    ;; Python configuration
    (with-eval-after-load 'python-mode
      ;; Define Python host mode
      (define-hostmode poly-python-hostmode :mode 'python-mode)

      ;; Define SQL inner mode for Python triple-quoted strings
      (define-innermode poly-sql-python-innermode
        :mode 'sql-mode
        :head-matcher (rx (or (seq "\"\"\"" (* space) (or "-- SQL" "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER"))
                              (seq "'''" (* space) (or "-- SQL" "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER"))
                              (seq "sql = \"\"\"")
                              (seq "query = \"\"\"")
                              (seq "SQL = \"\"\"")
                              (seq "QUERY = \"\"\"")))
        :tail-matcher (rx (or "\"\"\"" "'''"))
        :head-mode 'host
        :tail-mode 'host)

      ;; Define the polymode for Python with SQL
      (define-polymode poly-python-sql-mode
        :hostmode 'poly-python-hostmode
        :innermodes '(poly-sql-python-innermode)))

    ;; JavaScript/TypeScript configuration
    (with-eval-after-load 'js-mode
      ;; JavaScript host mode
      (define-hostmode poly-js-hostmode :mode 'js-mode)

      ;; SQL inner mode for template literals
      (define-innermode poly-sql-js-innermode
        :mode 'sql-mode
        :head-matcher (rx "`" (* space) (or "-- SQL" "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER"))
        :tail-matcher "`"
        :head-mode 'host
        :tail-mode 'host)

      ;; Polymode for JS with SQL
      (define-polymode poly-js-sql-mode
        :hostmode 'poly-js-hostmode
        :innermodes '(poly-sql-js-innermode)))

    (with-eval-after-load 'typescript-mode
      ;; TypeScript host mode
      (define-hostmode poly-ts-hostmode :mode 'typescript-mode)

      ;; Polymode for TS with SQL (reusing JS inner mode)
      (define-polymode poly-ts-sql-mode
        :hostmode 'poly-ts-hostmode
        :innermodes '(poly-sql-js-innermode)))))
;; End of disabled polymode block

(message "SQL config: after disabled polymode block...")

(message "SQL config: before edit-indirect...")

;; Edit-indirect configuration for editing SQL strings
(use-package! edit-indirect
  :defer t
  :commands (edit-indirect-region)
  :init
  (message "SQL config: edit-indirect init...")
  :config
  (message "SQL config: edit-indirect config...")
  ;; Custom function to edit SQL in string at point
  (defun jr/edit-sql-string-at-point ()
    "Edit the SQL string at point in a separate buffer with SQL mode."
    (interactive)
    (let* ((string-bounds (bounds-of-thing-at-point 'string))
           (start (if string-bounds (1+ (car string-bounds)) (region-beginning)))
           (end (if string-bounds (1- (cdr string-bounds)) (region-end))))
      (when (or string-bounds (use-region-p))
        (let ((buf (edit-indirect-region start end t)))
          (with-current-buffer buf
            (sql-mode)
            ;; Set up SQL formatting
            (when (fboundp 'sqlformat-on-save-mode)
              (sqlformat-on-save-mode -1)) ; Disable auto-format on save in indirect buffer
            (local-set-key (kbd "C-c C-f") 'jr/format-sql-buffer))))))

  ;; Helper function to format SQL in indirect buffer
  (defun jr/format-sql-buffer ()
    "Format the current SQL buffer."
    (interactive)
    (cond
     ((fboundp 'sqlformat-buffer) (sqlformat-buffer))
     ((fboundp 'format-all-buffer) (format-all-buffer))
     (t (message "No SQL formatter available")))))

(message "after edit-indirect...")

;; SQL formatting configuration
(use-package! sqlformat
  :defer t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :config
  ;; Configure SQL formatter (pgformatter, sqlformat, or sql-formatter)
  ;; Try to detect available formatter
  (cond
   ((executable-find "pg_format")
    (setq sqlformat-command 'pgformatter
          sqlformat-args '("-s" "2" "-g" "-U" "1")))
   ((executable-find "sqlformat")
    (setq sqlformat-command 'sqlformat
          sqlformat-args '("-r" "-k" "upper")))
   ((executable-find "sql-formatter")
    (setq sqlformat-command 'sql-formatter
          sqlformat-args '("-l" "postgresql")))
   (t
    (message "No SQL formatter found. Install pg_format, sqlformat, or sql-formatter"))))

(message "after sqlformat...")

(message "before apheleia SQL config...")

;; Apheleia configuration for SQL
(after! apheleia
  (message "inside apheleia SQL config...")
  ;; Add SQL formatters to apheleia
  (message "Setting up pgformatter...")
  (setf (alist-get 'pgformatter apheleia-formatters)
        '("pg_format" "-"))
  (message "Setting up sqlformat...")
  (setf (alist-get 'sqlformat apheleia-formatters)
        '("sqlformat" "-r" "-k" "upper" "-"))
  (message "Setting up sql-formatter...")
  (setf (alist-get 'sql-formatter apheleia-formatters)
        '("sql-formatter" "-l" "postgresql"))

  (message "Associating SQL mode with formatter...")
  ;; Associate SQL mode with formatter
  (add-to-list 'apheleia-mode-alist
               (cons 'sql-mode
                     (cond
                      ((executable-find "pg_format") 'pgformatter)
                      ((executable-find "sqlformat") 'sqlformat)
                      ((executable-find "sql-formatter") 'sql-formatter))))
  (message "Apheleia SQL config completed"))

(message "after apheleia SQL config setup...")

;; Helper functions for SQL string formatting
(message "defining SQL helper functions...")

(message "defining jr/format-sql-string-at-point...")
(defun jr/format-sql-string-at-point ()
  "Format the SQL string at point."
  (interactive)
  (save-excursion
    (let* ((string-bounds (bounds-of-thing-at-point 'string))
           (start (if string-bounds (1+ (car string-bounds)) (region-beginning)))
           (end (if string-bounds (1- (cdr string-bounds)) (region-end)))
           (sql-text (buffer-substring-no-properties start end))
           (formatted-sql (jr/format-sql-string sql-text)))
      (when formatted-sql
        (delete-region start end)
        (goto-char start)
        (insert formatted-sql)))))

(message "defining jr/format-sql-string...")
(defun jr/format-sql-string (sql-string)
  "Format SQL-STRING using available formatter."
  (with-temp-buffer
    (insert sql-string)
    (sql-mode)
    (cond
     ((fboundp 'sqlformat-buffer)
      (sqlformat-buffer)
      (buffer-string))
     ((fboundp 'format-all-buffer)
      (format-all-buffer)
      (buffer-string))
     (t
      (message "No SQL formatter available")
      nil))))

(message "defining jr/format-all-sql-strings...")
;; Function to detect and format SQL strings in buffer
(defun jr/format-all-sql-strings ()
  "Format all SQL strings in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((sql-pattern (rx (or "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER" "-- SQL"))))
      (while (re-search-forward sql-pattern nil t)
        (when (nth 3 (syntax-ppss)) ; Check if we're in a string
          (jr/format-sql-string-at-point)
          (forward-char))))))

(message "setting up SQL keybindings...")
;; Keybindings for SQL formatting - using "c q" for SQL (query) to avoid conflict
(condition-case err
    (map! :leader
          (:prefix ("c q" . "SQL/Query")
           :desc "Edit SQL string" "e" #'jr/edit-sql-string-at-point
           :desc "Format SQL string" "f" #'jr/format-sql-string-at-point
           :desc "Format all SQL strings" "F" #'jr/format-all-sql-strings))
  (error (message "Error setting up SQL keybindings: %s" err)))

(message "after SQL keybindings...")

;; For Go mode SQL strings
(after! go-mode
  (defun jr/go-format-sql-string ()
    "Format SQL string in Go code, handling Go's backtick strings."
    (interactive)
    (save-excursion
      (let* ((start (save-excursion
                      (re-search-backward "`" nil t)
                      (1+ (point))))
             (end (save-excursion
                    (re-search-forward "`" nil t)
                    (1- (point))))
             (sql-text (when (and start end)
                         (buffer-substring-no-properties start end)))
             (formatted-sql (when sql-text (jr/format-sql-string sql-text))))
        (when formatted-sql
          (delete-region start end)
          (goto-char start)
          (insert formatted-sql)))))

  ;; Add Go-specific keybinding
  (map! :map go-mode-map
        :localleader
        (:prefix ("s" . "SQL")
         :desc "Format SQL string" "f" #'jr/go-format-sql-string)))

;; Enhanced SQL mode configuration
(after! sql
  ;; Set default SQL product to PostgreSQL
  (setq sql-product 'postgres)

  ;; Improve SQL indentation
  (setq sql-indent-offset 2)

  ;; Enable SQL mode for common SQL file extensions
  (add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.plsql\\'" . sql-mode))

  ;; Configure SQL mode hooks
  (add-hook 'sql-mode-hook
            (lambda ()
              ;; Enable better indentation
              (when (fboundp 'sql-indent-mode)
                (sql-indent-mode))
              ;; Show whitespace in SQL files
              (setq show-trailing-whitespace t)
              ;; Enable format on save for SQL files
              (when (fboundp 'sqlformat-on-save-mode)
                (sqlformat-on-save-mode))))

  ;; PostgreSQL specific settings
  (setq sql-postgres-program "psql")
  (setq sql-postgres-options '("-P" "pager=off")))

;; SQL indent configuration
(use-package! sql-indent
  :after sql
  :config
  (setq sql-indent-offset 2)
  ;; Only activate sql-indent-mode in actual SQL buffers, not in other modes
  (add-hook 'sql-mode-hook
            (lambda ()
              (when (eq major-mode 'sql-mode)
                (sql-indent-mode)))))

;; Quick function to test SQL formatting
(defun jr/test-sql-formatter ()
  "Test if SQL formatter is working."
  (interactive)
  (let ((test-sql "SELECT * FROM users WHERE id = 1"))
    (message "Testing SQL formatter...")
    (message "Original: %s" test-sql)
    (message "Formatted: %s" (or (jr/format-sql-string test-sql) "Formatter not available"))))

;; Visual indication for SQL strings
(defface sql-string-face
  '((t :inherit font-lock-string-face :background "#1a1a2e"))
  "Face for SQL strings"
  :group 'sql)

(defun jr/highlight-sql-strings ()
  "Add highlighting for SQL keywords in strings."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\(\"\\|'\\|`\\).*?\\(SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|DROP\\|ALTER\\).*?\\1"
      0 'sql-string-face t))))

;; Enable SQL string highlighting in programming modes
(dolist (mode '(python-mode go-mode js-mode typescript-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            'jr/highlight-sql-strings))

;; Toggle polymode for current buffer
(defun jr/toggle-polymode ()
  "Toggle polymode for SQL strings in current buffer."
  (interactive)
  (cond
   ((eq major-mode 'poly-python-sql-mode)
    (python-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'python-mode)
    (poly-python-sql-mode)
    (message "Polymode enabled for Python/SQL"))
   ((eq major-mode 'poly-js-sql-mode)
    (js-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'js-mode)
    (poly-js-sql-mode)
    (message "Polymode enabled for JS/SQL"))
   ((eq major-mode 'poly-ts-sql-mode)
    (typescript-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'typescript-mode)
    (poly-ts-sql-mode)
    (message "Polymode enabled for TypeScript/SQL"))
   (t
    (message "Polymode not available for %s" major-mode))))

;; Add keybinding for toggling polymode - using "c q" to match other SQL bindings
(condition-case err
    (map! :leader
          (:prefix ("c q" . "SQL/Query")
           :desc "Toggle polymode" "p" #'jr/toggle-polymode))
  (error (message "Error setting up polymode keybinding: %s" err)))

(message "before projectile config...")

(after! projectile
  ;; For Projectile
  (setq projectile-indexing-method 'alien) ; Use external tools like git
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
  (define-key projectile-mode-map (kbd "C-c p I") 'projectile-invalidate-cache)
  )

(message "after projectile config...")

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(message "checking loading komodo folder...")
(when (file-exists-p "~/Repositories/komodo")
  (message "loading komodo folder...")
  (add-to-list 'load-path "~/Repositories/komodo")
  (load-directory "~/Repositories/komodo")
  )

(message "after komodo...")

;; ============================================================================
;; UUID GENERATION
;; ============================================================================

(defun jr/generate-uuidv4 ()
  "Generate a random UUIDv4."
  (let* ((random-bytes (make-string 16 0))
         (i 0))
    ;; Fill with random bytes
    (while (< i 16)
      (aset random-bytes i (random 256))
      (setq i (1+ i)))
    ;; Set version (4) and variant bits
    (aset random-bytes 6 (logior (logand (aref random-bytes 6) #x0f) #x40))
    (aset random-bytes 8 (logior (logand (aref random-bytes 8) #x3f) #x80))
    ;; Format as UUID string
    (format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
            (aref random-bytes 0) (aref random-bytes 1)
            (aref random-bytes 2) (aref random-bytes 3)
            (aref random-bytes 4) (aref random-bytes 5)
            (aref random-bytes 6) (aref random-bytes 7)
            (aref random-bytes 8) (aref random-bytes 9)
            (aref random-bytes 10) (aref random-bytes 11)
            (aref random-bytes 12) (aref random-bytes 13)
            (aref random-bytes 14) (aref random-bytes 15))))

(defun jr/generate-uuidv7 ()
  "Generate a time-ordered UUIDv7 per RFC 9562.
UUIDv7 format: unix_ts_ms (48 bits) + ver (4) + rand_a (12) + var (2) + rand_b (62)."
  (let* (;; Get current time and convert to milliseconds since Unix epoch
         (time (current-time))
         (high (car time))
         (low (cadr time))
         (usec (or (nth 2 time) 0))
         ;; Calculate total seconds: high * 2^16 + low
         (seconds (+ (* high 65536) low))
         ;; Convert to milliseconds
         (timestamp-ms (+ (* seconds 1000) (/ usec 1000)))
         ;; Create 16 bytes for UUID
         (bytes (make-string 16 0)))

    ;; Bytes 0-5: 48-bit timestamp in big-endian format
    (aset bytes 0 (logand (ash timestamp-ms -40) #xFF))
    (aset bytes 1 (logand (ash timestamp-ms -32) #xFF))
    (aset bytes 2 (logand (ash timestamp-ms -24) #xFF))
    (aset bytes 3 (logand (ash timestamp-ms -16) #xFF))
    (aset bytes 4 (logand (ash timestamp-ms -8) #xFF))
    (aset bytes 5 (logand timestamp-ms #xFF))

    ;; Bytes 6-7: version + 12 bits random
    ;; Byte 6: version 7 in high nibble (0111) + 4 bits random
    (aset bytes 6 (logior #x70 (logand (random 256) #x0F)))
    ;; Byte 7: 8 bits random
    (aset bytes 7 (random 256))

    ;; Bytes 8-15: variant + 62 bits random
    ;; Byte 8: variant (10) in high 2 bits + 6 bits random
    (aset bytes 8 (logior #x80 (logand (random 256) #x3F)))
    ;; Bytes 9-15: 7 bytes of random data
    (let ((i 9))
      (while (< i 16)
        (aset bytes i (random 256))
        (setq i (1+ i))))

    ;; Format as UUID string (8-4-4-4-12)
    (format "%02x%02x%02x%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
            (aref bytes 0) (aref bytes 1) (aref bytes 2) (aref bytes 3)
            (aref bytes 4) (aref bytes 5)
            (aref bytes 6) (aref bytes 7)
            (aref bytes 8) (aref bytes 9)
            (aref bytes 10) (aref bytes 11) (aref bytes 12)
            (aref bytes 13) (aref bytes 14) (aref bytes 15))))

(defun jr/insert-uuidv4 ()
  "Insert a UUIDv4 at point."
  (interactive)
  (insert (jr/generate-uuidv4)))

(defun jr/insert-uuidv7 ()
  "Insert a UUIDv7 at point."
  (interactive)
  (insert (jr/generate-uuidv7)))

(defun jr/copy-uuidv4 ()
  "Generate a UUIDv4 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-uuidv4)))
    (kill-new uuid)
    (message "UUIDv4 copied: %s" uuid)))

(defun jr/copy-uuidv7 ()
  "Generate a UUIDv7 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-uuidv7)))
    (kill-new uuid)
    (message "UUIDv7 copied: %s" uuid)))

(defun jr/replace-uuid-at-point-with-v4 ()
  "Replace the UUID at point or in region with a new UUIDv4."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-uuidv4))
              (message "Replaced UUID with UUIDv4"))
          (message "No UUID found at point"))))))

(defun jr/replace-uuid-at-point-with-v7 ()
  "Replace the UUID at point or in region with a new UUIDv7."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-uuidv7))
              (message "Replaced UUID with UUIDv7"))
          (message "No UUID found at point"))))))

;; Readable UUID generation with pronounceable first segment
(defvar jr/readable-words
  '("facade" "decade" "beaded" "accede" "deface" "backed" "baffed" "beefed"
    "cabbed" "cabled" "cached" "caddie" "cafed0" "caged0" "calced" "called"
    "dabbed" "danced" "decaf0" "decode" "deeded" "defeat" "defied" "ebbed0"
    "edged0" "faced0" "faded0" "failed" "falled" "fedde0" "feed00" "abacab"
    "abase0" "abased" "abated" "abbace" "accede" "access" "acedia" "acned0"
    "badass" "baffle" "baffed" "balled" "beaded" "beadle" "beefed" "cabala"
    "cabale" "cabled" "cadded" "caecal" "ceased" "dabble" "dabbed" "decade"
    "decaff" "deface" "defade" "ebbede" "efface" "fabled" "facade" "facede"
    "fadded" "bedbad" "bedded" "beebee" "beefed" "cabbed" "caffee" "canned"
    "deaded" "deaden" "deadbe" "decade" "decaff" "decede" "accede" "aceded"
    "addeda" "baccab" "baffed" "beefed" "caffed" "daffed" "efface" "faccee"
    "abcdef" "acebad" "accede" "badeaf" "baffed" "bedead" "caffed" "decade"
    "defbad" "efface" "fabade" "fadcab" "acefac" "badfad" "bedbad" "cabfed"
    "dabfad" "deface" "efface" "fabbed" "badfac" "bedfac" "caffac" "dadfac"
    "deafac" "efffac" "f00d00" "facade" "baddad" "beef00" "cafe00" "dead00"
    "deaf00" "decaf0" "decade" "feed00" "babe00" "dada00" "fade00" "face00"
    "abad00" "acace0" "added0" "baaed0" "cabba0" "dabba0" "fabba0" "ebb000"
    "acc000" "add000" "aff000" "baa000" "bad000" "bee000" "cab000" "dab000"
    "dad000" "ebb000" "fad000" "fee000" "abba00" "acdc00" "bada00" "bead00"
    "cafe00" "dada00" "dead00" "deaf00" "fade00" "face00" "feed00" "deed00"
    "aaa000" "bbb000" "ccc000" "ddd000" "eee000" "fff000" "aba000" "aca000"
    "ada000" "afa000" "aea000" "bab000" "bac000" "bad000" "cac000" "cad000"
    "dab000" "dac000" "dad000" "fab000" "fac000" "fad000" "ebb000" "ecc000"
    "abcabc" "defdef" "fedcba" "bcdefa" "cdefab" "acabab" "fadede" "bebebe"
    "cecece" "dedede" "efefef" "afafaf" "bababa" "cacaca" "dadada" "fabfab"
    "deface" "beface" "caface" "daface" "efface" "baface" "acface" "adface")
  "List of readable hex-valid words for UUID prefixes (all exactly 6 chars).")

(defun jr/get-random-readable-word ()
  "Get a random readable word from the list."
  (nth (random (length jr/readable-words)) jr/readable-words))

(defun jr/generate-readable-uuidv4 ()
  "Generate a UUIDv4 with a readable first segment."
  (let* ((readable-word (jr/get-random-readable-word))
         (random-bytes (make-string 10 0))
         (i 0))
    ;; Fill remaining random bytes (not including first 8 hex chars)
    (while (< i 10)
      (aset random-bytes i (random 256))
      (setq i (1+ i)))
    ;; Set version (4) and variant bits in CORRECT positions per RFC 9562
    ;; Version must be at position 14 (third group first char) = random-bytes[2]
    ;; Variant must be at position 19 (fourth group first char) = random-bytes[4]
    (aset random-bytes 2 (logior (logand (aref random-bytes 2) #x0f) #x40))
    (aset random-bytes 4 (logior (logand (aref random-bytes 4) #x3f) #x80))
    ;; Format with readable prefix (word is 6 chars, add 2 hex to make 8)
    ;; Format: word+2hex-4hex-4hex-4hex-12hex = 8-4-4-4-12 UUID format
    (format "%s%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
            readable-word
            (random 256)           ; Add 1 byte (2 hex) to make first group 8 chars
            (aref random-bytes 0)  ; Second group: 2 bytes
            (aref random-bytes 1)
            (aref random-bytes 2)  ; Third group: version 4 in high nibble + 4 random bits
            (aref random-bytes 3)
            (aref random-bytes 4)  ; Fourth group: variant 10 in high 2 bits + 6 random bits
            (aref random-bytes 5)
            (aref random-bytes 6)  ; Fifth group: 6 bytes = 12 hex chars
            (aref random-bytes 7)
            (aref random-bytes 8)
            (aref random-bytes 9)
            (random 256)           ; Add 2 more bytes for proper 36-char length
            (random 256))))

(defun jr/generate-readable-uuidv7 ()
  "Generate a UUIDv7 with a readable first segment."
  (let* ((readable-word (jr/get-random-readable-word))
         ;; Create 10 bytes for the non-timestamp part
         (bytes (make-string 10 0)))

    ;; Fill all 10 bytes with random data first
    (let ((i 0))
      (while (< i 10)
        (aset bytes i (random 256))
        (setq i (1+ i))))

    ;; Set version (7) and variant bits in CORRECT positions per RFC 9562
    ;; Version must be at position 14 (third group first char) = bytes[2]
    ;; Variant must be at position 19 (fourth group first char) = bytes[4]
    ;; Byte 2: version 7 in high nibble + 4 bits random
    (aset bytes 2 (logior #x70 (logand (aref bytes 2) #x0F)))
    ;; Byte 4: variant (10) in high 2 bits + 6 bits random
    (aset bytes 4 (logior #x80 (logand (aref bytes 4) #x3F)))

    ;; Format with readable prefix (word is 6 chars, add 2 hex to make 8)
    ;; Format: word+2hex-4hex-4hex-4hex-12hex = 8-4-4-4-12 UUID format
    (format "%s%02x-%02x%02x-%02x%02x-%02x%02x-%02x%02x%02x%02x%02x%02x"
            readable-word
            (random 256)           ; Add 1 byte (2 hex) to make first group 8 chars
            (aref bytes 0) (aref bytes 1)
            (aref bytes 2) (aref bytes 3)
            (aref bytes 4) (aref bytes 5)
            (aref bytes 6) (aref bytes 7) (aref bytes 8) (aref bytes 9)
            (random 256) (random 256))))

(defun jr/insert-readable-uuidv4 ()
  "Insert a readable UUIDv4 at point."
  (interactive)
  (insert (jr/generate-readable-uuidv4)))

(defun jr/insert-readable-uuidv7 ()
  "Insert a readable UUIDv7 at point."
  (interactive)
  (insert (jr/generate-readable-uuidv7)))

(defun jr/copy-readable-uuidv4 ()
  "Generate a readable UUIDv4 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-readable-uuidv4)))
    (kill-new uuid)
    (message "Readable UUIDv4 copied: %s" uuid)))

(defun jr/copy-readable-uuidv7 ()
  "Generate a readable UUIDv7 and copy it to the kill ring."
  (interactive)
  (let ((uuid (jr/generate-readable-uuidv7)))
    (kill-new uuid)
    (message "Readable UUIDv7 copied: %s" uuid)))

(defun jr/replace-uuid-at-point-with-readable-v4 ()
  "Replace the UUID at point or in region with a new readable UUIDv4."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-readable-uuidv4))
              (message "Replaced UUID with readable UUIDv4"))
          (message "No UUID found at point"))))))

(defun jr/replace-uuid-at-point-with-readable-v7 ()
  "Replace the UUID at point or in region with a new readable UUIDv7."
  (interactive)
  (let* ((uuid-regex "[0-9a-f]\\{8\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{4\\}-[0-9a-f]\\{12\\}")
         (bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (save-excursion
                     (let ((start (progn
                                    (re-search-backward "[^0-9a-f-]" nil t)
                                    (forward-char)
                                    (point)))
                           (end (progn
                                  (re-search-forward "[^0-9a-f-]" nil t)
                                  (backward-char)
                                  (point))))
                       (cons start end))))))
    (when bounds
      (let ((text (buffer-substring-no-properties (car bounds) (cdr bounds))))
        (if (string-match-p uuid-regex text)
            (progn
              (delete-region (car bounds) (cdr bounds))
              (goto-char (car bounds))
              (insert (jr/generate-readable-uuidv7))
              (message "Replaced UUID with readable UUIDv7"))
          (message "No UUID found at point"))))))

(message "after UUID generation...")

;; Keybindings for UUID generation (placed after function definitions)
;; Using SPC o u (open → UUID) to avoid conflict with SPC i u (insert unicode)
(map! :leader
      (:prefix ("o" . "open")
               (:prefix ("u" . "UUID")
                :desc "Insert UUIDv4" "4" #'jr/insert-uuidv4
                :desc "Insert UUIDv7" "7" #'jr/insert-uuidv7
                (:prefix ("R" . "readable")
                 :desc "Insert readable UUIDv4" "4" #'jr/insert-readable-uuidv4
                 :desc "Insert readable UUIDv7" "7" #'jr/insert-readable-uuidv7)
                (:prefix ("y" . "copy/yank")
                 :desc "Copy UUIDv4" "4" #'jr/copy-uuidv4
                 :desc "Copy UUIDv7" "7" #'jr/copy-uuidv7
                 (:prefix ("R" . "readable")
                  :desc "Copy readable UUIDv4" "4" #'jr/copy-readable-uuidv4
                  :desc "Copy readable UUIDv7" "7" #'jr/copy-readable-uuidv7))
                (:prefix ("r" . "replace")
                 :desc "Replace with UUIDv4" "4" #'jr/replace-uuid-at-point-with-v4
                 :desc "Replace with UUIDv7" "7" #'jr/replace-uuid-at-point-with-v7
                 (:prefix ("R" . "readable")
                  :desc "Replace with readable UUIDv4" "4" #'jr/replace-uuid-at-point-with-readable-v4
                  :desc "Replace with readable UUIDv7" "7" #'jr/replace-uuid-at-point-with-readable-v7)))))

(message "after UUID keybindings...")

(use-package! gptel
  :config
                                        ; (setq! gptel-api-key "your key")
  (gptel-make-gh-copilot "Copilot")
  )

;; ============================================================================
;; AI COMMIT MESSAGE GENERATION
;; ============================================================================

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
                      (message "Failed to generate commit message: %s" info))))))
  )

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
                      (message "Failed to generate commit message: %s" info))))))
  )

;; Claude Code commit message generation functions
(defun jr/claude-code-generate-commit-message ()
  "Generate a commit message using Claude Code based on the current git diff."
  (interactive)
  (if (not (executable-find "claude"))
      (progn
        (message "Claude Code not found. Falling back to gptel.")
        (jr/ai-generate-commit-message))
    (let* ((diff-output (shell-command-to-string "git diff --staged"))
           (prompt (format "Generate a concise git commit message for these changes. Output ONLY the commit message text, nothing else. No explanations, no quotes, no formatting. Focus on what was changed and why. Keep it under 72 characters for the first line. If you need multiple lines, add a blank line after the first line and then continue with more details.\n\nChanges:\n%s" diff-output)))
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
               ;; Split output into lines and filter out nix warnings
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
           (prompt (format "Generate a concise git commit message for these changes. Output ONLY the commit message text, nothing else. No explanations, no quotes, no formatting. Focus on what was changed and why. Keep it under 72 characters for the first line. If you need multiple lines, add a blank line after the first line and then continue with more details.\n\nChanges:\n%s" diff-output)))
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
               ;; Split output into lines and filter out nix warnings
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

;; Add keybindings for AI commit message generation
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

(message "after gptel...")

;; ============================================================================
;; EMAIL CONFIGURATION WITH MU4E
;; ============================================================================

;; (after! mu4e
;;   ;; Basic mu4e configuration
;;   (setq mu4e-maildir "~/mail"
;;         mu4e-attachment-dir "~/Downloads"
;;         mu4e-get-mail-command "mbsync -a"
;;         mu4e-update-interval 300  ; Update every 5 minutes
;;         mu4e-compose-context-policy 'ask
;;         mu4e-context-policy 'pick-first
;;         mu4e-view-prefer-html t
;;         mu4e-compose-format-flowed t
;;         mu4e-change-filenames-when-moving t
;;         mu4e-headers-include-related nil
;;         mu4e-headers-skip-duplicates t
;;         ;; Configure sending mail
;;         message-send-mail-function 'message-send-mail-with-sendmail
;;         sendmail-program (executable-find "msmtp")
;;         message-sendmail-extra-arguments '("--read-envelope-from")
;;         message-sendmail-f-is-evil t
;;         ;; Don't keep message buffers around
;;         message-kill-buffer-on-exit t)

;;   ;; Gmail account context
;;   (setq mu4e-contexts
;;         `(,(make-mu4e-context
;;             :name "Gmail"
;;             :match-func (lambda (msg)
;;                           (when msg
;;                             (mu4e-message-contact-field-matches msg :to "jon@join.build")))
;;             :vars '((user-mail-address . "jon@join.build")
;;                     (user-full-name . "Jonathan Rothberg")
;;                     (mu4e-sent-folder . "/[Gmail]/Sent Mail")
;;                     (mu4e-drafts-folder . "/[Gmail]/Drafts")
;;                     (mu4e-trash-folder . "/[Gmail]/Trash")
;;                     (mu4e-refile-folder . "/[Gmail]/All Mail")))))

;;   ;; Bookmarks for common searches
;;   (setq mu4e-bookmarks
;;         '((:name "Unread messages" :query "flag:unread AND NOT flag:trashed" :key ?u)
;;           (:name "Today's messages" :query "date:today..now" :key ?t)
;;           (:name "Last 7 days" :query "date:7d..now" :key ?w)
;;           (:name "Messages with attachments" :query "flag:attach" :key ?a))))

;; ;; Email alerts and notifications
;; (use-package! mu4e-alert
;;   :after mu4e
;;   :config
;;   (mu4e-alert-enable-mode-line-display)
;;   (mu4e-alert-enable-notifications)
;;   (setq mu4e-alert-email-notification-types '(subjects))
;;   (mu4e-alert-set-default-style 'libnotify))

;; ;; ============================================================================
;; ;; GOOGLE CALENDAR INTEGRATION WITH ORG-GCAL
;; ;; ============================================================================

;; ;; Configure auth-source to use pass
;; (require 'auth-source-pass)
;; (auth-source-pass-enable)

;; ;; Set org-gcal variables early to prevent warnings
;; ;; Use shell command to get secret from pass since auth-source-pass may not be fully initialized yet
;; (setq org-gcal-client-id "382762621855-6kflrt6kdl7fel022mqq0ac66lg0aftg.apps.googleusercontent.com"
;;       org-gcal-client-secret (string-trim (shell-command-to-string "pass gcal/client-secret 2>/dev/null"))
;;       org-gcal-fetch-file-alist '(("jon@join.build" . "~/org/gcal.org"))
;;       org-gcal-notify-p t
;;       org-gcal-remove-cancelled-events t
;;       ;; Use external browser for OAuth
;;       browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "xdg-open"
;;       ;; Set token storage location
;;       org-gcal-token-file (expand-file-name "~/.cache/org-gcal/.org-gcal-token")
;;       ;; Disable oauth2-auto to avoid the plstore issue
;;       org-gcal-use-oauth2-auto nil)

;; ;; Prevent oauth2-auto from interfering
;; (with-eval-after-load 'org-gcal
;;   (advice-add 'org-gcal--get-token :around
;;               (lambda (orig-fun &rest args)
;;                 "Force org-gcal to use standard token mechanism."
;;                 (let ((org-gcal-use-oauth2-auto nil))
;;                   (apply orig-fun args)))))

;; (use-package! org-gcal
;;   :after org
;;   :init
;;   ;; Create cache directory for tokens
;;   (let ((cache-dir (expand-file-name "~/.cache/org-gcal")))
;;     (unless (file-exists-p cache-dir)
;;       (make-directory cache-dir t)))
;;   :config
;;   ;; Force org-gcal to use its own token storage, not oauth2-auto
;;   (setq org-gcal-use-oauth2-auto nil)
;;   ;; Ensure we're using the standard org-gcal token mechanism
;;   (setq org-gcal-token-file (expand-file-name "~/.cache/org-gcal/.org-gcal-token"))

;;   ;; Verify configuration
;;   (if (and org-gcal-client-id
;;            org-gcal-client-secret
;;            (not (string-empty-p org-gcal-client-secret)))
;;       (progn
;;         ;; Automatic sync hooks
;;         (add-hook 'org-agenda-mode-hook 'org-gcal-fetch)
;;         (add-hook 'org-capture-after-finalize-hook 'org-gcal-sync)
;;         (message "org-gcal configured successfully with client secret from pass"))
;;     (message "org-gcal: Failed to load client secret from pass")))

;; ;; Calendar capture template
;; (after! org-capture
;;   (add-to-list 'org-capture-templates
;;                '("c" "Calendar Event" entry (file "~/org/gcal.org")
;;                  "* %?\n:PROPERTIES:\n:calendar-id: jon@join.build\n:END:\n:org-gcal:\n%^T--%^T\n:END:\n\n" :empty-lines 1))
;;   (add-to-list 'org-capture-templates
;;                '("e" "Email TODO" entry (file "~/org/inbox.org")
;;                  "* TODO %:subject\n:PROPERTIES:\n:EMAIL: %:from\n:DATE: %:date\n:END:\n\n%a\n\n%?" :empty-lines 1)))

;; ;; ============================================================================
;; ;; EMAIL AND CALENDAR KEYBINDINGS
;; ;; ============================================================================

;; (map! :leader
;;       (:prefix ("m" . "mail/calendar")
;;        :desc "Open mu4e" "m" #'mu4e
;;        :desc "Compose email" "c" #'mu4e-compose-new
;;        :desc "Update mail" "u" #'mu4e-update-mail-and-index
;;        :desc "Search email" "s" #'mu4e-search
;;        :desc "Jump to maildir" "j" #'mu4e-headers-search-bookmark
;;        :desc "Sync calendar" "S" #'org-gcal-sync
;;        :desc "Fetch calendar" "f" #'org-gcal-fetch
;;        :desc "Post to calendar" "p" #'org-gcal-post-at-point))

;; Integration with org-agenda
(after! org-agenda
  (setq org-agenda-files (append org-agenda-files '("~/org/gcal.org"))))

(message "after email and calendar setup...")

(use-package elysium
  :config
  :custom
  ;; Below are the default values
  (elysium-window-size 0.33) ; The elysium buffer will be 1/3 your screen
  (elysium-window-style 'vertical)
  ) ; Can be customized to horizontal

(use-package direnv
  :config
  (direnv-mode))

;; ============================================================================
;; FLAKE.NIX SUPPORT
;; ============================================================================
;; Enhanced Nix flake support for automatic environment management

;; Automatic .envrc creation for flake.nix projects
(defun jr/create-envrc-for-flake ()
  "Create a .envrc file for flake.nix if it doesn't exist."
  (interactive)
  (let ((flake-file (expand-file-name "flake.nix" (projectile-project-root)))
        (envrc-file (expand-file-name ".envrc" (projectile-project-root))))
    (when (and (file-exists-p flake-file)
               (not (file-exists-p envrc-file)))
      (with-temp-file envrc-file
        (insert "use flake\n"))
      (message "Created .envrc file for flake.nix project"))))

;; Hook to automatically create .envrc when opening flake.nix projects
(defun jr/flake-project-setup ()
  "Setup flake.nix project when entering."
  (when (and (projectile-project-p)
             (file-exists-p (expand-file-name "flake.nix" (projectile-project-root))))
    (jr/create-envrc-for-flake)
    ;; Allow direnv if .envrc was just created
    (when (and (fboundp 'direnv-allow)
               (file-exists-p (expand-file-name ".envrc" (projectile-project-root))))
      (direnv-allow))))

;; Add hook for projectile mode
(add-hook 'projectile-after-switch-project-hook #'jr/flake-project-setup)

;; Enhanced nix-shell support with flake awareness
(use-package! nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

;; Function to enter flake development shell
(defun jr/nix-develop ()
  "Enter nix develop shell for current flake project."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (if (file-exists-p "flake.nix")
        (progn
          (message "Entering nix develop shell...")
          (vterm-send-string "nix develop\n"))
      (message "No flake.nix found in project root"))))

;; Function to update flake inputs
(defun jr/nix-flake-update ()
  "Update nix flake inputs."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (if (file-exists-p "flake.nix")
        (compile "nix flake update")
      (message "No flake.nix found in project root"))))

;; Function to check flake
(defun jr/nix-flake-check ()
  "Check nix flake."
  (interactive)
  (let ((default-directory (or (projectile-project-root) default-directory)))
    (if (file-exists-p "flake.nix")
        (compile "nix flake check")
      (message "No flake.nix found in project root"))))

;; Enhanced environment variable management for flakes
(defun jr/refresh-flake-env ()
  "Refresh environment variables from flake develop shell."
  (interactive)
  (when (and (projectile-project-p)
             (file-exists-p (expand-file-name "flake.nix" (projectile-project-root))))
    (direnv-update-environment)
    (jr/nix-post-activate-check)
    (message "Refreshed flake environment")))

;; Function to show current Nix environment status
(defun jr/nix-env-status ()
  "Show current Nix environment status."
  (interactive)
  (let* ((project-root (or (projectile-project-root) default-directory))
         (has-flake (file-exists-p (expand-file-name "flake.nix" project-root)))
         (has-envrc (file-exists-p (expand-file-name ".envrc" project-root)))
         (in-direnv (getenv "DIRENV_DIR"))
         (nix-store-path (getenv "NIX_STORE")))
    (with-current-buffer (get-buffer-create "*Nix Environment Status*")
      (erase-buffer)
      (insert "=== Nix Environment Status ===\n\n")
      (insert (format "Project root: %s\n" project-root))
      (insert (format "Has flake.nix: %s\n" (if has-flake "Yes" "No")))
      (insert (format "Has .envrc: %s\n" (if has-envrc "Yes" "No")))
      (insert (format "In direnv environment: %s\n" (if in-direnv "Yes" "No")))
      (when in-direnv
        (insert (format "Direnv directory: %s\n" in-direnv)))
      (insert "\n=== Key Environment Variables ===\n")
      (dolist (var '("PATH" "NIX_PATH" "NIX_STORE" "NODE_PATH" "PYTHONPATH"))
        (let ((value (getenv var)))
          (when value
            (insert (format "\n%s:\n  %s\n" var (replace-regexp-in-string ":" "\n  " value))))))
      (display-buffer (current-buffer)))
    (message "Nix environment status displayed")))

;; Keybindings for flake operations
(map! :leader
      (:prefix ("n" . "nix")
       :desc "Enter nix develop" "d" #'jr/nix-develop
       :desc "Update flake" "u" #'jr/nix-flake-update
       :desc "Check flake" "c" #'jr/nix-flake-check
       :desc "Refresh flake env" "r" #'jr/refresh-flake-env
       :desc "Create .envrc for flake" "e" #'jr/create-envrc-for-flake
       :desc "Show env status" "s" #'jr/nix-env-status))

;; Auto-refresh environment when saving flake.nix
(defun jr/flake-nix-save-hook ()
  "Hook to run when saving flake.nix files."
  (when (string-match-p "flake\\.nix$" (buffer-file-name))
    (message "Flake.nix saved. Run 'SPC n r' to refresh environment if needed.")))

(add-hook 'after-save-hook #'jr/flake-nix-save-hook)

;; Better integration with existing nix-shell configuration
(after! nix-shell
  ;; Prefer flakes when available
  (defadvice! jr/prefer-flake-develop (orig-fun &rest args)
    "Use 'nix develop' instead of 'nix-shell' when flake.nix exists."
    :around #'nix-shell-command
    (if (file-exists-p (expand-file-name "flake.nix" (or (projectile-project-root) default-directory)))
        (let ((nix-shell-command "nix develop -c"))
          (apply orig-fun args))
      (apply orig-fun args))))

;;; ~/.doom.d/config.el
(setq org-export-in-background nil)

;; 3. (OPTIONAL) DEFINE MACROS FOR COMMON DIRECTIVES
                                        ; (after! org
                                        ;   (setq org-export-global-macros
                                        ;         '(("pause" . "@@html:@@")
                                        ;           ("reset_layout" . "@@html:@@"))))

                                        ; (use-package! claude-code
                                        ;   :bind
                                        ;   ("C-c c" . claude-code-command-map)
                                        ;   :config
                                        ;   (claude-code-mode)
                                        ;
                                        ;   (add-to-list 'display-buffer-alist
                                        ;                '("^\\*claude"
                                        ;                  (display-buffer-in-side-window)
                                        ;                  (side . right)
                                        ;                  (window-width . 0.33)))
                                        ;   )
                                        ;
                                        ; (use-package! emacs-claude-code
                                        ;   :config
                                        ;
                                        ;   (setq --ecc-auto-response-responses
                                        ;         '((:y/n . "1")                              ; Respond "1" to Y/N prompts
                                        ;           (:y/y/n . "2")                            ; Respond "2" to Y/Y/N prompts
                                        ;           (:waiting . "/user:auto")                 ; Send /user:auto when waiting
                                        ;           (:initial-waiting . "/user:understand-guidelines"))) ; Initial waiting response
                                        ;
                                        ;   ;; Enable useful features
                                        ;   (ecc-auto-periodical-toggle)                  ; Enable auto-periodical commands
                                        ;   (--ecc-vterm-utils-enable-yank-advice)        ; Enable yank-as-file for large content
                                        ;
                                        ;   ;; Fine-tune behavior (optional)
                                        ;   (setq --ecc-vterm-yank-as-file-threshold 100)    ; Prompt threshold for yank-as-file
                                        ;   (setq --ecc-auto-response-periodic-interval 300) ; 5 minutes periodic return
                                        ;   (setq ecc-auto-periodical-commands              ; Commands to run periodically
                                        ;         '((10 . "/compact")                            ; Run /compact every 10 interactions
                                        ;           (20 . "/user:auto")))                        ; Run /user:auto every 20 interactions
                                        ;   )

;;; Terminal Management Configuration
;; Enhanced terminal management for staying within Emacs

;; Better vterm-toggle configuration
(use-package! vterm-toggle
  :after vterm projectile
  :config
  (setq vterm-toggle-fullscreen-p nil)
  (setq vterm-toggle-scope 'projectile)
  (setq vterm-toggle-projectile-root t)
  (setq vterm-toggle-reset-window-configration-after-exit nil)
  :bind
  (("C-c t" . vterm-toggle)
   ("C-c T" . vterm-toggle-cd)
   :map vterm-mode-map
   ("s-n" . vterm-toggle-forward)
   ("s-p" . vterm-toggle-backward)))

;; Add multi-vterm for managing multiple terminals
(use-package! multi-vterm
  :after vterm
  :config
  (setq multi-vterm-buffer-name "vterm")
  :bind
  (("C-c v n" . multi-vterm)
   ("C-c v p" . multi-vterm-prev)
   ("C-c v j" . multi-vterm-next)
   ("C-c v d" . multi-vterm-dedicated-toggle)
   ("C-c v r" . multi-vterm-rename-buffer)))

;; Configure tab-bar-mode for workspace management
(use-package! tab-bar
  :config
  (setq tab-bar-show 1)
  (setq tab-bar-close-button-show nil)
  (setq tab-bar-new-tab-choice "*vterm*")
  (setq tab-bar-tab-hints t)
  (setq tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-mode 1)
  :bind
  (("C-x t n" . tab-bar-new-tab)
   ("C-x t 0" . tab-bar-close-tab)
   ("C-x t o" . tab-bar-switch-to-tab)
   ("C-x t r" . tab-bar-rename-tab)
   ("C-x t m" . tab-bar-move-tab)))

;; Terminal-specific display rules
;; Configure how terminal windows should be displayed
(after! vterm
  ;; Bottom terminal popup
  (set-popup-rule! "^\\*vterm"
    :side 'bottom
    :size 0.30
    :select t
    :quit nil
    :ttl nil
    :modeline t)

  ;; Dedicated terminal on the right side
  (set-popup-rule! "^\\*vterm-dedicated"
    :side 'right
    :size 0.40
    :select t
    :quit nil
    :ttl nil
    :modeline t)

  ;; Multi-vterm buffers
  (set-popup-rule! "^\\*vterm\\*<[0-9]+>"
    :side 'bottom
    :size 0.30
    :select t
    :quit nil
    :ttl nil
    :modeline t))

;; Terminal workflow functions
;; Custom functions for efficient terminal management

(defun my/vterm-in-project ()
  "Open vterm in current project root."
  (interactive)
  (let ((default-directory (projectile-project-root)))
    (multi-vterm)))

(defun my/terminal-workspace ()
  "Create or switch to a terminal workspace using tab-bar."
  (interactive)
  (if (member "terminals" (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))
      (tab-bar-select-tab-by-name "terminals")
    (tab-bar-new-tab)
    (tab-bar-rename-tab "terminals")
    (delete-other-windows)
    (multi-vterm)
    (split-window-right)
    (multi-vterm)))

(defun my/vterm-send-region-or-current-line ()
  "Send the current line or selected region to vterm."
  (interactive)
  (let ((content (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'line t))))
    (with-current-buffer (get-buffer "*vterm*")
      (vterm-send-string content))))

(defun my/cycle-through-terminals ()
  "Cycle through all vterm buffers."
  (interactive)
  (let ((vterm-buffers (seq-filter (lambda (buf)
                                     (string-match-p "\\*vterm" (buffer-name buf)))
                                   (buffer-list))))
    (when vterm-buffers
      (switch-to-buffer (car (last vterm-buffers))))))

(defun my/close-all-terminals ()
  "Close all vterm buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (when (string-match-p "\\*vterm" (buffer-name buffer))
      (kill-buffer buffer))))

;; Bind the workflow functions
(map! :leader
      (:prefix ("v" . "terminals")
       :desc "Open project terminal" "p" #'my/vterm-in-project
       :desc "Terminal workspace" "w" #'my/terminal-workspace
       :desc "Send to terminal" "s" #'my/vterm-send-region-or-current-line
       :desc "Cycle terminals" "c" #'my/cycle-through-terminals
       :desc "Close all terminals" "x" #'my/close-all-terminals))

;; Integration with eyebrowse workspaces
(after! eyebrowse
  (defun my/eyebrowse-terminal-config ()
    "Setup terminal configuration for current eyebrowse workspace."
    (interactive)
    (eyebrowse-create-window-config)
    (delete-other-windows)
    (multi-vterm)
    (split-window-horizontally)
    (multi-vterm)
    (balance-windows))

  (defun my/eyebrowse-switch-to-terminal-workspace ()
    "Switch to or create a terminal-focused eyebrowse workspace."
    (interactive)
    (let ((terminal-workspace-exists nil)
          (current-slot (eyebrowse--get 'current-slot)))
      ;; Check if we have a terminal workspace
      (dolist (window-config (eyebrowse--get 'window-configs))
        (when (string-match-p "terminals" (or (cdr (assoc 'tag (cdr window-config))) ""))
          (setq terminal-workspace-exists (car window-config))))
      (if terminal-workspace-exists
          (eyebrowse-switch-to-window-config terminal-workspace-exists)
        ;; Create new terminal workspace
        (eyebrowse-create-window-config)
        (eyebrowse-rename-window-config (eyebrowse--get 'current-slot) "terminals")
        (my/eyebrowse-terminal-config))))

  ;; Add keybinding for terminal workspace
  (map! :leader
        :desc "Terminal eyebrowse workspace" "v e" #'my/eyebrowse-switch-to-terminal-workspace))

;; Popper configuration for better popup management
(use-package! popper
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*vterm.*\\*"
          "\\*eshell.*\\*"
          "\\*shell.*\\*"
          "\\*term.*\\*"
          "\\*ansi-term.*\\*"
          "\\*Messages\\*"
          "\\*Warnings\\*"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Help\\*"))
  (popper-mode +1)
  (popper-echo-mode +1))

;; Optional: eat terminal as fallback
(use-package! eat
  :config
  (setq eat-kill-buffer-on-exit t)
  :hook
  (eshell-mode . eat-eshell-mode))

(defun jr/magit-files-changed-vs-master (&optional ref)
  "Show files changed on current branch vs REF (default: master) using merge-base (three dots)."
  (interactive)
  (let* ((ref (or ref "origin/master"))
         (default-directory (or (ignore-errors (magit-toplevel))
                                default-directory))
         (files (magit-git-lines "diff" "--name-only" (format "%s...HEAD" ref))))
    (with-current-buffer (get-buffer-create "*Changed files vs master*")
      (erase-buffer)
      (insert (mapconcat #'identity files "\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(defun jr/git-diff-file-at-point-vs-master (&optional ref)
  "Show git diff of file at point compared to REF (default: origin/master).
Works with file paths under cursor in any context (dired, magit, regular buffers)."
  (interactive)
  (require 'magit)
  (let* ((ref (or ref "origin/master"))
         (git-root (magit-toplevel))
         (file-path (or
                     ;; Try to get file from various contexts
                     (and (eq major-mode 'dired-mode)
                          (dired-get-filename nil t))
                     (and (derived-mode-p 'magit-mode)
                          (magit-file-at-point))
                     ;; Get file path from thing-at-point
                     (thing-at-point 'filename t)
                     ;; Current buffer file
                     (buffer-file-name))))
    (if (not git-root)
        (user-error "Not in a git repository")
      (if (not file-path)
          (user-error "No file path found at point")
        ;; Make file-path absolute if it's relative
        (let* ((absolute-path (expand-file-name file-path default-directory))
               ;; Get path relative to git root
               (relative-path (file-relative-name absolute-path git-root))
               (default-directory git-root))
          (if (not (file-exists-p absolute-path))
              (user-error "File does not exist: %s" absolute-path)
            ;; Use diff-mode with proper colors and dedicated window parameter for quit
            (let* ((buffer-name (format "*diff: %s vs %s*" relative-path ref))
                   (diff-command (format "git diff --no-ext-diff %s -- %s"
                                         (shell-quote-argument ref)
                                         (shell-quote-argument relative-path))))
              (with-current-buffer (get-buffer-create buffer-name)
                (let ((inhibit-read-only t))
                  (erase-buffer)
                  (insert (shell-command-to-string diff-command))
                  (if (= (buffer-size) 0)
                      (progn
                        (kill-buffer)
                        (user-error "No differences found for %s vs %s" relative-path ref))
                    (diff-mode)
                    (setq-local font-lock-defaults diff-font-lock-defaults)
                    (font-lock-mode 1)
                    (font-lock-flush)
                    (font-lock-ensure (point-min) (point-max))
                    (goto-char (point-min))
                    (setq buffer-read-only t)
                    ;; Add evil mode keybinding for 'q'
                    (evil-local-set-key 'normal (kbd "q")
                                        (lambda ()
                                          (interactive)
                                          (quit-window t))))))
              ;; Display buffer with quit-window compatible settings
              (let* ((diff-buf (get-buffer buffer-name))
                     (original-window (selected-window)))
                ;; Check if already displayed
                (if-let ((existing-window (get-buffer-window diff-buf)))
                    (select-window existing-window)
                  ;; Create new window on the right
                  (let ((new-window (split-window original-window nil 'right)))
                    ;; Mark the window as dedicated and set quit-restore parameter
                    (set-window-buffer new-window diff-buf)
                    (set-window-parameter new-window 'quit-restore
                                          (list 'window 'window original-window diff-buf))
                    (set-window-dedicated-p new-window t)
                    (select-window new-window))))
              (message "Showing diff for %s vs %s" relative-path ref))))))))

(defun my/magit-diff-file-at-point-vs-origin-master ()
  "Open a Magit diff for the file at point: origin/master...HEAD, limited to that file."
  (interactive)
  (require 'magit)
  (let* ((root (or (ignore-errors (magit-toplevel))
                   (vc-root-dir)
                   (locate-dominating-file default-directory ".git")
                   (user-error "Not inside a Git repo")))
         (raw  (or (thing-at-point 'filename t)
                   (string-trim (buffer-substring (line-beginning-position)
                                                  (line-end-position)))))
         (abs  (if (file-name-absolute-p raw) raw (expand-file-name raw root)))
         (rel  (file-relative-name abs root))
         (range "origin/master...HEAD"))
    (let ((default-directory root))
      ;; Produce a proper Magit diff buffer with pathspec
      (magit-diff-range range nil (list rel))
      ;; Optional niceties
      (setq-local magit-diff-refine-hunk 'all)
      (magit-refresh))))

;; Optional keybinding for your “list of files” major-mode:
;; (map! :map my-files-list-mode-map "D" #'my/magit-diff-file-at-point-vs-origin-master)

(message "done loading config.el...")

(provide 'config)
;;; config.el
