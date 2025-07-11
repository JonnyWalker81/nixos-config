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

(if (eq system-type 'darwin)
    (progn
      (exec-path-from-shell-copy-env "GOPATH")
      (exec-path-from-shell-copy-env "RUST_SRC_PATH")
      (exec-path-from-shell-copy-env "RUSTUP_HOME")
      (exec-path-from-shell-copy-env "CARGO_HOME")))

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
      user-mail-address "jon@geneva.com")

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

(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))))

(setq doom-line-numbers-style 'relative)
(setq nlinum-highlight-current-line t)

(after! magit
  (evil-collection-init 'magit)
  (setq git-commit-summary-max-length 72)
 )

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

doom-font (font-spec :family "JetBrains Mono" :size 5)

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
  ;;  (require 'dap-go)		; download and expand vscode-go-extenstion to the =~/.extensions/go=
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

(use-package! go-mode
  :config
  (setq gofmt-command "goimports")
  )

(after! lsp-go
  (setq lsp-format-buffer-on-save nil    ; disable LSP’s formatter on save
        lsp-go-format-tool    "goimports")) ; tell gopls to use goimports

(set-formatter! 'gofmt '("goimports") :modes '(go-ts-mode))

(after! go-mode
  ;; 1. Use goimports instead of gofmt
  (setq gofmt-command "goimports")                             ; :contentReference[oaicite:2]{index=2}
  ;; 2. Format buffer (and update imports) on save, buffer-local
  (add-hook 'go-mode-hook
            (lambda ()
              (add-hook 'before-save-hook #'gofmt-before-save nil t))))

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

(use-package! gleam-mode
  :bind (:map gleam-mode-map
              ("C-c g f" . gleam-format)))

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

;; (after! apheleia
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (apheleia-mode -1))))

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

;; Add keybindings for AI commit message generation
(map! :leader
      (:prefix ("g" . "git")
       :desc "Generate AI commit message" "m" #'jr/ai-generate-commit-message))

;; Add magit-specific keybinding
(after! magit
  (transient-append-suffix 'magit-commit "c"
    '("m" "AI commit message" jr/ai-insert-commit-message))

  ;; Add keybinding in commit message buffer
  (define-key git-commit-mode-map (kbd "C-c C-a") #'jr/ai-insert-commit-message))

(message "after gptel...")

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

(message "done loading config.el...")

(provide 'config)
;;; config.el
