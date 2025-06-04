;;-*- lexical-binding: t -*-
;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(message "loading...")
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
    ;; something for OS X if true
    ;; optional something if not
    (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  )

(if (eq system-type 'linux)
    ;; something for OS X if true
    ;; optional something if not
    (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
  )

(add-to-list 'treesit-extra-load-path "~/.tree-sitter/bin")

(load! "config-misc")
(load! "config-font")
(load! "config-bindings")

(require 'vc-git)

(require 'ox-reveal)

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
(set-fill-column 120)

(message "before lsp-mode...")

;; (add-hook 'go-mode-hook #'lsp-deferred)
(after! lsp-mode
  (add-to-list 'lsp--formatting-indent-alist '(typescript-tsx-mode . typescript-indent-level))
  (add-to-list 'lsp--formatting-indent-alist '(tsx-ts-mode . typescript-ts-indent-level))
  (add-hook 'go-ts-mode-hook #'lsp-deferred)
  (add-hook 'tsx-ts-mode-hook #'lsp-deferred)
  (add-hook 'typescript-ts-mode-hook #'lsp-deferred)
  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 3000)

  ;; Use the VSCode HTML Language Server instead of the generic one
  (setq lsp-html-server-command '("vscode-html-language-server" "--stdio"))
  ;; You can also customize formatting settings:
  (setq lsp-html-format-enable t         ; ensure formatting is active
        lsp-html-format-wrap-line-length 0
        lsp-html-format-unformatted      nil)

  (setq-hook! '(typescript-mode-hook typescript-tsx-mode-hook)
    +format-with-lsp nil)

  (setq-hook! '(typescript-ts-mode-hook typescript-tsx-mode-hook)
    +format-with-lsp nil)

  (setq-hook! '(tsx-mode-hook typescript-tsx-mode-hook)
    +format-with-lsp nil)

  (add-hook 'graphql-mode-hook #'lsp)

  )

;; In ~/.doom.d/config.el
(add-hook 'web-mode-hook #'lsp-deferred)
;; Or for plain html-mode:
;; (add-hook 'html-mode-hook #'lsp-deferred)


(after! editorconfig
  (add-to-list 'editorconfig-indentation-alist '(typescript-tsx-mode typescript-indent-level)))

(message "after lsp-mode...")

(message "after lsp...")

(setq doom-line-numbers-style 'relative)

(after! org
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

  )

(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))))

(setq doom-line-numbers-style 'relative)
(setq nlinum-highlight-current-line t)

(after! magit
  (evil-collection-init 'magit))

(add-hook 'protobuf-mode-hook (lambda ()
                                (format-all-mode t)
                                ))

(message "after magit...")

(after! typescript-mode
  (add-hook 'typescript-mode-hook (lambda ()
                                    ;; (format-all-mode -1)
                                    ;; (prettier-js-mode)
                                    (setq +format-with-lsp nil)
                                    (setq +format-with 'prettier)
                                    ))

  ;; (add-hook 'before-save-hook (lambda ()
  ;;                               (prettier-js)
  ;;                               ))

  (remove-hook 'before-save-hook #'lsp-format-buffer t)
  (remove-hook 'before-save-hook #'lsp-organize-imports t)

  (setq-hook! 'typescript-mode-hook +format-with-lsp nil)
  (setq typescript-indent-level 2)
  )

(after! typescript-tsx-mode

  (add-hook 'typescript-tsx-mode-hook (lambda ()
                                        ;; (format-all-mode -1)
                                        ;; (prettier-js-mode)
                                        (setq +format-with-lsp nil)
                                        (setq +format-with 'prettier)
                                        ))

  ;; (add-hook 'before-save-hook (lambda ()
  ;;                               (prettier-js)
  ;;                               ))

  (setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
  (remove-hook 'before-save-hook #'lsp-format-buffer t)
  (remove-hook 'before-save-hook #'lsp-organize-imports t)
  (setq typescript-indent-level 2)
  )

(setq-hook! 'typescript-tsx-mode-hook +format-with-lsp nil)
(remove-hook 'before-save-hook #'lsp-format-buffer t)
(remove-hook 'before-save-hook #'lsp-organize-imports t)
(setq typescript-indent-level 2)

(message "after typescript-tsx-mode")

(setq-hook! 'typescript-tsx-mode-hook
  typescript-indent-level 2)

(setq-hook! 'typescript-mode-hook
  typescript-indent-level 2)

(message "after web-mode")

(add-hook 'typescript-mode-hook
          (lambda ()
            ;; Remove LSP’s format and organize-imports on save
            (remove-hook 'before-save-hook #'lsp-format-buffer t)
            (remove-hook 'before-save-hook #'lsp-organize-imports t)))

(add-hook 'typescript-tsx-mode-hook
          (lambda ()
            ;; Remove LSP’s format and organize-imports on save
            (remove-hook 'before-save-hook #'lsp-format-buffer t)
            (remove-hook 'before-save-hook #'lsp-organize-imports t)))

(setq +format-with-lsp nil)

(setq lsp-format-buffer-on-save nil)

(setq lsp-javascript-format-enable nil
      lsp-typescript-format-enable nil)

(auto-composition-mode t)

doom-font (font-spec :family "JetBrains Mono" :size 5)

(setq doom-font (font-spec :family "JetBrainsMono" :size 16)
      doom-big-font (font-spec :family "JetBrainsMono" :size 24))

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
  (message "Setting copilot node executable to %s" (executable-find "node"))
  (setq copilot-node-executable (executable-find "node"))
  )

(map! :localleader  :desc "jr/prettify" "s" #'jr/prettify-and-save)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
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
  (message "Node executable: %s" (executable-find "node"))
  (setq copilot-node-executable (executable-find "node"))

  )

(message "after copilot...")
(defun jr/copilot-tab ()
  "Tab command that will complete with copilot if a completion is
available. Otherwise will try company, yasnippet or normal
tab-indent."
  (interactive)
  (copilot-accept-completion)
  ;; (or (copilot-accept-completion)
  ;;     (and (company--active-p) (company-complete))
  ;;     (evil-insert 1)
  ;;     ;; (indent-for-tab-command)
  ;;     )
  )

(defun jr/nix-post-activate-check ()
  (jr/set-copilot-node-executable)
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
            "git config --get remote.origin.url")))             ; get origin URL :contentReference[oaicite:13]{index=13}
         (https-url
          (-> remote-url
              (replace-regexp-in-string "\\`git@\\(.*?\\):" "https://\\1/") ; SSH→HTTPS
              (replace-regexp-in-string "\\.git\\'" "")))               ; drop .git suffix
         (branch (magit-get-current-branch))                             ; current branch :contentReference[oaicite:14]{index=14}
         (root
          (or (magit-toplevel default-directory)                      ; Magit repo root :contentReference[oaicite:15]{index=15}
              (vc-call-backend (vc-responsible-backend default-directory)
                               'root default-directory)))
         (relative-path (file-relative-name buffer-file-name root))      ; relative path :contentReference[oaicite:16]{index=16}
         (full-url (concat https-url "blob/" branch "/" relative-path)))
    (kill-new full-url)                                          ; copy URL to kill ring
    (message "GitHub URL: %s" full-url)                              ; print URL
    ))

(defun jr/github-file-line-link ()
  "Return a GitHub URL pointing to the current file and line at point."
  (interactive)
  (let ((base (my/github-file-link))
        (line (number-to-string (line-number-at-pos))))               ; line number at point :contentReference[oaicite:17]{index=17}
    (concat base "#L" line)))                                        ; append line anchor :contentReference[oaicite:18]{index=18}


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
  :mode (("\\.tsx\\'" . tsx-ts-mode)
         ("\\.js\\'"  . typescript-ts-mode)
         ("\\.mjs\\'" . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cjs\\'" . typescript-ts-mode)
         ("\\.ts\\'"  . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.json\\'" .  json-ts-mode)
         ("\\.Dockerfile\\'" . dockerfile-ts-mode)
         ("\\.prisma\\'" . prisma-ts-mode)
         ;; More modes defined here...
         )
  :preface
  (defun os/setup-install-grammars ()
    "Install Tree-sitter grammars if they are absent."
    (interactive)
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
      (add-to-list 'treesit-language-source-alist grammar)
      ;; Only install `grammar' if we don't already have it
      ;; installed. However, if you want to *update* a grammar then
      ;; this obviously prevents that from happening.
      (unless (treesit-language-available-p (car grammar))
        (treesit-install-language-grammar (car grammar)))))

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
             (js-mode . typescript-ts-mode)
             (js2-mode . typescript-ts-mode)
             (c-mode . c-ts-mode)
             (c++-mode . c++-ts-mode)
             (c-or-c++-mode . c-or-c++-ts-mode)
             (bash-mode . bash-ts-mode)
             (css-mode . css-ts-mode)
             (go-mode . go-ts-mode)
             (json-mode . json-ts-mode)
             (js-json-mode . json-ts-mode)
             (sh-mode . bash-ts-mode)
             (sh-base-mode . bash-ts-mode)))
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

  ;; (setf (alist-get 'web-mode apheleia-formatters)
  ;;       '("prettier" "--stdin-filepath" input-file "--prose-wrap=preserve" "--object-wrap=preserve" "--html-whitespace-sensitivity=strict"))
  )

;; (after! apheleia
;;   (add-hook 'web-mode-hook
;;             (lambda ()
;;               (apheleia-mode -1))))

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


(message "done loading config.el...")

(provide 'config)
;;; config.el
