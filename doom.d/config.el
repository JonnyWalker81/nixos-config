;;-*- lexical-binding: t -*-
;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;; Add personal doom config directory to load-path
(add-to-list 'load-path "~/.doom.")
(add-to-list 'load-path "~/.elisp")
;; (add-to-list 'load-path "~/.elisp/lin")
;; (add-to-list 'load-path "~/Dropbox/dotfiles/doom/elisp/sunrise-commander")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'load-path "~/Repositories/org-reveal")
;; (setq org-directory "~/Dropbox/Notes/Org/")

;; (require 'sunrise)
;; (require 'sunrise-buttons)
;; (require 'sunrise-modeline)


(if (eq system-type 'darwin)
                                        ;; something for OS X if true
                                        ;; optional something if not
   (exec-path-from-shell-copy-env "GOPATH")
  (exec-path-from-shell-copy-env "RUST_SRC_PATH")
 )

(load! "config-misc")
(load! "config-font")
(load! "config-bindings")

(require 'vc-git)

(require 'ox-reveal)

(toggle-frame-maximized)

(add-to-list 'default-frame-alist '(inhibit-double-buffering . t))

(require 'nix-shell)


;; (require 'lin)
;; (global-hl-line-mode 1)
;; (lin-add-to-many-modes)

;; (defun my-lin--macos-mode-color (light dark)
;;   "Use LIGHT or DARK color value, depending on macOS mode."
;;   (if (eq ns-system-appearance 'light)
;;       light
;;     dark))

;; (defun my-lin-custom-faces ()
;;   (when (memq window-system '(mac ns))
;;     (let ((bg (my-lin--macos-mode-color "selectedContentBackgroundColor" "#2a40b8"))
;;           (fg (my-lin--macos-mode-color "alternateSelectedControlTextColor" "#ffffff")))
;;       (set-face-attribute 'lin-hl nil :background bg)
;;       (set-face-attribute 'lin-hl-override-fg nil :background bg :foreground fg))))

;; (when (memq window-system '(mac ns))
;;   (add-hook 'ns-system-appearance-change-functions #'my-lin-custom-faces))

;; (set-face-attribute 'lin-hl nil
;;                     :background (modus-themes-color 'green-subtle-bg)
;;                     :underline (modus-themes-color 'green-intense))

;; This changes the face that is meant NOT to override the foregrounds.
;; Check `lin-override-foreground'.
;; (set-face-attribute 'lin-hl nil :background "#f0d3ff" :underline "#e0a3ff")

;; And this one is the face that DOES override the foregrounds
;; (set-face-attribute 'lin-hl-override-fg nil :background "#f0d3ff" :underline "#e0a3ff")

;; (when (memq window-system '(mac ns))
;;   (set-face-attribute 'lin-hl nil
;;                       :background "selectedContentBackgroundColor")

;;   ;; To also override the foreground (see `lin-override-foreground'):
;;   (set-face-attribute 'lin-hl-override-fg nil
;;                       :foreground "alternateSelectedControlTextColor"
;;                       :background "selectedContentBackgroundColor"))

;; (when (memq window-system '(mac ns))
;;   (set-face-attribute 'lin-hl nil
;;                       :background "selectedContentBackgroundColor")

;;   ;; To also override the foreground (see `lin-override-foreground'):
;;   (set-face-attribute 'lin-hl-override-fg nil
;;                       :foreground "alternateSelectedControlTextColor"
;;                       :background "selectedContentBackgroundColor"))

;; (super-save-mode +1)

(setq visual-fill-column-width 80)


(use-package! blamer
  :bind (("s-i" . blamer-show-commit-info))
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
  (set-face-attribute 'blamer-face nil :height 1.0)
  (global-blamer-mode 1))



;; (setq rustic-lsp-server 'rust-analyzer)
;; (setq lsp-rust-server 'rust-analyzer)
;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server"))
;; (setq rustic-lsp-client nil)
;; (remove-hook 'rustic-mode-hook 'flycheck-mode)

;; (setq rustic-lsp-client 'lsp-mode)
;; (setq eglot-rust-server 'rust-analyzer)

(setq magit-todos-exclude-globs '("*brindle*"))

;; ;; (after! rustic
;; ;;   (setq rustic-lsp-server 'rust-analyzer)
;; ;;   )

;; ;; (setq lsp-enable-file-watchers nil
;; ;;       lsp-enable-indentation nil
;; ;;       lsp-enable-semantic-highlighting nil
;; ;;       lsp-enable-symbol-highlighting nil
;; ;;       lsp-ui-doc-enable nil
;; ;;       lsp-ui-sideline-show-hover nil)

;; ;; (use-package! rust-mode
;; ;;   :mode "\\.rs$"
;; ;;   :config
;; ;;   (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
;; ;;   (setq rust-format-on-save t)
;; ;;   (flycheck-mode))

;; ;; (after! rustic
;; ;;   :mode "\\.rs$"
;; ;;   (setq rustic-format-on-save t)
;; ;;   (flycheck-stop)
;; ;;   (flycheck-mode -1)
;; ;;   )

;; ;; (setq doom-theme 'doom-Iosvkem)
;; ;; (setq doom-theme 'doom-city-lights)
;; ;; (setq doom-theme 'doom-challenger-deep)
;; ;; (setq doom-theme 'doom-opera)
;; ;; (setq doom-theme 'doom-wilmersdorf)
;; ;; (setq doom-theme 'doom-sourcerer)
;; ;; (setq doom-theme 'doom-vibrant)
;; ;; (setq doom-theme 'clues)
;; (setq doom-theme 'modus-vivendi)
;; ;; (setq doom-theme 'doom-nord)
;; ;; (setq doom-theme 'doom-spacegrey)
;; ;; (setq doom-theme 'zenburn)
;; ;; (setq doom-theme 'doom-tomorrow-night)
;; ;; (setq doom-theme 'doom-outrun-electric)
;; ;; (setq doom-theme 'doom-vibrant)
;; ;; (setq doom-theme 'doom-peacock)

(map! :leader  :desc "dumb-jump-go" "d" #'dumb-jump-go)
(map! :leader  :desc "dumb-jump-go other window" "D" #'dumb-jump-go-other-window)

;; (load-theme 'zenburn)
(setq +popup-buffer-mode 'toggle)
(setq doom-line-numbers-style 'relative)
(setq ns-use-thin-smoothing t)
;; (setq lsp-enable-snippet nil)
;; ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

(require 'protobuf-mode)
(require 'prettier-js)

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
 (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
 ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
 ;; (doom-themes-neotree-config)
 ;; ;; or for treemacs users
 ;; (doom-themes-treemacs-config)

 ;; ;; Corrects (and improves) org-mode's native fontification.
 ;; (doom-themes-org-config)

 ;; (setq which-key-idle-delay 1.0)

 ;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
 ;; (add-to-list 'default-frame-alist '(ns-appearance . dark))
 ;; (setq ns-use-proxy-icon nil)
 ;; (setq frame-title-format nil)

 (set-fill-column 120)

(defun jr-setup-dev-env ()
  (interactive)
  (message "Settting up dev env...")
  (setq doom-line-numbers-style 'relative)
  (setq display-line-numbers 'visual)
  (set-fill-column 120)
  (setq rust-format-on-save t)
  (setq go-mode-format-on-save t)
  ;; (remove-hook 'rustic-mode-hook 'flycheck-mode)
  (setq gofmt-command "goimports")
  (setq lsp/rust-analyzer-inlay-hint-kind-type-hint t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-lens-mode t)
  (lsp-ui-mode t)
  (company-mode-on)
  )

;; (after! helm
;;         (setq helm-display-function 'helm-display-buffer-in-own-frame
;;         helm-display-buffer-reuse-frame t
;;         helm-use-undecorated-frame-option t)
;; )
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Optional - provides fancier overlays.

(after! lsp-mode
  (lsp-register-custom-settings
   '(("gopls.experimentalWorkspaceModule" t t)
     ("gopls.completeUnimported" t t)
     ("gopls.staticcheck" t t)
     ("typescript.inlayHints.variableTypes.enabled" t t)
     ("typescript.inlayHints" t t)
     ))
  )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package! lsp-mode
  :ensure
  :commands lsp
 :config
  (setq lsp-rust-analyzer-cargo-watch-command "clippy")
  (setq lsp-eldoc-render-all t)
  (setq lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-ui-mode 1)
  (lsp-lens-mode 1)
  (lsp-lens-mode)
  (setq lsp-lens-enable t)
 (add-hook 'lsp-mode-hook 'lsp-ui-mode)
 (add-hook 'lsp-mode-hook 'lsp-lens-mode)
  )

(defun jr-after-change-major-mode-hook ()
;;   (interactive)
  (setq doom-line-numbers-style 'relative)
  (setq display-line-numbers 'visual)
  (setq web-mode-enable-current-element-highlight t)
;;   ;; (smartparens-global-mode -1)
  )

(use-package! flycheck-golangci-lint
  :ensure t
  :hook (go-mode . flycheck-golangci-lint-setup))

(add-hook 'terraform-mode-hook (lambda ()
                                 (terraform-format-on-save-mode)
                                 ))

(add-hook 'prog-mode-hook (lambda ()
                                 ;; (add-to-list 'tree-sitter-major-mode-language-alist '(terraform-mode . hcl))
                            ;; (jr-setup-dev-env)
                            (set-fill-column 120)
                              (tree-sitter-hl-mode)
                            ;; (smartparens-global-mode -1)
                ;; (smartparens-mode -1)
                ;; (turn-off-smartparens-mode)
                            ))

(add-hook 'after-change-major-mode-hook (lambda()
                                          (jr-after-change-major-mode-hook)
                                          ))

;; (add-hook 'js2-mode-hook (lambda ()
;;                            (jr-setup-dev-env)
;;                            ))

(after! org
                           ;; (setq org-startup-indented nil)
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
                           )

;; ;; in ~/.doom.d/config.el (for example)
(after! smartparens
  (dolist (brace '("(" "{" "["))
    (sp-pair brace nil :unless '(:rem sp-point-before-word-p sp-point-before-same-p))))

(setq doom-line-numbers-style 'relative)

(use-package company
  :ensure t
  :delight
  :init
  (setq company-backends '(
                           company-files
                           company-capf
                           company-keywords
                           company-semantic
                           company-etags
                           company-elisp
                           company-clang
                           company-irony-c-headers
                           company-irony
                           company-jedi
                           company-cmake
                           company-ispell
                           company-yasnippet))
)

(setq nlinum-highlight-current-line t)

;; (setq rustic-format-on-save t)
(setq go-mode-format-on-save t)

;; (setq racer-rust-src-path
;;       (concat (string-trim
;;                (shell-command-to-string "rustc --print sysroot"))
;;               "/lib/rustlib/src/rust/src"))

(setq company-idle-delay 0.1)
(setq company-minimum-prefix-length 2)

(global-whitespace-mode)
;; (global-whitespace-newline-mode)


(advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

(setq gofmt-command "goimports")
;; (setq gofmt-command "gci")
;; (setq gofmt-command "gci")
;; (setq gofmt-args '("-s Standard" "-s Default" "-s 'Prefix(github.com/geneva)'"))
;; (setq gofmt-args '())

(add-hook 'before-save-hook #'gofmt-before-save)

(after! magit
       (evil-collection-init 'magit))

(add-hook 'protobuf-mode-hook (lambda ()
                                (format-all-mode t)
                                ))
(add-hook 'go-mode-hook (lambda ()
      (setq gofmt-command "goimports")
      (add-hook 'before-save-hook 'gofmt-before-save)
                          ;; (add-hook 'before-save-hook 'gofmt-before-save)

                          ;; (company-mode -1)
                          (setq go-mode-format-on-save t)
    ;; (lsp-go-install-save-hooks)
          (flycheck-mode -1)
                          (format-all-mode t)
                          ;; (eldoc-mode 0)
                          (company-mode 1)

                          (setq compile-command "go build")
                          (setq compilation-read-command nil)

                ;; (smartparens-global-mode -1)
                ;; (smartparens-mode -1)
                ;; (turn-off-smartparens-mode)

                          ))

(add-hook 'elm-mode-hook (lambda ()
                           (setq elm-format-on-save-mode t)
                           (elm-format-on-save-mode)
                           ))

(setq display-line-numbers 'visual)

;; (add-hook 'rustic-mode-hook 'eglot-ensure)
(add-hook 'rustic-mode-hook (lambda ()
  ;; (company-mode -1) (flycheck-mode -1) (remove-hook 'rustic-mode-hook
  ;; 'flycheck-mode) (flycheck-mode -1) (add-hook 'before-save-hook (lambda ()
  ;; (rustic-format-buffer) (message "formatting rust buffer...")
                    ))

;; (add-hook 'omnisharp-mode-hook (lambda ()
;;                                 (add-hook 'before-save-hook (lambda ()
;;	                                                              (omnisharp-code-format-entire-file)
;;	                                                              ))))
;;
;;
;;(add-hook 'csharp-mode-hook (lambda ()
;;                             (add-hook 'before-save-hook (lambda ()
;;                                                           ;; (when (derived-mode-p 'csharp-mode)
;;                                                           (omnisharp-code-format-entire-file)
;;                                                           ;; )
;;                                                           ))))

(add-hook 'graphql-mode-hook 'prettier-js-mode)

(add-hook 'typescript-mode-hook 'prettier-js-mode)

(add-hook 'swift-mode-hook (lambda ()
                             (format-all-mode t)
                             ))

(defun tide-setup-hook ()
    (tide-setup)
    (eldoc-mode)
    (run-import-js)
    (tide-hl-identifier-mode +1)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-attr-indent-offset 2)
    (setq web-mode-attr-value-indent-offset 2)
    ;; (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio"))
    (set (make-local-variable 'company-backends)
         '((company-tide company-files :with company-yasnippet)
           (company-dabbrev-code company-dabbrev))))

;; hooks
;; (add-hook 'before-save-hook 'tide-format-before-save)


;; use rjsx-mode for .js* files except json and use tide with rjsx
(add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
(add-hook 'rjsx-mode-hook 'tide-setup-hook)


;; web-mode extra config
(add-hook 'web-mode-hook 'tide-setup-hook
          (lambda () (pcase (file-name-extension buffer-file-name)
                  ("tsx" ('tide-setup-hook))
                  (_ (my-web-mode-hook)))))

;; (flycheck-add-mode 'typescript-tslint 'web-mode)
(add-hook 'web-mode-hook 'company-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)
(add-hook 'web-mode-hook #'turn-on-smartparens-mode t)
;; (defun jr-swift-format-hook ()
;;   (interactive)
;;   (format-all-buffer)
;;   )

;; (autopair-global-mode 1)
;; (smartparens-global-mode -1)
;; (smartparens-global-mode nil)
;; ;; (add-hook 'swift-mode-hook (lambda ()
;; ;;                              (add-hook 'before-save-hook 'jr-swift-format-hook
;; ;;                                        )))

;; (if (eq system-type 'windows)
;;     (setq omnisharp-server-executable-path "C:\\Users\\jrothberg\\Downloads\\Omnisharp-win-x86\\OmniSharp.exe")
;;   )

;; ;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; ;;(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; ;; (racer-mode)
;; ;; (add-hook 'rust-mode-hook (lambda()
;; ;;                             (racer-mode)
;; ;;                                     ))

;; ;; (add-hook 'rust-mode-hook #'racer-mode)
;; ;; (add-hook 'racer-mode-hook #'eldoc-mode)
;;
;; doom-big-font (font-spec :family "Fira Mono" :size 19)
;; (setq doom-font (font-spec :family "Fira Code" :size 19))
(auto-composition-mode t)
 ;; (setq doom-font (font-spec :family "Input Mono" :size 19))
 ;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 19))
 ;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 19))
 ;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 19))
 ;; (setq doom-font (font-spec :family "Cascadia Code" :size 16))
 (setq doom-font (font-spec :family "JetBrains Mono Medium" :size 14))
;; (setq doom-font (font-spec :family "JetBrains Mono Medium" :size 16))
 ;; (setq doom-font (font-spec :family "JetBrains Mono SemiLight" :size 21)) 
;; (setq doom-font (font-spec :family "Victor Mono" :size 21)) 
;; (setq doom-font (font-spec :family "Victor Mono Bold" :size 16))

;; (if (string= (system-name) "Jonathans-MacBook-Pro.local")
;;  (setq doom-font (font-spec :family "Cascadia Code" :size 16))
;;     )

;; (add-hook 'magit-mode-hook (lambda ()
;;                               (message "magit-mode-hook")
;;                               (evil-emacs-state 1)
;;                               ))

;; (add-hook 'magit-status-mode-hook (lambda ()
;;                               (message "magit-mode-status-hook")
;;                                 (evil-emacs-state 1)
;;                                      ))
;; (after! magit
;;         (evil-set-initial-state 'magit-mode 'normal)
;;         (add-hook! 'org-mode :append (turn-off-evil-mode))
;;   )
;; (add-to-list 'evil-emacs-state-modes 'magit-mode)
;; (add-to-list 'evil-emacs-state-modes 'magit-status-mode)


;; (defvar host (substring (shell-command-to-string "hostname") 0 -1))
;; (message host)
;; (defvar host-dir (concat "~/.emacs.d/hosts"))
;; (add-to-list 'load-path host-dir)

;; (let ((init-host-feature (intern (concat "init-" host))))
;;   (message (concat "Found local init..."))
;;   (require init-host-feature nil 'noerror))

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

(add-hook 'org-mode-hook (lambda () (electric-indent-local-mode -1)))

(setq go-tag-args (list "-transform" "camelcase"))

(setq github-review-fetch-top-level-and-review-comments t)

(require 'exec-path-from-shell)

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(require 'keychain-environment)
(keychain-refresh-environment)

;; (use-package! vertico
;;   :ensure t
;;   :init
;;   (vertico-mode))

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

(use-package savehist
  :init
  (savehist-mode))

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

(defun jr/bump-code-to-env (from-branch &optional to-branch)
  (interactive (list (read-string "From Branch (master): "
                             nil nil "master")
                     (read-string "To Branch (dev): "
                             nil nil "dev")))


  (let* ((project-root (projectile-project-root))
         (default-directory project-root)
         )
        (message "From: %s" from-branch)
        (message "To: %s" to-branch)
        (message "Project Root: %s" project-root)

        (shell-command (format "git checkout %s" to-branch))
        (shell-command (format "git pull origin %s" to-branch))
        (shell-command (format "git rebase %s" from-branch))
        (shell-command "git" "push" "origin")
        )
  )

(setq magit-circleci-token "d6d0347d6efda8daffc42d7b1156e4ed22572c05")

(use-package! modus-themes
  :ensure
  :init
  ;; ;; Add all your customizations prior to loading the themes
  ;; (setq modus-themes-italic-constructs t
  ;;       modus-themes-bold-constructs nil
  ;;       modus-themes-region '(bg-only no-extend))

  ;; Load the theme files before enabling a theme
  (modus-themes-load-themes)
  :config
  ;; Load the theme of your choice:
  (modus-themes-load-vivendi)
  :bind ("<f5>" . modus-themes-toggle))

(map! :leader  :desc "fzf-projectile" "f h" #'fzf-projectile)
(map! :leader  :desc "fzf-switch-buffer" "b h" #'fzf-switch-buffer)

(use-package! lusty-explorer
   :ensure
   :init
  )

(smooth-scrolling-mode 1)
(setq scroll-step 1)
(setq scroll-margin 99)
(setq smooth-scroll-margin 99)

(use-package! zig-mode
  :hook ((zig-mode . lsp-deferred))
  :custom (zig-format-on-save t)
  :config
  (after! lsp-mode
    (add-to-list 'lsp-language-id-configuration '(zig-mode . "zig"))
    (message (concat (getenv "HOME") "/zls/zls"))
    (lsp-register-client
      (make-lsp-client
        :new-connection (lsp-stdio-connection (concat (getenv "HOME") "/zls/zls"))
        :major-modes '(zig-mode)
        :server-id 'zls))))


(defun affe-orderless-regexp-compiler (input _type)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(use-package! affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package! blamer
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))

(use-package! eyebrowse
  :ensure
  :config
        (eyebrowse-mode t)
  )

(provide 'config)
;;; config.el
