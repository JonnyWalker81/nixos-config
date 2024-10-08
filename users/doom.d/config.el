;;-*- lexical-binding: t -*-
;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


(defun load-directory (dir)
  (let ((load-it (lambda (f)
		   (load-file (concat (file-name-as-directory dir) f)))
		 ))
    (mapc load-it (directory-files dir nil "\\.el$"))))

;; Add personal doom config directory to load-path
(add-to-list 'load-path "~/.doom.d")
(add-to-list 'load-path "~/.elisp")
;; (add-to-list 'load-path "~/.elisp/lin")
;; (add-to-list 'load-path "~/Dropbox/dotfiles/doom/elisp/sunrise-commander")
(add-to-list 'exec-path "~/.cargo/bin")
(add-to-list 'load-path "~/Repositories/org-reveal")
(when (file-exists-p "~/Repositories/komodo")
  (add-to-list 'load-path "~/Repositories/komodo")
  (load-directory "~/Repositories/komodo")
  )
;; (setq org-directory "~/Dropbox/Notes/Org/")

;; (require 'sunrise)
;; (require 'sunrise-buttons)
;; (require 'sunrise-modeline)

;; (use-package! tsc
;;   :config
;; (setq! tsc-dyn-get-from '(:compilation))
;;   )

;; (setq major-mode-remap-alist
;;  '(
;;    ;; (go-mode . go-ts-mode)
;;    ;; (yaml-mode . yaml-ts-mode)
;;    (bash-mode . bash-ts-mode)
;;    (js2-mode . js-ts-mode)
;;    (typescript-mode . typescript-ts-mode)
;;    (json-mode . json-ts-mode)
;;    (css-mode . css-ts-mode)
;;    ;; (python-mode . python-ts-mode)
;;    )
;;  )

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



;; (setq rustic-lsp-server 'rust-analyzer)
;; (setq lsp-rust-server 'rust-analyzer)
;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/rust-analyzer"))
;; (setq lsp-rust-analyzer-server-command '("~/.cargo/bin/ra_lsp_server"))
;; (setq rustic-lsp-client nil)
;; (remove-hook 'rustic-mode-hook 'flycheck-mode)

;; (setq rustic-lsp-client 'lsp-mode)
;; (setq eglot-rust-server 'rust-analyzer)

(setq magit-todos-exclude-globs '(
                                  "*brindle*"
                                  "*node_modules*"
                                  ))

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

(map! :leader :desc "fold code" "c f" #'fold-this)
(map! :leader :desc "fold code" "c u" #'fold-this-unfold-at-point)

(map! :leader :desc "toggle lsp doc" "t d" #'lsp-ui-doc-toggle)

;; (add-to-list 'auto-mode-alist '("\\.ml[iylp]?$" . caml-mode))
;; (autoload 'caml-mode "caml" "Major mode for editing OCaml code." t)
;; (autoload 'run-caml "inf-caml" "Run an inferior OCaml process." t)
;; (autoload 'camldebug "camldebug" "Run ocamldebug on program." t)
;; (add-to-list 'interpreter-mode-alist '("ocamlrun" . caml-mode))
;; (add-to-list 'interpreter-mode-alist '("ocaml" . caml-mode))

;; (load-theme 'zenburn)
(setq +popup-buffer-mode 'toggle)
(setq doom-line-numbers-style 'relative)
(setq ns-use-thin-smoothing t)
;; (setq lsp-enable-snippet nil)
;; ;; Enable flashing mode-line on errors
;; (doom-themes-visual-bell-config)

(require 'protobuf-mode)
;; (require 'prettier-js)

;; (setq prettier-js-args nil)

;; (setq prettier-js-args '(
;;   "--trailing-comma" "es5"
;;   "--tab-width" "2"
;;   "--print-width" "100"
;; ))

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
;; (add-to-list 'auto-mode-alist '("\\.rs$" . rustic-mode))
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

;; (defun jr-setup-dev-env ()
;;   (interactive)
;;   (message "Settting up dev env...")
;;   (setq doom-line-numbers-style 'relative)
;;   (setq display-line-numbers 'visual)
;;   (set-fill-column 120)
;;   (setq rust-format-on-save t)
;;   (setq go-mode-format-on-save t)
;;   ;; (remove-hook 'rustic-mode-hook 'flycheck-mode)
;;   (setq gofmt-command "goimports")
;;   (setq lsp/rust-analyzer-inlay-hint-kind-type-hint t)
;;   (setq lsp-rust-analyzer-server-display-inlay-hints t)
;;   ;; (lsp-lens-mode t)
;;   ;; (lsp-ui-mode t)
;;   (company-mode-on)
;;   )

;; (after! helm
;;         (setq helm-display-function 'helm-display-buffer-in-own-frame
;;         helm-display-buffer-reuse-frame t
;;         helm-use-undecorated-frame-option t)
;; )
;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
;; (defun lsp-go-install-save-hooks ()
;;   (add-hook 'before-save-hook #'lsp-format-buffer t t)
;;   (add-hook 'before-save-hook #'lsp-organize-imports t t))

;; Optional - provides fancier overlays.

(after! lsp-mode
  ;;   (lsp-register-custom-settings
  ;;    '(
  ;;      ("gopls.workspaces" t t)
  ;;      ("gopls.completeUnimported" t t)
  ;;      ("gopls.staticcheck" t t)
  ;;      ("typescript.inlayHints.variableTypes.enabled" t t)
  ;;      ("typescript.inlayHints" t t)
  ;;      ))
  ;;
  ;;   (setq lsp-ui-doc-enable t)
  ;;   (setq lsp-ui-peek-enable t)
  ;;   (setq lsp-yaml-single-quote t)

  (setq lsp-enable-file-watchers t)
  (setq lsp-file-watch-threshold 3000)
  ;; (setq lsp-restart 'auto-restart)
  )

;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode)

;; (use-package! lsp-mode
;;   :ensure
;;   :commands lsp
;;  :config
;;   (setq lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (setq lsp-eldoc-render-all t)
;;   (setq lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-ui-mode 1)
;;   (lsp-lens-mode 1)
;;   (lsp-lens-mode)
;;   (setq lsp-lens-enable t)
;;  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;  (add-hook 'lsp-mode-hook 'lsp-lens-mode)
;;   )

;; (defun jr-after-change-major-mode-hook ()
;; ;;   (interactive)
;;   (setq doom-line-numbers-style 'relative)
;;   (setq display-line-numbers 'visual)
;;   (setq web-mode-enable-current-element-highlight t)
;; ;;   ;; (smartparens-global-mode -1)
;; )

;; (use-package! flycheck-golangci-lint
;;   :ensure t
;;   :hook (go-mode . flycheck-golangci-lint-setup))

;; (use-package! tree-sitter
;;               :ensure 
;;               :config
;;   (add-to-list 'tree-sitter-load-path (concat doom-cache-dir "tree-sitter"))
;; )
;;
;; (after! tree-sitter
;;   (add-to-list 'tree-sitter-load-path (concat doom-cache-dir "tree-sitter"))
;;   )
;;
;; (add-hook 'terraform-mode-hook (lambda ()
;;                                  (terraform-format-on-save-mode)
;;                                  ))
;;

;; (add-hook 'prog-mode-hook (lambda ()
(setq doom-line-numbers-style 'relative)
;;                                  ;; (add-to-list 'tree-sitter-major-mode-language-alist '(terraform-mode . hcl))
;;                             ;; (jr-setup-dev-env)
;;                             (set-fill-column 120)
;;                               ;; (tree-sitter-hl-mode)
;;                             ;; (smartparens-global-mode -1)
;;                 ;; (smartparens-mode -1)
;;                 ;; (turn-off-smartparens-mode)
;;                           ))

;;
;; (add-hook 'after-change-major-mode-hook (lambda()
;;                                           (jr-after-change-major-mode-hook)
;;                                           ))
;;
;; ;; (add-hook 'js2-mode-hook (lambda ()
;; ;;                            (jr-setup-dev-env)
;; ;;                            ))
;;
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

;; (use-package company
;;   :ensure t
;;   :delight
;;   :init
;;   (setq company-backends '(
;;                            company-files
;;                            ;; company-capf
;;                            company-keywords
;;                            company-semantic
;;                            company-etags
;;                            company-elisp
;;                            company-clang
;;                            company-irony-c-headers
;;                            company-irony
;;                            company-jedi
;;                            company-cmake
;;                            company-ispell
;;                            company-yasnippet))
;; )
;;
(setq nlinum-highlight-current-line t)

;; (setq rustic-format-on-save t)
;; (setq go-mode-format-on-save t)

;; (setq racer-rust-src-path
;;       (concat (string-trim
;;                (shell-command-to-string "rustc --print sysroot"))
;;               "/lib/rustlib/src/rust/src"))

;; (setq company-idle-delay 0.1)
;; (setq company-minimum-prefix-length 2)

;; (global-whitespace-mode)
;; (global-whitespace-newline-mode)


;; (advice-add 'lsp :before (lambda (&rest _args) (eval '(setf (lsp-session-server-id->folders (lsp-session)) (ht)))))

;; (setq gofmt-command "goimports")
;; (setq gofmt-command "gci")
;; (setq gofmt-command "gci")
;; (setq gofmt-args '("-s Standard" "-s Default" "-s 'Prefix(github.com/geneva)'"))
;; (setq gofmt-args '())

;; (add-hook 'before-save-hook #'gofmt-before-save)

(after! magit
  (evil-collection-init 'magit))

(add-hook 'protobuf-mode-hook (lambda ()
                                (format-all-mode t)
                                ))
;; (add-hook 'go-mode-hook (lambda ()
;;                           ;; (lsp-deferred)
;;       (setq gofmt-command "goimports")
;;       (add-hook 'before-save-hook 'gofmt-before-save)
;;                           ;; (add-hook 'before-save-hook 'gofmt-before-save)

;;                           ;; (company-mode -1)
;;                           (setq go-mode-format-on-save t)
;;     ;; (lsp-go-install-save-hooks)
;;           (flycheck-mode -1)
;;                           (format-all-mode t)
;;                           ;; (eldoc-mode 0)
;;                           (company-mode 1)

;;                           (setq compile-command "go build")
;;                           (setq compilation-read-command nil)

;;                 ;; (smartparens-global-mode -1)
;;                 ;; (smartparens-mode -1)
;;                 ;; (turn-off-smartparens-mode)

;;                           ))

;; (add-hook 'elm-mode-hook (lambda ()
;;                            (setq elm-format-on-save-mode t)
;;                            (elm-format-on-save-mode)
;;                            ))
;;
;; (setq display-line-numbers 'visual)

;; (add-hook 'rustic-mode-hook 'eglot-ensure)
;; (add-hook 'rustic-mode-hook (lambda ()
;;                               (lsp)
;; (lsp-mode)
;; (company-mode -1) (flycheck-mode -1) (remove-hook 'rustic-mode-hook
;; 'flycheck-mode) (flycheck-mode -1) (add-hook 'before-save-hook (lambda ()
;; (rustic-format-buffer) (message "formatting rust buffer...")
;; ))

;; (add-hook 'go-mode-hook (lambda ()
;;                           (lsp-mode)
;;                           ))
;;
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

;; (add-hook 'graphql-mode-hook 'prettier-js-mode)
;;
(after! typescript-mode
  (add-hook 'typescript-mode-hook (lambda ()
                                    (format-all-mode -1)
                                    ;; (prettier-js-mode)
                                    (setq +format-with-lsp nil)
                                    ))
  )

(setq-hook! 'typescript-mode-hook +format-with-lsp nil)
;;
;; (add-hook 'swift-mode-hook (lambda ()
;;                              (format-all-mode t)
;;                              ))
;;
;; (defun tide-setup-hook ()
;;     (tide-setup)
;;     (eldoc-mode)
;;     (run-import-js)
;;     (tide-hl-identifier-mode +1)
;;     (setq web-mode-enable-auto-quoting nil)
;;     (setq web-mode-markup-indent-offset 2)
;;     (setq web-mode-code-indent-offset 2)
;;     (setq web-mode-attr-indent-offset 2)
;;     (setq web-mode-attr-value-indent-offset 2)
;;     ;; (setq lsp-eslint-server-command '("node" (concat (getenv "HOME") "/var/src/vscode-eslint/server/out/eslintServer.js") "--stdio"))
;;     (set (make-local-variable 'company-backends)
;;          '((company-tide company-files :with company-yasnippet)
;;            (company-dabbrev-code company-dabbrev))))
;;
;; hooks
;; (add-hook 'before-save-hook 'tide-format-before-save)


;; use rjsx-mode for .js* files except json and use tide with rjsx
;; (add-to-list 'auto-mode-alist '("\\.js.*$" . rjsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
;; (add-hook 'rjsx-mode-hook 'tide-setup-hook)
;;
;;
;; ;; web-mode extra config
;; (add-hook 'web-mode-hook 'tide-setup-hook
;;           (lambda () (pcase (file-name-extension buffer-file-name)
;;                   ("tsx" ('tide-setup-hook))
;;                   (_ (my-web-mode-hook)))))
;;
;; ;; (flycheck-add-mode 'typescript-tslint 'web-mode)
;; (add-hook 'web-mode-hook 'company-mode)
;; ;; (add-hook 'web-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook #'turn-on-smartparens-mode t)
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
(auto-composition-mode t)
;; (setq doom-font (font-spec :family "Input Mono" :size 19))
;; (setq doom-font (font-spec :family "Iosevka Nerd Font Mono" :size 19))
;; (setq doom-font (font-spec :family "DejaVu Sans Mono" :size 19))
;; (setq doom-font (font-spec :family "Fantasque Sans Mono" :size 19))
;; doom-big-font (font-spec :family "Fira Mono" :size 25)
;; (setq doom-font (font-spec :family "Fira Code" :size 16))
                                        ; doom-big-font (font-spec :family "Monaspace" :size 25)
                                        ; (setq doom-font (font-spec :family "Monaspace" :size 16))
;; (setq doom-font (font-spec :family "Cascadia Code" :size 16))
;; doom-big-font (font-spec :family "Cascadia Code" :size 25)
(setq doom-font (font-spec :family "JetBrains Mono" :size 16))
doom-big-font (font-spec :family "JetBrains Mono" :size 28)
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

(add-hook 'org-mode-hook (lambda () 
                           (electric-indent-local-mode -1)
                          (setq org-adapt-indentation t)
                           ))

(setq go-tag-args (list "-transform" "camelcase"))

(setq github-review-fetch-top-level-and-review-comments t)

(require 'exec-path-from-shell)

(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;; (exec-path-from-shell-initialize)

(require 'keychain-environment)
(keychain-refresh-environment)

(use-package! vertico
  :custom
  (vertico-cycle t)
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode)
  )
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

;; (use-package! modus-themes
;;  :ensure
;;  :init
;;  ;; ;; Add all your customizations prior to loading the themes
;;  (setq modus-themes-italic-constructs t
;;        modus-themes-bold-constructs t
;;        modus-themes-region '(bg-only)
;;        modus-themes-hl-line '(underline)
;;        modus-themes-paren-match '(bold intense underline)
;;        modus-themes-completions '((matches . (extrabold))
;;                                 (selection . (semibold accented))
;;                                 (popup . (accented intense)))
;;        modus-themes-mode-line '(accented borderless (padding . 5) (height . 0.9))
;;        )
;;
;;  ;; Load the theme files before enabling a theme
;;  ;; (modus-themes-load-themes)
;;  :config
;;  ;; Load the theme of your choice:
;;  ;; (modus-themes-load-vivendi)
;;   (load-theme 'modus-vivendi t)             ; Dark theme
;;  :bind ("<f5>" . modus-themes-toggle)
;;  )


;; (use-package! ef-themes
;;   :ensure
;;   :init
;;
;;   (setq ef-themes-mixed-fonts t
;;         ef-modus-themes-variable-pitch-ui t)
;;
;;   (mapc #'disable-theme custom-enabled-themes)
;;
;;   ;; (ef-themes-select 'ef-duo-dark)
;;   (ef-themes-select 'ef-deuteranopia-dark)
;;   ;; (ef-themes-select 'ef-trio-dark)
;;
;;   )

;; (use-package! tokyo-theme
;;               :ensure
;;               :config 
;;             (load-theme 'tokyo t)
;;               )

;; (use-package! rose-pine-doom-emacs
;;               :ensure
;;               :config
;;               (setq doom-theme 'doom-rose-pine-moon)
;;               )

;; (setq doom-theme 'doom-rose-pine-moon)
(setq doom-theme 'doom-tokyo-night)

(use-package! topsy
  :ensure
  :hook (prog-mode . topsy-mode)
  )

(use-package! fzf
  :ensure
  :init
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll --")
  )

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

(use-package! lusty-explorer
  :ensure
  :init
  )

(smooth-scrolling-mode 1)
(setq scroll-step 1)
;; (setq scroll-margin 999)
(setq smooth-scroll-margin 999)

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


(defun affe-orderless-regexp-compiler (input _type)
  (setq input (orderless-pattern-compiler input))
  (cons input (lambda (str) (orderless--highlight input str))))
(setq affe-regexp-compiler #'affe-orderless-regexp-compiler)

(use-package! affe
  :config
  ;; Manual preview key for `affe-grep'
  (consult-customize affe-grep :preview-key (kbd "M-.")))

(use-package! consult 
  ;; :hook (completion-list-mode . consult-preview-at-point-mode)
  :config
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
  )

(use-package! eyebrowse
  :ensure
  :config
  (eyebrowse-mode t)
  )

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

(add-hook 'dap-stopped-hook
          (lambda (arg) (call-interactively #'dap-hydra)))

;; (add-hook 'tuareg-mode-hook (lambda ()
;;   (define-key tuareg-mode-map (kbd "C-M-<tab>") #'ocamlformat)
;;   (add-hook 'before-save-hook #'ocamlformat-before-save)))
;;
(use-package! ocamlformat
  :custom (ocamlformat-enable 'enable-outside-detected-project)
  :hook (before-save . ocamlformat-before-save)
  )

;;
;; (use-package! dired-preview
;;   :hook ((dired-mode . dired-preview-mode))
;;   )
;; (after! tuareg-mode 
;;   (add-hook 'before-save-hook #'ocamlformat-before-save)
;; )

  ;; (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'go-mode-hook 'lsp-defferred)
  (add-hook 'go-ts-mode-hook 'lsp-deferred)
(use-package! go-mode 
;; ;;   ;; :hook ((go-mode . lsp-mode))
;; ;;   :hook ((go-mode))
  :config
;; ;;   ;; (set-formatter! 'gofmt "goimports")
  (setq gofmt-command "goimports")
  )


(after! go-mode
  ;; (set-formatter! 'gofmt "goimports")
  (setq gofmt-command "goimports")
  ;; (add-hook 'before-save-hook 'gofmt-before-save)
  (add-hook 'before-save-hook (lambda ()
                                (gofmt-before-save)
                                (lsp-organize-imports)
                                ))
  ;; (lsp-mode)
  )
;;
(setq-hook! 'go-mode-hook +format-with-lsp t) 
;;
(setq display-line-numbers-type 'relative)


(use-package! jenkinsfile-mode
  :config
(add-to-list 'auto-mode-alist '(".*Jenkinsfile.*\\'" . jenkinsfile-mode))
             )

(defun check-lsp-mode ()
  (when (derived-mode-p 'prog-mode) 
    (unless lsp-mode
      (message "enabling lsp-mode")
      (lsp-mode)
      )
    (setq +format-with-lsp
          (not (or (eq major-mode 'typescript-tsx-mode)
                   (eq major-mode 'typescript-mode))))
    )
  ) 

;; (run-with-idle-timer 2 t #'check-lsp-mode)

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

;; (use-package! dirvish
;;   :init
;;   (dirvish-override-dired-mode)
;;   :config
;;   (setq dired-listing-switches "-la --almost-all --human-readable --group-directories-first --no-group")
;;   )


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

;; (define-key global-map (kbd "C-<tab>") #'jr/copilot-tab)


;; (after! (evil copilot)
;;   ;; Define the custom function that either accepts the completion or does the default behavior
;;   (defun my/copilot-tab-or-default ()
;;     (interactive)
;;     (if (and
;;          ;; (bound-and-true-p copilot-mode)
;;          (copilot--overlay-visible)
;;          ;; Add any other conditions to check for active copilot suggestions if necessary
;;          )
;;         (copilot-accept-completion)
;;       (evil-insert 1))) ; Default action to insert a tab. Adjust as needed.
;;
;;   ;; Bind the custom function to <tab> in Evil's insert state
;;   (evil-define-key 'insert 'global (kbd "<tab>") 'my/copilot-tab-or-default))

(setq doom-line-numbers-style 'relative)

(setq code-review-auth-login-marker 'forge)

(add-hook 'code-review-mode-hook
          (lambda ()
            ;; include *Code-Review* buffer into current workspace
            (persp-add-buffer (current-buffer))))

(use-package! gleam-mode
  :bind (:map gleam-mode-map
              ("C-c g f" . gleam-format)))


;; (use-package! lsp-bridge
;;   :config
;;   (setq lsp-bridge-enable-log t)
;;   (setq lsp-bridge-python-command (executable-find "python3"))
;;   (setq lsp-bridge-enable-inlay-hint t)
;;   (setq lsp-bridge-enable-auto-format-code t)
;;   (setq lsp-bridge-enable-hover-diagnostic t)
;;   (evil-define-key 'insert acm-mode-map (kbd "C-n") #'acm-select-next)
;;   (evil-define-key 'insert acm-mode-map (kbd "C-p") #'acm-select-prev)
;;   (add-hook 'acm-mode-hook #'evil-normalize-keymaps)
;;
;;   (map! :leader :desc "lsp-bridge-code-action" "c a" #'lsp-bridge-code-action)
;;   (global-lsp-bridge-mode)
;;   )

(use-package! indent-guide
  :config
  (indent-guide-global-mode)
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


;; (setq read-file-name-function #'consult-find-file-with-preview)
;;
;; (defun consult-find-file-with-preview (prompt &optional dir default mustmatch initial pred)
;;   (interactive)
;;   (let ((default-directory (or dir default-directory))
;;         (minibuffer-completing-file-name t))
;;     (consult--read #'read-file-name-internal :state (consult--file-preview)
;;                    :prompt prompt
;;                    :initial initial
;;                    :require-match mustmatch
;;                    :predicate pred)))

;; (define-key vertico-map [S-up] #'vertico-previous)
;; (define-key vertico-map [S-down] #'vertico-next)
;; (consult-customize consult-recent-file :preview-key '([S-up] [S-down]))

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
          (message "Copied branch name: %s" branch-name))
      (message "Not in a Git repository or not on a branch"))))

(use-package! treesit-auto 
              :config

(setq jr/js-tsauto-config
      (make-treesit-auto-recipe
       :lang 'javascript
       :ts-mode 'js-ts-mode
       :remap '(js2-mode js-mode javascript-mode rjsx-mode)
       :url "https://github.com/tree-sitter/tree-sitter-javascript"
       :revision "f1e5a09"
       :source-dir "src"
       :ext "\\.js\\'"))

;; (add-to-list 'treesit-auto-recipe-list jr/js-tsauto-config)

              (global-treesit-auto-mode)

              )

(after! typescript-mode
        (setq typescript-indent-level 2)
        )

(defun jr/dired-untracked (dir)
  (interactive "DUntracked in directory: ")
  (cd (projectile-project-root))
  (switch-to-buffer (get-buffer-create "*untracked*"))
  (shell-command "git ls-files --others --exclude-standard | xargs ls -l" (current-buffer))
  (dired-mode dir)
  (set (make-local-variable 'dired-subdir-alist)
       (list (cons default-directory (point-min-marker)))))

(provide 'config)
;;; config.el
