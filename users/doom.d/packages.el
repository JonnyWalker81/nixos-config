;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; This is where you install packages, by declaring them with the `package!'
;; macro, then running 'doom refresh' on the command line. You'll need to
;; restart Emacs for your changes to take effect! Or at least, run M-x
;; `doom/reload'.
;;
;; WARNING: Don't disable core packages listed in ~/.emacs.d/core/packages.el.
;; Doom requires these, and disabling them may have terrible side effects.
;;
;; Here are a couple examples:

;; All of Doom's packages are pinned to a specific commit, and updated from
;; release to release. To un-pin all packages and live on the edge, do:
                                        ;(setq doom-pinned-packages nil)

;; ...but to unpin a single package:
                                        ;(package! pinned-package :pin nil)

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
                                        ;(package! some-package)

;; To install a package directly from a particular repo, you'll need to specify
;; a `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
                                        ;(package! another-package
                                        ;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
                                        ;(package! this-package
                                        ;  :recipe (:host github :repo "username/repo"
                                        ;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, for whatever reason,
;; you can do so here with the `:disable' property:
                                        ;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
                                        ;(package! builtin-package :recipe (:nonrecursive t))
                                        ;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
                                        ;(package! builtin-package :recipe (:branch "develop"))
                                        ;
;; (unpin! tree-sitter)
;; (unpin! tree-sitter-langs)
;; (unpin! tree-sitter-indent)

;; (unpin! tuareg)
                                        ; (unpin! magit forge)
                                        ; (package! magit :pin "48818355728c48d986d74dde8b1e9fba25f0fd53")
                                        ; (package! forge :pin "fd586e106effa456defc94c6fd21a2f9ac185d09")
;; (package! flycheck)
(package! graphql-mode)
;; (package! rust-mode)
;; (package! flycheck-rust)
;; (package! lsp-mode)
;; (package! lsp-ui)
(package! dockerfile-mode)
(package! protobuf-mode)
;; (package! prettier-js-mode)

;; (package! flycheck-inline)

(package! org-super-agenda)

;; rjsx-mode, typescript-mode, web-mode, tide, company, yasnippet, import-js, prettier-js
;; (package! rjsx-mode)
;; (package! import-mode)
;; (package! prettier-js)
(package! autopair)
(package! elfeed-org)
(package! elfeed-goodies)

(package! modus-themes)
(package! ef-themes)

(package! go-tag)
(package! exec-path-from-shell)

;; (package! vertico)
;; (package! orderless)
;; (package! marginalia)

;; (package! selectrum)

(package! visual-regexp)
(package! visual-regexp-steroids)

                                        ; (package! magit-circleci)
(package! lusty-explorer)

(package! smooth-scrolling)
(package! super-save)

(package! zig-mode)

(package! affe)

(package! blamer)

(package! eyebrowse)

(package! go-playground)
;; (package! go-mode)

(package! tree-sitter)
(package! tree-sitter-langs)

;; (package! flycheck-golangci-lint)

(package! just-mode)

(package! denote)

(package! window-stool :recipe (:host github :repo "jaszhe/window-stool" :files ("*.el")))

(package! dap-mode)

(package! fold-this)
;; (package! caml-mode)

(package! dired-preview)

(package! edbi)
(package! lispy)
(package! elisp-autofmt)

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el" "dist")))

;; (package! gleam-mode
;;   :recipe (:host github :repo "gleam-lang/gleam-mode" :files ("*.el")))

(package! gleam-mode
  :recipe (:local-repo "~/Repositories/gleam-mode"))

(package! tree-sitter-indent
  :recipe (:local-repo "~/Repositories/tree-sitter-indent")
  )
;; (straight-use-package
;; '(tree-sitter-indent :type git
;; :repo "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git"
;; :branch "main"
;; :files ("tree-sitter-indent.el")))

                                        ; (package! code-review)

;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  ;; :repo "manateelazycat/lsp-bridge"
;;                  :repo "JonnyWalker81/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet)
;;   (package! tempel)
;;   )

;; (package! indent-guide
;;   :recipe (:host github
;;            :repo "zk-phi/indent-guide"
;;            :branch "master"
;;            :files ("*.el")
;;            ))

(package! highlight-indent-guides)

(package! shfmt
  :recipe (:host github
           :repo "purcell/emacs-shfmt"
           :branch "master"
           :files ("*.el")
           )
  )

(package! tokyo-theme
  :recipe (:host github
           :repo "rawleyfowler/tokyo-theme.el"
           :branch "main"
           :files ("*.el")
           )
  )

;; (package! rose-pine-doom-emacs
;;           :recipe (:host github
;;                    :repo "donniebreve/rose-pine-doom-emacs"
;;                    :branch "main"
;;                    :files ("*.el")
;;                    )
;;           )

(package! treesit-auto)

(package! jenkinsfile-mode)

(package! ox-pandoc)
(package! ox-reveal)

(package! jinx)

(package! verb)
(package! org-present)

(package! gptel :recipe (:nonrecursive t))

(package! elysium)

;;; packages.el ends here
