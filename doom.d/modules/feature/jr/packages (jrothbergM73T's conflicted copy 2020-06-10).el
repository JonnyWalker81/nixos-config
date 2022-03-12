;; -*- no-byte-compile: t; -*-
;;; feature/jr/packages.el

(package! emacs-snippets
  :recipe (:fetcher github
           :repo "hlissner/emacs-snippets"
           :files ("*")))

(package! deadgrep)

(package! cargo)

(package! clang-format)


