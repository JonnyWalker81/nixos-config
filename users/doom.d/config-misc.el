;;; config-misc.el --- description -*- lexical-binding: t; -*-

;; Misc Functions
(defun set-go-path ()
  (interactive)
  (setenv "GOPATH" (expand-file-name (vc-git-root (pwd))))
  )

(defun my-create-newline-and-enter-sexp (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode))

    (defun my-create-newline-and-enter-sexp-kotlin (&rest _ignored)
      "Open a new brace or bracket expression, with relevant newlines and indent. "
      (message "kotlin  begin")
      (newline)
      (indent-according-to-mode)
      (forward-line -1)
      (indent-according-to-mode)
      (message "kotlin  end")
      )

(sp-local-pair 'rust-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))
(sp-local-pair 'go-mode "{" nil :post-handlers '((my-create-newline-and-enter-sexp "RET")))

(setq gofmt-command "goimports")

(setq helm-ag-base-command "rg --vimgrep --no-heading")

;; Configure gopls to use the "testing" build tag
;; This tells gopls to ignore files with //go:build !testing or // +build !testing
(after! lsp-mode
  (setq lsp-go-build-flags ["-tags=testing"])
  ;; Also set GOFLAGS environment variable for consistency
  (setenv "GOFLAGS" "-tags=testing"))


;;(with-eval-after-load 'smartparens-config
;;  (show-smartparens-mode t)
;;  (add-hook 'prog-mode-hook 'enable-smartparens)
;;  (add-hook 'tide-mode-hook 'enable-smartparens)
;;  (add-hook 'web-mode-hook 'enable-smartparens)
;;  (smartparens-global-mode t)
;;  )

;;(add-hook 'after-load-functions 'enable-smartparens-functions)

(defun my-insert-copyright-header()
  "Insert copywrite header to file."
  (interactive)
  (insert "/*\n")
  (insert (format-time-string " * Copyright © 2002-%Y Bluebeam, Inc. All Rights Reserved.\n"))
  (insert " * Creator: Jonathan Rothberg\n")
  (insert " */\n")
  )

(defun my-insert-copyright-header-elm()
  "Insert copywrite header to file."
  (interactive)
  (insert "{-\n")
  (insert (format-time-string "  Copyright © 2002-%Y Bluebeam, Inc. All Rights Reserved.\n"))
  (insert "  Creator: Jonathan Rothberg\n")
  (insert "-}\n")
  )


(defun close-buffers-with-pattern (PATTERN)
  "Close buffers with a certain pattern"
  (interactive "sPattern:")
  (loop for buffer in (buffer-list)
        do (if (string-prefix-p PATTERN (buffer-name buffer))
               (kill-buffer buffer))))


(defun close-request-buffers()
  "Close *request* buffers"
  (interactive)
  (close-buffers-with-pattern " *request"))

(defun jr-setup-dev-workspace()
  (interactive)
  (split-window-horizontally)
  (+eshell/open-popup default-directory)
  )

;; Setup Prettier
;; (require 'prettier-js)

;; (add-hook 'js2-mode-hook 'prettier-js-mode)
;; (add-hook 'web-mode-hook 'prettier-js-mode)




(provide 'config-misc)
;;; config-misc.el ends here
