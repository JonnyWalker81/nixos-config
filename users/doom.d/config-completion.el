;;; config-completion.el --- Completion framework configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; COMPLETION FRAMEWORK CONFIGURATION
;; ============================================================================
;; Vertico, Corfu, Consult, Marginalia, Embark, Cape, and Affe configuration

;; ----------------------------------------------------------------------------
;; Vertico (vertical completion)
;; ----------------------------------------------------------------------------

(use-package! vertico
  :custom
  (vertico-count 15)    ; show 15 candidates at once
  (vertico-resize t)    ; grow and shrink the minibuffer
  (vertico-cycle t)     ; wrap around at the top/bottom
  :custom-face
  (vertico-current ((t (:background "#3a3f5a"))))
  :init
  (vertico-mode))

(after! vertico
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion)))))

;; ----------------------------------------------------------------------------
;; Corfu (in-buffer completion)
;; ----------------------------------------------------------------------------

(use-package! corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 3)        ; Require 3 chars instead of 2
  (corfu-auto-delay 0.3)       ; Wait 300ms before showing completions
  (corfu-cycle t)
  (corfu-separator ?\s)
  (corfu-quit-no-match 'separator)  ; Don't spam when no match
  (corfu-preview-current nil))      ; Don't preview - reduces redisplay

;; ----------------------------------------------------------------------------
;; Marginalia (rich annotations)
;; ----------------------------------------------------------------------------

(use-package! marginalia
  :init
  (marginalia-mode))

;; ----------------------------------------------------------------------------
;; Consult (completing-read enhancements)
;; ----------------------------------------------------------------------------

(use-package! consult
  :after vertico)

;; Consult toggle preview
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
;; Embark (contextual actions)
;; ----------------------------------------------------------------------------

(use-package! embark
  :bind
  (("C-." . embark-act)    ; pick action for candidate
   ("M-." . embark-dwim))) ; do-what-I-mean

;; ----------------------------------------------------------------------------
;; Cape (completion-at-point extensions)
;; ----------------------------------------------------------------------------

(use-package! cape
  :init
  ;; add common backends
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; ----------------------------------------------------------------------------
;; Affe (async fuzzy finder)
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
;; Savehist (minibuffer history)
;; ----------------------------------------------------------------------------

(use-package savehist
  :init
  (savehist-mode))

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

(map! :leader :desc "find in buffer" "f /" #'consult-line)

(provide 'config-completion)
;;; config-completion.el ends here
