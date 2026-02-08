;;; config-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; ORG-MODE CONFIGURATION
;; ============================================================================
;; Org-mode, babel, presentations, and capture templates

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

  ;; org-reveal for reveal.js presentations
  (require 'org-re-reveal)
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@5")

  ;; org-presenterm for terminal presentations
  (require 'ox-presenterm)
  (add-to-list 'org-export-backends 'presenterm)

  ;; Capture templates
  (add-to-list 'org-capture-templates
               '("w" "Work Todo"  entry
                 (file "work.org")
                 "* TODO %T %?  :work:" :empty-lines 1))

  ;; Agenda custom commands
  (setq org-agenda-custom-commands
        '(("w" "Work Todos"
           ((agenda "")
            (tags-todo "work")
            ))
          ("a" "Work Todos"
           ((agenda "")
            (alltodo "")
            ))))

  ;; Verb (HTTP requests in org)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)

  ;; Presenterm export keybinding
  (map! :map org-mode-map
        :localleader
        (:prefix ("e" . "export")
         :desc "Export to Presenterm" "P" #'org-presenterm-export-to-markdown)))

;; ----------------------------------------------------------------------------
;; D2 Diagram Support
;; ----------------------------------------------------------------------------

(use-package! ob-d2
  :after org
  :config
  ;; Ensure D2 is available in org-babel
  (add-to-list 'org-babel-load-languages '(d2 . t))
  (org-babel-do-load-languages 'org-babel-load-languages org-babel-load-languages))

;; ----------------------------------------------------------------------------
;; Mermaid Diagram Support
;; ----------------------------------------------------------------------------

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

;; ----------------------------------------------------------------------------
;; Org-mode Hooks
;; ----------------------------------------------------------------------------

(add-hook 'org-mode-hook (lambda ()
                           (electric-indent-local-mode -1)
                           (setq org-adapt-indentation t)))

;; ----------------------------------------------------------------------------
;; Org with verb-command-map
;; ----------------------------------------------------------------------------

(use-package! org
  :mode ("\\.org\\'" . org-mode)
  :config (define-key org-mode-map (kbd "C-c C-r") verb-command-map))

(provide 'config-org)
;;; config-org.el ends here
