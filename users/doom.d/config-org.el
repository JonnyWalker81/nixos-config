;;; config-org.el --- Org-mode configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; ORG-MODE CONFIGURATION
;; ============================================================================
;; Org-mode, babel, presentations, and capture templates

(after! org

  (require 'ox-md)

  (defun jr/org--copy-markdown-to-clipboard (markdown)
    "Copy MARKDOWN to kill ring and system clipboard."
    (kill-new markdown)
    (when (fboundp 'gui-set-selection)
      (gui-set-selection 'CLIPBOARD markdown))
    (message "Copied Markdown to clipboard"))

  (defun jr/org--show-markdown-buffer (markdown)
    "Display MARKDOWN in a dedicated buffer."
    (let ((buf (get-buffer-create "*Org Markdown Export*")))
      (with-current-buffer buf
        (erase-buffer)
        (insert markdown)
        (goto-char (point-min))
        (if (fboundp 'markdown-mode)
            (markdown-mode)
          (text-mode)))
      (pop-to-buffer buf)))

  (defun jr/org--markdown-to-slack (markdown)
    "Convert Markdown text in MARKDOWN to Slack mrkdwn."
    (let ((out markdown))
      (setq out (replace-regexp-in-string "\\*\\*\\([^*\\n]+\\)\\*\\*" "*\\1*" out))
      (setq out (replace-regexp-in-string "~~\\([^~\\n]+\\)~~" "~\\1~" out))
      (setq out (replace-regexp-in-string "^\\s-*#+\\s-+\\(.+\\)$" "*\\1*" out))
      (setq out (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\([^)]+\\))" "<\\2|\\1>" out))
      (setq out (replace-regexp-in-string "```[A-Za-z0-9_+-]*\\n" "```\n" out))
      out))

  (defun jr/org-export-buffer-to-markdown-buffer ()
    "Export current Org buffer to Markdown in a new buffer."
    (interactive)
    (jr/org--show-markdown-buffer (org-export-as 'md nil nil nil nil)))

  (defun jr/org-export-buffer-to-markdown-clipboard ()
    "Export current Org buffer to Markdown and copy to clipboard."
    (interactive)
    (jr/org--copy-markdown-to-clipboard (org-export-as 'md nil nil nil nil)))

  (defun jr/org-export-region-to-markdown-buffer (beg end)
    "Export Org region from BEG to END to Markdown in a new buffer."
    (interactive "r")
    (unless (use-region-p)
      (user-error "Select a region first"))
    (jr/org--show-markdown-buffer
     (org-export-string-as (buffer-substring-no-properties beg end) 'md t)))

  (defun jr/org-export-region-to-markdown-clipboard (beg end)
    "Export Org region from BEG to END to Markdown and copy to clipboard."
    (interactive "r")
    (unless (use-region-p)
      (user-error "Select a region first"))
    (jr/org--copy-markdown-to-clipboard
     (org-export-string-as (buffer-substring-no-properties beg end) 'md t)))

  (defun jr/org-export-buffer-to-slack-clipboard ()
    "Export current Org buffer to Slack mrkdwn and copy to clipboard."
    (interactive)
    (jr/org--copy-markdown-to-clipboard
     (jr/org--markdown-to-slack (org-export-as 'md nil nil nil nil))))

  (defun jr/org-export-region-to-slack-clipboard (beg end)
    "Export Org region from BEG to END to Slack mrkdwn and copy to clipboard."
    (interactive "r")
    (unless (use-region-p)
      (user-error "Select a region first"))
    (jr/org--copy-markdown-to-clipboard
     (jr/org--markdown-to-slack
      (org-export-string-as (buffer-substring-no-properties beg end) 'md t))))

  (defun jr/org-export-section-to-slack-clipboard ()
    "Export current Org section/subtree to Slack mrkdwn and copy to clipboard."
    (interactive)
    (when (org-before-first-heading-p)
      (user-error "Move point to an Org heading/section first"))
    (save-excursion
      (unless (org-at-heading-p)
        (org-back-to-heading t))
      (jr/org--copy-markdown-to-clipboard
       (jr/org--markdown-to-slack (org-export-as 'md t nil nil nil)))))

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

  ;; Verb (HTTP requests in org)
  (define-key org-mode-map (kbd "C-c C-r") verb-command-map)
  (define-key org-mode-map (kbd "C-c C-w") #'org-refile)

  ;; Presenterm export keybinding
  (map! :map org-mode-map
        :localleader
        (:prefix ("e" . "export")
         :desc "Org buffer -> Markdown buffer" "m" #'jr/org-export-buffer-to-markdown-buffer
         :desc "Org buffer -> Markdown clipboard" "M" #'jr/org-export-buffer-to-markdown-clipboard
         :desc "Org region -> Markdown buffer" "r" #'jr/org-export-region-to-markdown-buffer
         :desc "Org region -> Markdown clipboard" "R" #'jr/org-export-region-to-markdown-clipboard
         :desc "Org buffer -> Slack clipboard" "s" #'jr/org-export-buffer-to-slack-clipboard
         :desc "Org section -> Slack clipboard" "S" #'jr/org-export-section-to-slack-clipboard
         :desc "Org region -> Slack clipboard" "x" #'jr/org-export-region-to-slack-clipboard
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
