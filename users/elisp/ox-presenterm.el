;;; ox-presenterm.el --- Export Org mode to Presenterm Markdown -*- lexical-binding: t; -*-

;; Author: Jonathan Rothberg
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (org "9.4"))
;; Keywords: org, markdown, presentation, presenterm
;; URL: https://github.com/mfontanini/presenterm

;;; Commentary:

;; This library provides functionality to export Org mode documents to 
;; Presenterm-compatible Markdown format.
;; 
;; Presenterm is a terminal-based presentation tool that uses markdown
;; files with special syntax: https://github.com/mfontanini/presenterm

;;; Code:

(require 'ox-md)

(org-export-define-derived-backend 'presenterm 'md
  :filters-alist '((:filter-parse-tree . org-presenterm-filter-parse-tree)
                   (:filter-headline . org-presenterm-filter-headline)
                   (:filter-plain-list . org-presenterm-filter-plain-list))
  :translate-alist '((src-block . org-presenterm-src-block)
                    (special-block . org-presenterm-special-block))
  :menu-entry
  '(?P "Export to Presenterm Markdown"
       ((?P "To file" org-presenterm-export-to-markdown)
        (?p "To temporary buffer" org-presenterm-export-to-buffer))))

;; Add metadata and process document structure
(defun org-presenterm-filter-parse-tree (tree backend info)
  "Add presenterm-specific metadata and structure to the parse TREE."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (org-export-data (plist-get info :author) info))
         (date (org-export-data (plist-get info :date) info))
         (meta (format "---\ntitle: \"%s\"\nauthor: \"%s\"\ndate: \"%s\"\n---\n\n" 
                       (or title "Untitled Presentation") 
                       (or author "Anonymous") 
                       (or date ""))))
    ;; Add metadata at the beginning
    (org-element-map tree 'section
      (lambda (section)
        (when (= (or (org-element-property :begin section) 0) 1)
          (org-element-put-property 
           section :content 
           (concat meta (org-element-property :content section)))))
      info t)
    tree))

;; Handle headlines as slide separators
(defun org-presenterm-filter-headline (headline backend info)
  "Convert org HEADLINE to presenterm slide separator and handle presenter notes."
  (when (eq backend 'presenterm)
    (let* ((level (org-element-property :level headline))
           (properties (org-element-property :properties headline))
           (notes (and properties (cdr (assoc "NOTES" properties))))
           (result headline))
      ;; Add slide separator for level 1-2 headlines
      (when (and level (<= level 2))
        (setq result (concat "---\n\n" result)))
      ;; Add presenter notes if available
      (when notes
        (setq result (concat result "\n\n>! " notes)))
      result)))

;; Process lists for incremental display
(defun org-presenterm-filter-plain-list (plain-list backend info)
  "Process PLAIN-LIST for presenterm-specific features like incremental lists."
  (when (eq backend 'presenterm)
    (let* ((parent (org-export-get-parent plain-list))
           (incremental nil))
      ;; Check if parent headline has the 'incremental' tag
      (while (and parent (not incremental))
        (when (eq (org-element-type parent) 'headline)
          (setq incremental 
                (or (member "incremental" (org-element-property :tags parent))
                    (and (org-element-property :properties parent)
                         (equal "t" (cdr (assoc "INCREMENTAL" (org-element-property :properties parent))))))))
        (setq parent (org-export-get-parent parent)))
      
      ;; If incremental, transform the list items to use +! markers
      (when incremental
        (let ((content (org-element-property :content plain-list)))
          (org-element-put-property 
           plain-list :content 
           (replace-regexp-in-string 
            "^\\([ \t]*\\)\\([-+*]\\|[0-9]+[.)]\\)\\([ \t]+\\)"
            "\\1+!\\3"
            content))))
      plain-list)))

;; Handle special blocks
(defun org-presenterm-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Presenterm Markdown."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string= block-type "NOTES")
      (format ">! %s" contents))
     (t (org-md-special-block special-block contents info)))))

;; Handle src blocks with proper syntax highlighting
(defun org-presenterm-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Presenterm Markdown."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info)))
    (format "```%s\n%s\n```" (or lang "") code)))

;; Function to export to a file
(defun org-presenterm-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Presenterm Markdown file."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'presenterm outfile async subtreep visible-only nil nil
                       (lambda (file) 
                         (with-current-buffer (find-file-noselect file)
                           ;; Ensure we have at least one slide separator at the beginning
                           (goto-char (point-min))
                           (unless (re-search-forward "^---$" nil t 2) ; Skip the metadata block
                             (goto-char (point-min))
                             (re-search-forward "^---$" nil t 2) ; Find end of metadata
                             (forward-line 1)
                             (insert "---\n\n"))
                           ;; Clean up any consecutive slide separators
                           (goto-char (point-min))
                           (while (re-search-forward "^---\n\\([ \t\n]*\\)---$" nil t)
                             (replace-match "---" nil nil))
                           (save-buffer))
                         file))))

;; Function to export to a buffer
(defun org-presenterm-export-to-buffer (&optional async subtreep visible-only)
  "Export current buffer to a Presenterm Markdown buffer."
  (interactive)
  (org-export-to-buffer 'presenterm "*Org Presenterm Export*"
    async subtreep visible-only nil nil
    (lambda (buf)
      (with-current-buffer buf
        ;; Ensure we have at least one slide separator at the beginning
        (goto-char (point-min))
        (unless (re-search-forward "^---$" nil t 2) ; Skip the metadata block
          (goto-char (point-min))
          (re-search-forward "^---$" nil t 2) ; Find end of metadata
          (forward-line 1)
          (insert "---\n\n"))
        ;; Clean up any consecutive slide separators
        (goto-char (point-min))
        (while (re-search-forward "^---\n\\([ \t\n]*\\)---$" nil t)
          (replace-match "---" nil nil))))))

(provide 'ox-presenterm)
;;; ox-presenterm.el ends here