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
  :filters-alist '((:filter-parse-tree . org-presenterm-filter-parse-tree))
  :translate-alist '((src-block . org-presenterm-src-block)
                    (special-block . org-presenterm-special-block)
                    (headline . org-presenterm-headline)
                    (plain-list . org-presenterm-plain-list)
                    (item . org-presenterm-item)
                    (link . org-presenterm-link)
                    (keyword . org-presenterm-keyword)
                    (paragraph . org-presenterm-paragraph)
                    (export-block . org-presenterm-export-block))
  :menu-entry
  '(?P "Export to Presenterm Markdown"
       ((?P "To file" org-presenterm-export-to-markdown)
        (?p "To temporary buffer" org-presenterm-export-to-buffer))))

;; Skip reveal.js related keywords
(defun org-presenterm-keyword (keyword contents info)
  "Handle Org KEYWORD elements for Presenterm export."
  (let ((key (org-element-property :key keyword))
        (value (org-element-property :value keyword)))
    ;; Skip reveal.js specific keywords
    (cond
     ((string-match-p "^REVEAL_" key) nil)
     ((string-match-p "^HTML_" key) nil)
     ((string= key "ATTR_HTML") nil)
     ((string= key "OPTIONS") nil)
     (t (org-md-keyword keyword contents info)))))

;; Skip HTML export blocks
(defun org-presenterm-export-block (export-block contents info)
  "Transcode an EXPORT-BLOCK element for Presenterm export."
  (let ((type (org-element-property :type export-block)))
    (if (string= type "HTML")
        nil  ; Skip HTML export blocks
      (org-export-with-backend 'md export-block contents info))))

;; Add metadata and process document structure
(defun org-presenterm-filter-parse-tree (tree backend info)
  "Add presenterm-specific metadata and structure to the parse TREE."
  (let* ((title (org-export-data (plist-get info :title) info))
         (author (org-export-data (plist-get info :author) info))
         (date (org-export-data (plist-get info :date) info))
         (meta (format "---\ntitle: \"%s\"\nauthor: \"%s\"\ndate: \"%s\"\nalign: center\nvertical_align: middle\n---\n\n" 
                       (or title "Untitled Presentation") 
                       (or author "Anonymous") 
                       (or date ""))))
    ;; Remove any HTML attributes or Reveal.js specific properties
    (org-element-map tree '(keyword special-block)
      (lambda (element)
        (let ((type (org-element-type element)))
          (cond
           ((eq type 'keyword)
            (let ((key (org-element-property :key element)))
              (when (or (string-match-p "^REVEAL_" key)
                        (string-match-p "^HTML_" key)
                        (string= key "ATTR_HTML")
                        (string= key "OPTIONS"))
                (org-element-extract-element element))))
           ((eq type 'special-block)
            (let ((block-type (org-element-property :type element)))
              (when (string= block-type "REVEAL_HTML")
                (org-element-extract-element element)))))))
      info)
    
    ;; Add metadata at the beginning
    (org-element-map tree 'section
      (lambda (section)
        (when (= (or (org-element-property :begin section) 0) 1)
          (org-element-put-property 
           section :content 
           (concat meta (org-element-property :content section)))))
      info t)
    tree))

;; Transform headline elements into slide separators
(defun org-presenterm-headline (headline contents info)
  "Transcode a HEADLINE element from Org to Presenterm Markdown."
  ;; Remove HTML attributes from headline
  (org-element-map headline '(node-property)
    (lambda (prop)
      (when (string= (org-element-property :key prop) "REVEAL_EXTRA_ATTR")
        (org-element-extract-element prop))))
  
  (let* ((level (org-element-property :level headline))
         (title (org-export-data (org-element-property :title headline) info))
         (properties (org-element-property :properties headline))
         (notes (and properties (cdr (assoc "NOTES" properties))))
         (prev-headline (org-export-get-previous-element headline info))
         (result ""))
    
    ;; Use explicit <!-- end_slide --> for slide separators
    (when prev-headline
      (setq result (concat result "<!-- end_slide -->\n\n")))
    
    ;; For level 1 headings, always start a new slide
    (when (and level (= level 1))
      ;; Skip adding end_slide for the first slide, which already has one after the frontmatter
      (unless (and (not prev-headline) (= level 1))
        (setq result (concat result "<!-- end_slide -->\n\n"))))
    
    ;; Format the headline
    (setq result (concat result (format "# %s\n\n" title)))
    
    ;; Add the contents (filter out HTML attributes)
    (when contents
      (setq result (concat result (org-presenterm-clean-html-attrs contents))))
    
    ;; Add presenter notes if available
    (when notes
      (setq result (concat result "\n\n>! " notes)))
    
    result))

;; Function to clean HTML attributes from content
(defun org-presenterm-clean-html-attrs (content)
  "Remove HTML attributes and styling from CONTENT."
  (with-temp-buffer
    (insert content)
    
    ;; Remove ATTR_HTML lines
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+ATTR_HTML:.*\n" nil t)
      (replace-match ""))
    
    ;; Remove REVEAL_HTML tags and content
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*#\\+REVEAL_HTML:.*\n" nil t)
      (replace-match ""))
    
    ;; Remove any potential stray HTML formatting
    (goto-char (point-min))
    (while (re-search-forward "</?[a-zA-Z][^>]*>" nil t)
      (replace-match ""))
    
    (buffer-string)))

;; Custom paragraph handler to center content
(defun org-presenterm-paragraph (paragraph contents info)
  "Transcode a PARAGRAPH element from Org to Presenterm Markdown."
  ;; Process paragraphs normally, centering is handled in the YAML front matter
  (org-md-paragraph paragraph contents info))

;; Custom item handler for lists
(defun org-presenterm-item (item contents info)
  "Transcode an ITEM element from Org to Presenterm Markdown."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
         (parent (org-export-get-parent item))
         (parent-parent (org-export-get-parent parent))
         (incremental nil)
         (bullet "*"))  ;; Use asterisk (*) for bullets instead of dash (-)
    
    ;; Check for incremental property
    (let ((headline-parent parent-parent))
      (while (and headline-parent (not incremental))
        (when (eq (org-element-type headline-parent) 'headline)
          (setq incremental 
                (or (member "incremental" (org-element-property :tags headline-parent))
                    (and (org-element-property :properties headline-parent)
                         (equal "t" (cdr (assoc "INCREMENTAL" (org-element-property :properties headline-parent))))))))
        (setq headline-parent (org-export-get-parent headline-parent))))
    
    ;; Use +! for incremental lists
    (when incremental
      (setq bullet "+!"))
    
    ;; Get the item's content and clean it up
    (let ((clean-contents (org-presenterm-handle-item-content contents)))
      (concat bullet " " clean-contents))))

;; Handle links to make them compatible with Presenterm
(defun org-presenterm-link (link desc info)
  "Transcode a LINK object from Org to Presenterm Markdown."
  (let* ((type (org-element-property :type link))
         (path (org-element-property :path link))
         (raw-link (org-element-property :raw-link link)))
    ;; Special handling for images 
    (if (and (string= type "file")
             (string-match-p "\\(?:jpg\\|jpeg\\|png\\|gif\\|svg\\)\\'" path))
        ;; Standard markdown image syntax - Presenterm supports this directly
        (format "![%s](%s)" 
                (or desc (file-name-nondirectory path))
                path)
      ;; For other links, use standard markdown format
      (org-md-link link desc info))))

;; Helper function to process item content
(defun org-presenterm-handle-item-content (contents)
  "Process item content to ensure proper formatting."
  (with-temp-buffer
    (insert contents)
    
    ;; Replace any remaining bullet points with asterisks
    (goto-char (point-min))
    (while (re-search-forward "^[ \t]*-[ \t]+" nil t)
      (replace-match "* "))
    
    ;; Find and extract code blocks
    (goto-char (point-min))
    (let ((code-blocks '())
          (counter 0))
      ;; Find all code blocks and replace them with placeholders
      (while (re-search-forward "```\\(.*\\)\n\\(\\(.\\|\n\\)*?\\)```" nil t)
        (let ((lang (match-string 1))
              (code (match-string 2))
              (placeholder (format "@@CODE_BLOCK_%d@@" counter)))
          (push (cons placeholder (format "```%s\n%s```" lang code)) code-blocks)
          (replace-match placeholder)
          (setq counter (1+ counter))))
      
      ;; Get the processed content
      (let ((processed-content (buffer-string)))
        ;; Remove HTML attributes
        (setq processed-content (org-presenterm-clean-html-attrs processed-content))
        
        ;; Now, replace the placeholders with the original code blocks
        (dolist (block code-blocks)
          (setq processed-content 
                (replace-regexp-in-string (car block) (cdr block) processed-content t t)))
        processed-content))))

;; Process lists for incremental display
(defun org-presenterm-plain-list (plain-list contents info)
  "Transcode a PLAIN-LIST element from Org to Presenterm Markdown."
  ;; Return contents with proper spacing
  (concat "\n" contents "\n"))

;; Handle special blocks
(defun org-presenterm-special-block (special-block contents info)
  "Transcode a SPECIAL-BLOCK element from Org to Presenterm Markdown."
  (let ((block-type (org-element-property :type special-block)))
    (cond
     ((string= block-type "NOTES")
      (format ">! %s" contents))
     ((string-match-p "^REVEAL_" block-type) nil) ; Skip reveal.js specific blocks
     (t (org-md-special-block special-block contents info)))))

;; Handle src blocks with proper syntax highlighting
(defun org-presenterm-src-block (src-block contents info)
  "Transcode a SRC-BLOCK element from Org to Presenterm Markdown."
  (let* ((lang (org-element-property :language src-block))
         (code (org-export-format-code-default src-block info))
         (parent-element (org-export-get-parent-element src-block)))
    
    ;; Check if inside a list item
    (if (org-presenterm-in-list-p src-block)
        ;; For code blocks inside lists, ensure they're moved outside the list
        (format "\n\n```%s\n%s\n```\n\n" (or lang "") code)
      ;; Standard fenced code block for normal blocks
      (format "\n```%s\n%s\n```\n" (or lang "") code))))

;; Helper function to check if element is in a list
(defun org-presenterm-in-list-p (element)
  "Check if ELEMENT is inside a list."
  (let ((parent element))
    (while (and parent (not (eq (org-element-type parent) 'plain-list)))
      (setq parent (org-export-get-parent parent)))
    parent))

;; Function to export to a file
(defun org-presenterm-export-to-markdown (&optional async subtreep visible-only)
  "Export current buffer to a Presenterm Markdown file."
  (interactive)
  (let ((outfile (org-export-output-file-name ".md" subtreep)))
    (org-export-to-file 'presenterm outfile async subtreep visible-only nil nil
                       (lambda (file) 
                         (with-current-buffer (find-file-noselect file)
                           ;; Ensure we have proper slide separators
                           (goto-char (point-min))
                           
                           ;; Add first slide separator after YAML frontmatter
                           (goto-char (point-min))
                           (when (re-search-forward "^---$" nil t 2) ; End of YAML frontmatter
                             (forward-line 1)
                             (insert "<!-- end_slide -->\n\n"))
                           
                           ;; Remove any ATTR_HTML, REVEAL_*, or HTML_* lines
                           (goto-char (point-min))
                           (while (re-search-forward "^[ \t]*#\\+\\(ATTR_HTML\\|REVEAL_[^:]*\\|HTML_[^:]*\\):.*\n" nil t)
                             (replace-match ""))
                           
                           ;; Move code blocks out of lists
                           (goto-char (point-min))
                           (while (re-search-forward "^\\([ \t]*[*+!][ \t]+.*\\)\n\\([ \t]*```.*\\)" nil t)
                             (replace-match "\\1\n\n\\2"))
                           
                           ;; Replace any remaining dash bullets with asterisks
                           (goto-char (point-min))
                           (while (re-search-forward "^[ \t]*-[ \t]+" nil t)
                             (replace-match "* "))
                           
                           ;; Make sure code blocks have blank lines before and after
                           (goto-char (point-min))
                           (while (re-search-forward "\\([^ \t\n]\\)\n[ \t]*```" nil t)
                             (replace-match "\\1\n\n```"))
                           
                           (goto-char (point-min))
                           (while (re-search-forward "```\\(.*\\)\n\\([^\n]\\)" nil t)
                             (replace-match "```\\1\n\n\\2"))
                           
                           ;; Remove any HTML/CSS styling that may have snuck through
                           (goto-char (point-min))
                           (while (re-search-forward "<[^>]+>" nil t)
                             (let ((tag (match-string 0)))
                               ;; Don't remove HTML comments (which are used for slide separators)
                               (unless (string-match-p "<!--.*-->" tag)
                                 (replace-match ""))))
                           
                           ;; Make sure each level 1 heading has a slide separator
                           (goto-char (point-min))
                           (while (re-search-forward "^# " nil t)
                             (let ((heading-start (match-beginning 0)))
                               ;; Check if there's a slide separator before this heading
                               (save-excursion
                                 (goto-char heading-start)
                                 (forward-line -3) ; Look a few lines back
                                 (let ((has-separator (re-search-forward "<!-- end_slide -->" heading-start t)))
                                   (unless has-separator
                                     ;; Insert a separator if one doesn't exist
                                     (goto-char heading-start)
                                     (insert "<!-- end_slide -->\n\n"))))))
                           
                           ;; Add a final slide separator at the end of the file if needed
                           (goto-char (point-max))
                           (forward-line -1)
                           (unless (looking-at "<!-- end_slide -->")
                             (goto-char (point-max))
                             (insert "\n<!-- end_slide -->\n"))
                           
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
        ;; Ensure we have proper slide separators
        (goto-char (point-min))
        
        ;; Add first slide separator after YAML frontmatter
        (goto-char (point-min))
        (when (re-search-forward "^---$" nil t 2) ; End of YAML frontmatter
          (forward-line 1)
          (insert "<!-- end_slide -->\n\n"))
        
        ;; Remove any ATTR_HTML, REVEAL_*, or HTML_* lines
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*#\\+\\(ATTR_HTML\\|REVEAL_[^:]*\\|HTML_[^:]*\\):.*\n" nil t)
          (replace-match ""))
        
        ;; Move code blocks out of lists
        (goto-char (point-min))
        (while (re-search-forward "^\\([ \t]*[*+!][ \t]+.*\\)\n\\([ \t]*```.*\\)" nil t)
          (replace-match "\\1\n\n\\2"))
        
        ;; Replace any remaining dash bullets with asterisks
        (goto-char (point-min))
        (while (re-search-forward "^[ \t]*-[ \t]+" nil t)
          (replace-match "* "))
        
        ;; Make sure code blocks have blank lines before and after
        (goto-char (point-min))
        (while (re-search-forward "\\([^ \t\n]\\)\n[ \t]*```" nil t)
          (replace-match "\\1\n\n```"))
        
        (goto-char (point-min))
        (while (re-search-forward "```\\(.*\\)\n\\([^\n]\\)" nil t)
          (replace-match "```\\1\n\n\\2"))
        
        ;; Remove any HTML/CSS styling that may have snuck through
        (goto-char (point-min))
        (while (re-search-forward "<[^>]+>" nil t)
          (let ((tag (match-string 0)))
            ;; Don't remove HTML comments (which are used for slide separators)
            (unless (string-match-p "<!--.*-->" tag)
              (replace-match ""))))
        
        ;; Make sure each level 1 heading has a slide separator
        (goto-char (point-min))
        (while (re-search-forward "^# " nil t)
          (let ((heading-start (match-beginning 0)))
            ;; Check if there's a slide separator before this heading
            (save-excursion
              (goto-char heading-start)
              (forward-line -3) ; Look a few lines back
              (let ((has-separator (re-search-forward "<!-- end_slide -->" heading-start t)))
                (unless has-separator
                  ;; Insert a separator if one doesn't exist
                  (goto-char heading-start)
                  (insert "<!-- end_slide -->\n\n"))))))
        
        ;; Add a final slide separator at the end of the file if needed
        (goto-char (point-max))
        (forward-line -1)
        (unless (looking-at "<!-- end_slide -->")
          (goto-char (point-max))
          (insert "\n<!-- end_slide -->\n"))))))

(provide 'ox-presenterm)
;;; ox-presenterm.el ends here