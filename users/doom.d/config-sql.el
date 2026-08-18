;;; config-sql.el --- SQL configuration -*- lexical-binding: t; -*-

;; ============================================================================
;; SQL CONFIGURATION
;; ============================================================================
;; SQL mode, formatting, edit-indirect, and polymode support

;; ----------------------------------------------------------------------------
;; Edit-indirect for SQL strings
;; ----------------------------------------------------------------------------

(use-package! edit-indirect
  :defer t
  :commands (edit-indirect-region)
  :config
  ;; Custom function to edit SQL in string at point
  (defun jr/edit-sql-string-at-point ()
    "Edit the SQL string at point in a separate buffer with SQL mode."
    (interactive)
    (let* ((string-bounds (bounds-of-thing-at-point 'string))
           (start (if string-bounds (1+ (car string-bounds)) (region-beginning)))
           (end (if string-bounds (1- (cdr string-bounds)) (region-end))))
      (when (or string-bounds (use-region-p))
        (let ((buf (edit-indirect-region start end t)))
          (with-current-buffer buf
            (sql-mode)
            ;; Set up SQL formatting
            (when (fboundp 'sqlformat-on-save-mode)
              (sqlformat-on-save-mode -1)) ; Disable auto-format on save in indirect buffer
            (local-set-key (kbd "C-c C-f") 'jr/format-sql-buffer))))))

  ;; Helper function to format SQL in indirect buffer
  (defun jr/format-sql-buffer ()
    "Format the current SQL buffer."
    (interactive)
    (cond
     ((fboundp 'sqlformat-buffer) (sqlformat-buffer))
     ((fboundp 'format-all-buffer) (format-all-buffer))
     (t (message "No SQL formatter available")))))

;; ----------------------------------------------------------------------------
;; SQL Formatting
;; ----------------------------------------------------------------------------

(use-package! sqlformat
  :defer t
  :commands (sqlformat sqlformat-buffer sqlformat-region)
  :config
  ;; Configure SQL formatter (sql-formatter preferred for parity with the
  ;; nixvim conform setup: postgresql dialect, tab indentation)
  (cond
   ((executable-find "sql-formatter")
    (setq sqlformat-command 'sql-formatter
          sqlformat-args '("-l" "postgresql" "-c" "{\"useTabs\": true}")))
   ((executable-find "pg_format")
    (setq sqlformat-command 'pgformatter
          sqlformat-args '("-s" "2" "-g" "-U" "1")))
   ((executable-find "sqlformat")
    (setq sqlformat-command 'sqlformat
          sqlformat-args '("-r" "-k" "upper")))
   (t
    (message "No SQL formatter found. Install sql-formatter, pg_format, or sqlformat"))))

;; ----------------------------------------------------------------------------
;; SQL Helper Functions
;; ----------------------------------------------------------------------------

(defun jr/format-sql-string (sql-string)
  "Format SQL-STRING using available formatter."
  (with-temp-buffer
    (insert sql-string)
    (sql-mode)
    (cond
     ((fboundp 'sqlformat-buffer)
      (sqlformat-buffer)
      (buffer-string))
     ((fboundp 'format-all-buffer)
      (format-all-buffer)
      (buffer-string))
     (t
      (message "No SQL formatter available")
      nil))))

(defun jr/sql--string-region-at-point ()
  "Return (OPEN . CLOSE) bounds of the string enclosing point, or nil.
OPEN is the opening quote/backtick position and CLOSE is just past the
closing quote/backtick, matching `jr/go--raw-string-regions'. Handles Go
raw strings in both `go-mode' and `go-ts-mode' (where backticks are not
string syntax) and falls back to `bounds-of-thing-at-point' for ordinary
quoted strings."
  (or (and (derived-mode-p 'go-mode 'go-ts-mode)
           (cl-find-if (lambda (r) (and (<= (car r) (point)) (< (point) (cdr r))))
                       (jr/go--raw-string-regions)))
      (bounds-of-thing-at-point 'string)))

(defun jr/format-sql-string-at-point ()
  "Format the SQL string enclosing point, on demand.
Works with Go raw strings (`go-mode' and `go-ts-mode') and with ordinary
quoted strings. The formatted SQL is re-indented with tabs relative to
the line that opens the string, matching the project's Go SQL convention.
Uses the sql-formatter CLI (postgresql dialect, tab indentation)."
  (interactive)
  (cond
   ((not (executable-find "sql-formatter"))
    (message "sql-formatter not found on exec-path"))
   (t
    (let ((region (jr/sql--string-region-at-point)))
      (cond
       ((null region)
        (message "No string at point"))
       ((jr/go--format-sql-region (car region) (cdr region) t)
        (message "Formatted SQL string at point"))
       (t
        (message "SQL string at point already formatted")))))))

(defun jr/format-all-sql-strings ()
  "Format all SQL strings in the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((sql-pattern (rx (or "SELECT" "INSERT" "UPDATE" "DELETE" "CREATE" "DROP" "ALTER" "-- SQL"))))
      (while (re-search-forward sql-pattern nil t)
        (when (nth 3 (syntax-ppss)) ; Check if we're in a string
          (jr/format-sql-string-at-point)
          (forward-char))))))

;; Quick function to test SQL formatting
(defun jr/test-sql-formatter ()
  "Test if SQL formatter is working."
  (interactive)
  (let ((test-sql "SELECT * FROM users WHERE id = 1"))
    (message "Testing SQL formatter...")
    (message "Original: %s" test-sql)
    (message "Formatted: %s" (or (jr/format-sql-string test-sql) "Formatter not available"))))

;; ----------------------------------------------------------------------------
;; Go SQL String Formatting
;; ----------------------------------------------------------------------------

(after! go-mode
  ;; Go-specific on-demand SQL bindings. `s f' formats every SQL raw string in
  ;; the buffer; `s p' formats just the raw string the cursor is on. Both are
  ;; manual -- nothing runs on save.
  (map! :map go-mode-map
        :localleader
        (:prefix ("s" . "SQL")
         :desc "Format SQL strings (tabs)" "f" #'jr/go-format-sql-strings
         :desc "Format SQL string at point" "p" #'jr/format-sql-string-at-point)))

;; ----------------------------------------------------------------------------
;; Go Embedded SQL Formatting (tabs)
;; ----------------------------------------------------------------------------
;; Format SQL inside Go raw strings with the sql-formatter CLI and indent the
;; result purely with tabs, mirroring the nixvim FormatEmbedded behavior:
;; base indentation comes from the line that opens the string, nesting comes
;; from sql-formatter's useTabs output. Everything here is on-demand (invoked
;; via the Go localleader `s f'/`s p' bindings); nothing runs on save.

(defconst jr/go-sql-string-keyword-regexp
  "\\`[ \t\n]*\\(?:WITH\\|SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|ALTER\\|DROP\\)[ \t\n]"
  "Match string contents that start with a SQL keyword.
Mirrors the treesitter injection query used by the nixvim config.")

(defun jr/sql-formatter-format (text)
  "Format TEXT with the sql-formatter CLI (postgresql dialect, tab indent).
Return the formatted string, or nil when sql-formatter is unavailable or
fails."
  (when (executable-find "sql-formatter")
    (with-temp-buffer
      (insert text)
      (when (eq 0 (call-process-region (point-min) (point-max) "sql-formatter"
                                       t '(t nil) nil
                                       "-l" "postgresql"
                                       "-c" "{\"useTabs\": true}"))
        (buffer-string)))))

(defun jr/go--raw-string-regions ()
  "Return (OPEN . CLOSE) cons cells for Go raw strings in the buffer.
OPEN is the position of the opening backtick, CLOSE the position just
after the closing backtick. Regions are returned last-to-first so they
can be replaced without invalidating earlier positions.

In `go-ts-mode' the backtick has punctuation (not string) syntax, so
`syntax-ppss' cannot see raw strings there; use the tree-sitter parser
instead and fall back to the syntax-based scan in classic `go-mode'."
  (if (and (derived-mode-p 'go-ts-mode)
           (fboundp 'treesit-buffer-root-node)
           (treesit-language-available-p 'go))
      (let (regions)
        (dolist (capture (treesit-query-capture
                          (treesit-buffer-root-node 'go)
                          '((raw_string_literal) @str)))
          (let ((node (cdr capture)))
            (push (cons (treesit-node-start node) (treesit-node-end node))
                  regions)))
        (sort regions (lambda (a b) (> (car a) (car b)))))
    (let (regions)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "`" nil t)
          (let ((ppss (syntax-ppss)))
            (when (and (nth 3 ppss)
                       (eq (char-after (nth 8 ppss)) ?`))
              (let* ((open (nth 8 ppss))
                     (close (save-excursion
                              (goto-char open)
                              (ignore-errors (forward-sexp) (point)))))
                (when close
                  (push (cons open close) regions)
                  (goto-char close)))))))
      regions)))

(defun jr/go--format-sql-region (open close &optional force)
  "Format the SQL raw-string region between OPEN and CLOSE with tabs.
The string contents are the buffer text from (1+ OPEN) to (1- CLOSE)
\(i.e. OPEN and CLOSE point at the delimiter characters). The formatted
SQL is re-indented with tabs relative to the line that opens the string,
matching the nixvim conform behavior. Return non-nil when the buffer
text actually changed.

Unless FORCE is non-nil, only multi-line strings whose contents start
with a SQL keyword are formatted. FORCE bypasses that gate for explicit,
at-point formatting."
  (let ((content (buffer-substring-no-properties (1+ open) (1- close)))
        (case-fold-search nil))
    (when (or force
              (and (string-match-p "\n" content)
                   (string-match-p jr/go-sql-string-keyword-regexp content)))
      (let ((formatted (jr/sql-formatter-format content)))
        (when formatted
          (let* ((base (save-excursion
                         (goto-char open)
                         (forward-line 0)
                         (buffer-substring-no-properties
                          (point)
                          (progn (skip-chars-forward " \t") (point)))))
                 ;; Preserve a whitespace-only final line so the closing
                 ;; backtick keeps its own line when the original had one
                 ;; (same behavior as conform's injected formatter).
                 (postfix (and (string-match "\n\\([ \t]*\\)\\'" content)
                               (match-string 1 content)))
                 (lines (split-string (string-trim-right formatted) "\n"))
                 ;; First SQL line stays on the backtick line; the rest
                 ;; are indented with tabs relative to the opening line.
                 (replacement
                  (concat (car lines)
                          (when (cdr lines)
                            (concat "\n"
                                    (mapconcat (lambda (line)
                                                 (if (string-empty-p line)
                                                     line
                                                   (concat base line)))
                                               (cdr lines) "\n")))
                          (when postfix (concat "\n" postfix)))))
            (unless (string= replacement content)
              (save-excursion
                (delete-region (1+ open) (1- close))
                (goto-char (1+ open))
                (insert replacement))
              t)))))))

(defun jr/go-format-sql-strings ()
  "Format multi-line SQL raw strings in the current Go buffer with tabs.
Only strings whose contents start with a SQL keyword are formatted. This
is an on-demand command (Go localleader `s f'); it is NOT run on save.
Report what was (or was not) done and return the number of strings that
changed."
  (interactive)
  (let ((interactive-p (called-interactively-p 'interactive))
        (candidates 0)
        (changed 0))
    (cond
     ((not (derived-mode-p 'go-mode 'go-ts-mode))
      (when interactive-p
        (message "jr/go-format-sql-strings: not a Go buffer (%s)" major-mode)))
     ((not (executable-find "sql-formatter"))
      (when interactive-p
        (message "jr/go-format-sql-strings: sql-formatter not found on exec-path")))
     (t
      (let ((case-fold-search nil))
        (dolist (region (jr/go--raw-string-regions))
          (let* ((open (car region))
                 (close (cdr region))
                 (content (buffer-substring-no-properties (1+ open) (1- close))))
            (when (and (string-match-p "\n" content)
                       (string-match-p jr/go-sql-string-keyword-regexp content))
              (cl-incf candidates)
              ;; Already gated above, so force the region formatter.
              (when (jr/go--format-sql-region open close t)
                (cl-incf changed))))))
      (when interactive-p
        (cond
         ((zerop candidates)
          (message "No multi-line SQL strings found in buffer"))
         ((zerop changed)
          (message "%d SQL string(s) already formatted" candidates))
         (t
          (message "Formatted %d of %d SQL string(s)" changed candidates))))))
    changed))

;; NOTE: SQL formatting is on-demand only -- there is deliberately no
;; `before-save-hook' entry. Use the Go localleader bindings (`s f' to format
;; every SQL raw string in the buffer, `s p' to format just the one at point).

;; Same manual bindings for the tree-sitter Go mode
(map! :map go-ts-mode-map
      :localleader
      (:prefix ("s" . "SQL")
       :desc "Format SQL strings (tabs)" "f" #'jr/go-format-sql-strings
       :desc "Format SQL string at point" "p" #'jr/format-sql-string-at-point))

;; ----------------------------------------------------------------------------
;; SQL Mode Configuration
;; ----------------------------------------------------------------------------

(after! sql
  ;; Set default SQL product to PostgreSQL
  (setq sql-product 'postgres)

  ;; Improve SQL indentation
  (setq sql-indent-offset 2)

  ;; Enable SQL mode for common SQL file extensions
  (add-to-list 'auto-mode-alist '("\\.sql\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.psql\\'" . sql-mode))
  (add-to-list 'auto-mode-alist '("\\.plsql\\'" . sql-mode))

  ;; Configure SQL mode hooks
  (add-hook 'sql-mode-hook
            (lambda ()
              ;; Enable better indentation
              (when (fboundp 'sql-indent-mode)
                (sql-indent-mode))
              ;; Show whitespace in SQL files
              (setq show-trailing-whitespace t)))
  ;; NOTE: SQL formatting is intentionally on-demand, never on save. `.sql'
  ;; buffers are excluded from Doom's apheleia format-on-save via
  ;; `+format-on-save-disabled-modes' (sql-mode is a default member), and we no
  ;; longer enable `sqlformat-on-save-mode'. Format manually with `SPC c f'
  ;; (+format/buffer) or the SQL/Query bindings under `SPC c q'.

  ;; PostgreSQL specific settings
  (setq sql-postgres-program "psql")
  (setq sql-postgres-options '("-P" "pager=off")))

;; SQL indent configuration
(use-package! sql-indent
  :after sql
  :config
  (setq sql-indent-offset 2)
  ;; Only activate sql-indent-mode in actual SQL buffers, not in other modes
  (add-hook 'sql-mode-hook
            (lambda ()
              (when (eq major-mode 'sql-mode)
                (sql-indent-mode)))))

;; ----------------------------------------------------------------------------
;; Apheleia SQL Formatters
;; ----------------------------------------------------------------------------

(after! apheleia
  ;; Add SQL formatters to apheleia
  (setf (alist-get 'pgformatter apheleia-formatters)
        '("pg_format" "-"))
  (setf (alist-get 'sqlformat apheleia-formatters)
        '("sqlformat" "-r" "-k" "upper" "-"))
  (setf (alist-get 'sql-formatter apheleia-formatters)
        '("sql-formatter" "-l" "postgresql" "-c" "{\"useTabs\": true}"))

  ;; Associate SQL mode with formatter (sql-formatter preferred for parity
  ;; with the nixvim conform setup)
  (add-to-list 'apheleia-mode-alist
               (cons 'sql-mode
                     (cond
                      ((executable-find "sql-formatter") 'sql-formatter)
                      ((executable-find "pg_format") 'pgformatter)
                      ((executable-find "sqlformat") 'sqlformat)))))

;; ----------------------------------------------------------------------------
;; SQL String Highlighting
;; ----------------------------------------------------------------------------

(defface sql-string-face
  '((t :inherit font-lock-string-face :background "#1a1a2e"))
  "Face for SQL strings"
  :group 'sql)

(defun jr/highlight-sql-strings ()
  "Add highlighting for SQL keywords in strings."
  (interactive)
  (font-lock-add-keywords
   nil
   '(("\\(\"\\|'\\|`\\).*?\\(SELECT\\|INSERT\\|UPDATE\\|DELETE\\|CREATE\\|DROP\\|ALTER\\).*?\\1"
      0 'sql-string-face t))))

;; Enable SQL string highlighting in programming modes
(dolist (mode '(python-mode go-mode js-mode typescript-mode))
  (add-hook (intern (concat (symbol-name mode) "-hook"))
            'jr/highlight-sql-strings))

;; ----------------------------------------------------------------------------
;; Polymode Toggle (disabled polymode support preserved as comments)
;; ----------------------------------------------------------------------------

;; Toggle polymode for current buffer
(defun jr/toggle-polymode ()
  "Toggle polymode for SQL strings in current buffer."
  (interactive)
  (cond
   ((eq major-mode 'poly-python-sql-mode)
    (python-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'python-mode)
    (poly-python-sql-mode)
    (message "Polymode enabled for Python/SQL"))
   ((eq major-mode 'poly-js-sql-mode)
    (js-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'js-mode)
    (poly-js-sql-mode)
    (message "Polymode enabled for JS/SQL"))
   ((eq major-mode 'poly-ts-sql-mode)
    (typescript-mode)
    (message "Polymode disabled"))
   ((eq major-mode 'typescript-mode)
    (poly-ts-sql-mode)
    (message "Polymode enabled for TypeScript/SQL"))
   (t
    (message "Polymode not available for %s" major-mode))))

;; ----------------------------------------------------------------------------
;; Keybindings
;; ----------------------------------------------------------------------------

;; Using "c q" for SQL (query) to avoid conflict
(condition-case err
    (map! :leader
          (:prefix ("c q" . "SQL/Query")
           :desc "Edit SQL string" "e" #'jr/edit-sql-string-at-point
           :desc "Format SQL string" "f" #'jr/format-sql-string-at-point
           :desc "Format all SQL strings" "F" #'jr/format-all-sql-strings
           :desc "Toggle polymode" "p" #'jr/toggle-polymode))
  (error (message "Error setting up SQL keybindings: %s" err)))

(provide 'config-sql)
;;; config-sql.el ends here
