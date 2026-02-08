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
  ;; Configure SQL formatter (pgformatter, sqlformat, or sql-formatter)
  ;; Try to detect available formatter
  (cond
   ((executable-find "pg_format")
    (setq sqlformat-command 'pgformatter
          sqlformat-args '("-s" "2" "-g" "-U" "1")))
   ((executable-find "sqlformat")
    (setq sqlformat-command 'sqlformat
          sqlformat-args '("-r" "-k" "upper")))
   ((executable-find "sql-formatter")
    (setq sqlformat-command 'sql-formatter
          sqlformat-args '("-l" "postgresql")))
   (t
    (message "No SQL formatter found. Install pg_format, sqlformat, or sql-formatter"))))

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

(defun jr/format-sql-string-at-point ()
  "Format the SQL string at point."
  (interactive)
  (save-excursion
    (let* ((string-bounds (bounds-of-thing-at-point 'string))
           (start (if string-bounds (1+ (car string-bounds)) (region-beginning)))
           (end (if string-bounds (1- (cdr string-bounds)) (region-end)))
           (sql-text (buffer-substring-no-properties start end))
           (formatted-sql (jr/format-sql-string sql-text)))
      (when formatted-sql
        (delete-region start end)
        (goto-char start)
        (insert formatted-sql)))))

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
  (defun jr/go-format-sql-string ()
    "Format SQL string in Go code, handling Go's backtick strings."
    (interactive)
    (save-excursion
      (let* ((start (save-excursion
                      (re-search-backward "`" nil t)
                      (1+ (point))))
             (end (save-excursion
                    (re-search-forward "`" nil t)
                    (1- (point))))
             (sql-text (when (and start end)
                         (buffer-substring-no-properties start end)))
             (formatted-sql (when sql-text (jr/format-sql-string sql-text))))
        (when formatted-sql
          (delete-region start end)
          (goto-char start)
          (insert formatted-sql)))))

  ;; Add Go-specific keybinding
  (map! :map go-mode-map
        :localleader
        (:prefix ("s" . "SQL")
         :desc "Format SQL string" "f" #'jr/go-format-sql-string)))

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
              (setq show-trailing-whitespace t)
              ;; Enable format on save for SQL files
              (when (fboundp 'sqlformat-on-save-mode)
                (sqlformat-on-save-mode))))

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
        '("sql-formatter" "-l" "postgresql"))

  ;; Associate SQL mode with formatter
  (add-to-list 'apheleia-mode-alist
               (cons 'sql-mode
                     (cond
                      ((executable-find "pg_format") 'pgformatter)
                      ((executable-find "sqlformat") 'sqlformat)
                      ((executable-find "sql-formatter") 'sql-formatter)))))

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
