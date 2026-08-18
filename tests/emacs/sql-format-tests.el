;;; sql-format-tests.el --- Tests for Go embedded SQL formatting -*- lexical-binding: t; -*-

;; Verifies the jr/go-format-sql-strings machinery from users/doom.d/config-sql.el:
;; SQL inside Go raw strings is formatted by sql-formatter and indented purely
;; with tabs, the on-save hook is wired, and the keybindings are declared.
;;
;; Run:
;;   emacs --batch -l tests/emacs/sql-format-tests.el -f ert-run-tests-batch-and-exit

(require 'ert)
(require 'cl-lib)
(require 'subr-x)
(require 'treesit nil t)

;; Make the user-level tree-sitter grammars visible in batch runs so the
;; go-ts-mode tests exercise the real parser path
(when (and (boundp 'treesit-extra-load-path)
           (file-directory-p (expand-file-name "~/.tree-sitter/bin")))
  (add-to-list 'treesit-extra-load-path (expand-file-name "~/.tree-sitter/bin")))

(defconst sql-format-test-repo-root
  (expand-file-name "../.." (file-name-directory (or load-file-name buffer-file-name))))

(defconst sql-format-test-komodo-file
  (expand-file-name "~/Repositories/komodo/services/x/repository/milestone_cost_analytics.go"))

(defconst sql-format-test-komodo-test-file
  (expand-file-name "~/Repositories/komodo/services/x/repository/milestone_cost_analytics_test.go"))

;; --- Doom macro stubs so config-sql.el loads in vanilla batch Emacs --------

(defvar sql-format-test-map-calls nil)

(defmacro use-package! (&rest _args) nil)
(defmacro after! (&rest _args) nil)
(defmacro map! (&rest args)
  `(push ',args sql-format-test-map-calls))

(load (expand-file-name "users/doom.d/config-sql.el" sql-format-test-repo-root) nil t)

;; --- Helpers ----------------------------------------------------------------

(defmacro sql-format-test-with-go-buffer (content &rest body)
  "Run BODY in a temp buffer holding CONTENT with Go-like string syntax."
  (declare (indent 1))
  `(with-temp-buffer
     (modify-syntax-entry ?\` "\"" (syntax-table))
     (insert ,content)
     (setq-local major-mode 'go-mode) ; satisfy derived-mode-p without go-mode
     (goto-char (point-min))
     ,@body))

(defun sql-format-test-buffer-string ()
  (buffer-substring-no-properties (point-min) (point-max)))

(defun sql-format-test-strip-whitespace (s)
  (replace-regexp-in-string "[ \t\n]+" "" s))

(defun sql-format-test-string-lines-tab-indented-p (content open-line close-line)
  "Non-nil if every indented line in (OPEN-LINE, CLOSE-LINE) starts with tabs only."
  (let ((lines (split-string content "\n")))
    (cl-every (lambda (l) (not (string-match-p "^ " l)))
              (cl-subseq lines open-line (min close-line (length lines))))))

(defconst sql-format-test-fixture
  "package main

func query() string {
	const q = `
        SELECT id,name FROM users WHERE active=true ORDER BY name`
	return q
}
")

(defconst sql-format-test-fixture-non-sql
  "package main

var tmpl = `
    Hello %s,
    your order shipped.`
")

;; --- Tests ------------------------------------------------------------------

(ert-deftest sql-format-test-formats-fixture-with-tabs ()
  "Space-indented SQL strings are formatted and re-indented with tabs."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer sql-format-test-fixture
    (jr/go-format-sql-strings)
    (let ((result (sql-format-test-buffer-string)))
      ;; Content is preserved (whitespace-only change)
      (should (string= (sql-format-test-strip-whitespace sql-format-test-fixture)
                       (sql-format-test-strip-whitespace result)))
      ;; The SQL got expanded to multiple lines, first line on the backtick line
      (should (string-match-p "`SELECT\n" result))
      ;; No line in the buffer is space-indented anymore
      (should-not (string-match-p "^ " result))
      ;; Nesting uses tabs one level below the base
      (should (string-match-p "\n\t\tid,\n" result)))))

(ert-deftest sql-format-test-idempotent ()
  "Formatting an already formatted buffer changes nothing."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer sql-format-test-fixture
    (jr/go-format-sql-strings)
    (let ((first-pass (sql-format-test-buffer-string)))
      (jr/go-format-sql-strings)
      (should (string= first-pass (sql-format-test-buffer-string))))))

(ert-deftest sql-format-test-postfix-backtick-line-preserved ()
  "A closing backtick on its own line stays on its own line."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer
      "package main

func q() string {
	const q = `
        SELECT id FROM users
	`
	return q
}
"
    (jr/go-format-sql-strings)
    (should (string-match-p "\n\t`" (sql-format-test-buffer-string)))))

(ert-deftest sql-format-test-non-sql-strings-untouched ()
  "Multi-line strings that are not SQL are left exactly as they were."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer sql-format-test-fixture-non-sql
    (jr/go-format-sql-strings)
    (should (string= sql-format-test-fixture-non-sql
                     (sql-format-test-buffer-string)))))

(ert-deftest sql-format-test-komodo-file-formats-cleanly ()
  "The real komodo analytics file: mangling the SQL indentation to spaces and
reformatting restores pure-tab indentation with identical SQL content."
  (skip-unless (and (executable-find "sql-formatter")
                    (file-readable-p sql-format-test-komodo-file)))
  (let ((original (with-temp-buffer
                    (insert-file-contents sql-format-test-komodo-file)
                    (buffer-string))))
    (sql-format-test-with-go-buffer original
      ;; Mangle: expand leading tabs to spaces on every line inside raw strings
      (dolist (region (jr/go--raw-string-regions))
        (save-excursion
          (goto-char (1+ (car region)))
          (forward-line 1)
          (while (< (point) (1- (cdr region)))
            (when (looking-at "\t+")
              (let ((n (- (match-end 0) (match-beginning 0))))
                (replace-match (make-string (* 2 n) ?\s))))
            (forward-line 1))))
      (should (string-match-p "^ " (sql-format-test-buffer-string))) ; mangled
      (jr/go-format-sql-strings)
      (let ((result (sql-format-test-buffer-string)))
        ;; Token-identical to the on-disk file and no space indentation left
        (should (string= (sql-format-test-strip-whitespace original)
                         (sql-format-test-strip-whitespace result)))
        (should-not (string-match-p "^ " result))))))

(ert-deftest sql-format-test-return-counts ()
  "Return value reports strings changed: 1 on first format, 0 when a no-op."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer sql-format-test-fixture
    (should (= 1 (jr/go-format-sql-strings)))
    (should (= 0 (jr/go-format-sql-strings)))))

(ert-deftest sql-format-test-glued-start-string-normalized ()
  "Strings starting right after the backtick (`INSERT INTO ...) are formatted
into the canonical shape: first SQL line stays on the backtick line, the
rest is pure tab indentation."
  (skip-unless (executable-find "sql-formatter"))
  (sql-format-test-with-go-buffer
      "package main

func seed() []string {
	return []string{
		`INSERT INTO
	users (id, name)
VALUES
	(
            'abc',
		'Test User'
	)`,
	}
}
"
    (should (= 1 (jr/go-format-sql-strings)))
    (let ((result (sql-format-test-buffer-string)))
      ;; First SQL line stays on the backtick line
      (should (string-match-p "`INSERT INTO\n" result))
      ;; No space indentation remains
      (should-not (string-match-p "^ " result))
      ;; Idempotent
      (should (= 0 (jr/go-format-sql-strings))))))

(ert-deftest sql-format-test-komodo-test-file-formats-cleanly ()
  "The real komodo test file (glued-start INSERT fixtures): formatting is a
no-op when already formatted, and restores pure tabs after mangling."
  (skip-unless (and (executable-find "sql-formatter")
                    (file-readable-p sql-format-test-komodo-test-file)))
  (let ((original (with-temp-buffer
                    (insert-file-contents sql-format-test-komodo-test-file)
                    (buffer-string))))
    (sql-format-test-with-go-buffer original
      ;; Mangle: expand leading tabs to spaces inside every raw string
      (dolist (region (jr/go--raw-string-regions))
        (save-excursion
          (goto-char (1+ (car region)))
          (forward-line 1)
          (while (< (point) (1- (cdr region)))
            (when (looking-at "\t+")
              (let ((n (- (match-end 0) (match-beginning 0))))
                (replace-match (make-string (* 2 n) ?\s))))
            (forward-line 1))))
      (jr/go-format-sql-strings)
      (let ((result (sql-format-test-buffer-string)))
        (should (string= (sql-format-test-strip-whitespace original)
                         (sql-format-test-strip-whitespace result)))
        (should-not (string-match-p "^ " result))
        ;; Already-formatted content reformats to itself
        (should (= 0 (jr/go-format-sql-strings)))))))

(ert-deftest sql-format-test-go-ts-mode-scan-and-format ()
  "go-ts-mode gives backticks punctuation syntax, so regions must come from
the tree-sitter parser; the result must match the classic go-mode path.
This is the mode interactive sessions actually use for .go files."
  (skip-unless (and (executable-find "sql-formatter")
                    (fboundp 'go-ts-mode)
                    (fboundp 'treesit-language-available-p)
                    (treesit-language-available-p 'go)))
  (let (classic-result ts-result)
    (sql-format-test-with-go-buffer sql-format-test-fixture
      (jr/go-format-sql-strings)
      (setq classic-result (sql-format-test-buffer-string)))
    (with-temp-buffer
      (insert sql-format-test-fixture)
      (delay-mode-hooks (go-ts-mode))
      (should (> (length (jr/go--raw-string-regions)) 0))
      (should (= 1 (jr/go-format-sql-strings)))
      (setq ts-result (sql-format-test-buffer-string)))
    (should (string= classic-result ts-result))))

(ert-deftest sql-format-test-on-save-hook-wired ()
  "go-mode buffers get the SQL formatting before-save hook."
  (should (memq #'jr/go-enable-sql-string-formatting go-mode-hook))
  (should (memq #'jr/go-enable-sql-string-formatting go-ts-mode-hook))
  (with-temp-buffer
    (jr/go-enable-sql-string-formatting)
    (should (memq #'jr/go-format-sql-strings before-save-hook))))

(ert-deftest sql-format-test-keybindings-declared ()
  "The localleader bindings target the new formatter in both Go modes."
  ;; go-ts-mode binding is registered at load time (recorded by the map! stub)
  (should (cl-some (lambda (call)
                     (memq 'jr/go-format-sql-strings (flatten-tree call)))
                   sql-format-test-map-calls))
  ;; go-mode binding lives inside `after! go-mode' (not evaluated in batch);
  ;; assert the declaration exists in the source
  (let ((src (with-temp-buffer
               (insert-file-contents
                (expand-file-name "users/doom.d/config-sql.el" sql-format-test-repo-root))
               (buffer-string))))
    (should (string-match-p
             "go-mode-map[^(]*\n.*:localleader\\(.\\|\n\\)\\{0,200\\}\"f\" #'jr/go-format-sql-strings"
             src))))

(provide 'sql-format-tests)
;;; sql-format-tests.el ends here
