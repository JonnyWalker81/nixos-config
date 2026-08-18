;;; config-go.el --- Go/Golang configuration -*- lexical-binding: t; -*-

(require 'subr-x)

;; ============================================================================
;; GO/GOLANG CONFIGURATION WITH GOPLS
;; ============================================================================
;; Configuration matching VSCode Go extension settings

(use-package! go-mode
  :config
  ;; Use goimports as the format tool (matches "go.formatTool": "goimports")
  (setq gofmt-command "goimports")

  ;; Configure golangci-lint as the linter (matches "go.lintTool": "golangci-lint")
  (setq flycheck-go-golint-executable "golangci-lint")

  ;; IMPORTANT: Go uses tabs, not spaces for indentation
  ;; Set proper tab width for Go (standard is 4 or 8)
  (setq go-tab-width 4)

  ;; Hook for Go mode setup
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              (setq indent-tabs-mode nil)
              ;; Ensure Go uses tabs for indentation
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save (matches "[go]": {"editor.formatOnSave": true})
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports handled by jr/lsp-organize-imports-safe
              ))

  ;; Also configure go-ts-mode (tree-sitter mode)
  (add-hook 'go-ts-mode-hook
            (lambda ()
              ;; Enable LSP
              (lsp-deferred)
              ;; Ensure Go uses tabs for indentation
              (setq indent-tabs-mode nil)
              (setq tab-width 4)
              ;; Disable sql-indent-mode if it gets activated
              (when (bound-and-true-p sql-indent-mode)
                (sql-indent-mode -1))
              ;; Format on save
              (add-hook 'before-save-hook #'gofmt-before-save nil t)
              ;; Organize imports handled by jr/lsp-organize-imports-safe
              )))

;; ----------------------------------------------------------------------------
;; LSP Go Configuration (gopls)
;; ----------------------------------------------------------------------------

(after! lsp-go
  ;; Tell gopls to use goimports
  (setq lsp-go-format-tool "goimports")

  ;; Configure gopls settings to match VSCode config
  (setq lsp-go-gopls-server-args
        '("-remote=auto"))

  ;; Set gopls build directory filters (matches gopls "build.directoryFilters")
  (setq lsp-go-directory-filters
        ["-**/node_modules"
         "-**/testdata"])

  ;; Set local module for import organization (matches gopls "formatting.local")
  (setq lsp-go-imports-local-prefix "github.com/JoinCAD/komodo")

  ;; Enable all analyses
  (setq lsp-go-analyses
        '((fieldalignment . t)
          (nilness . t)
          (shadow . t)
          (unusedparams . t)
          (unusedwrite . t)
          (useany . t)
          (unusedvariable . t))))

;; ----------------------------------------------------------------------------
;; Flycheck/Golangci-lint Integration
;; ----------------------------------------------------------------------------

(after! flycheck
  (add-hook 'go-mode-hook
            (lambda ()
              ;; Set golangci-lint config file path (matches "go.lintFlags")
              (setq-local flycheck-golangci-lint-config "./.golangci-github.toml")
              ;; Enable flycheck mode for linting
              (flycheck-mode 1))))

;; ----------------------------------------------------------------------------
;; Formatter Setup
;; ----------------------------------------------------------------------------

;; Set formatter for tree-sitter Go mode
(set-formatter! 'gofmt '("goimports") :modes '(go-mode go-ts-mode))

;; ----------------------------------------------------------------------------
;; Additional gopls Configuration via LSP
;; ----------------------------------------------------------------------------

(after! lsp-mode
  ;; Add Go-specific LSP settings
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]node_modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]testdata\\'")

  ;; Configure gopls through LSP initialization options
  (setq lsp-gopls-server-args '("-remote=auto"))
  (setq lsp-gopls-staticcheck t)
  (setq lsp-gopls-complete-unimported t)
  (setq lsp-gopls-use-placeholders t)

  ;; Custom gopls settings matching VSCode configuration
  (lsp-register-custom-settings
   '(("gopls.formatting.local" "github.com/JoinCAD/komodo")
     ("gopls.build.directoryFilters" ["-**/node_modules" "-**/testdata"])
     ("gopls.analyses.fieldalignment" t)
     ("gopls.analyses.unusedparams" t)
     ("gopls.analyses.unusedwrite" t)
     ("gopls.analyses.useany" t))))

;; ----------------------------------------------------------------------------
;; Go Tag Configuration
;; ----------------------------------------------------------------------------

(setq go-tag-args (list "-transform" "camelcase"))

;; ----------------------------------------------------------------------------
;; Go Testing and Coverage
;; ----------------------------------------------------------------------------

(defvar-local jr/go-coverage-overlays nil
  "Coverage overlays active in the current buffer.")

(defface jr/go-coverage-covered-face
  '((t :inherit diff-added :background "#173620" :extend t))
  "Face used for covered Go lines.")

(defface jr/go-coverage-uncovered-face
  '((t :inherit diff-removed :background "#3a1f24" :extend t))
  "Face used for uncovered Go lines.")

(defface jr/go-coverage-covered-indicator-face
  '((t :inherit jr/go-coverage-covered-face :weight bold))
  "Face for the covered line indicator.")

(defface jr/go-coverage-uncovered-indicator-face
  '((t :inherit jr/go-coverage-uncovered-face :weight bold))
  "Face for the uncovered line indicator.")

(defun jr/go--current-package-directory ()
  "Return the current Go package directory."
  (or (and buffer-file-name (file-name-directory buffer-file-name))
      default-directory))

(defun jr/go--project-root (&optional directory)
  "Return the current Go project root for DIRECTORY."
  (let ((default-directory (or directory default-directory)))
    (or (when (fboundp 'projectile-project-root)
          (ignore-errors (projectile-project-root)))
        (locate-dominating-file default-directory "go.mod")
        default-directory)))

(defun jr/go--module-path (&optional directory)
  "Return the Go module path for DIRECTORY, or nil if unavailable."
  (let ((default-directory (or directory default-directory)))
    (string-trim
     (shell-command-to-string "go list -m -f '{{.Path}}' 2>/dev/null"))))

(defun jr/go--run-compilation (command directory buffer-name)
  "Run COMMAND from DIRECTORY using compilation in BUFFER-NAME."
  (let ((default-directory directory))
    (compilation-start command 'compilation-mode (lambda (_) buffer-name))))

(defun jr/go-test-current-package ()
  "Run go test for the current package."
  (interactive)
  (jr/go--run-compilation "go test ." (jr/go--current-package-directory) "*go test package*"))

(defun jr/go-test-current-project ()
  "Run go test for the current project."
  (interactive)
  (jr/go--run-compilation "go test ./..." (jr/go--project-root) "*go test project*"))

(defun jr/go-coverage-clear-buffer (&optional buffer)
  "Clear Go coverage overlays from BUFFER or the current buffer."
  (with-current-buffer (or buffer (current-buffer))
    (mapc #'delete-overlay jr/go-coverage-overlays)
    (setq jr/go-coverage-overlays nil)))

(defun jr/go-coverage-clear-project ()
  "Clear Go coverage overlays from all open Go buffers."
  (interactive)
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'go-mode 'go-ts-mode)
        (jr/go-coverage-clear-buffer buffer))))
  (message "Cleared Go coverage overlays"))

(defun jr/go-coverage--profile-path ()
  "Return the temporary coverage profile path for the current package."
  (expand-file-name (format "go-coverage-%s.out" (md5 (jr/go--current-package-directory)))
                    doom-cache-dir))

(defun jr/go-coverage--normalize-file (file base-directory)
  "Return FILE normalized against BASE-DIRECTORY."
  (let* ((project-root (jr/go--project-root base-directory))
         (module-path (jr/go--module-path project-root))
         (direct-path (expand-file-name file base-directory))
         (project-path (expand-file-name file project-root))
         (module-relative (and module-path
                               (string-prefix-p (concat module-path "/") file)
                               (string-remove-prefix (concat module-path "/") file)))
         (module-path-file (and module-relative
                                (expand-file-name module-relative project-root))))
    (file-truename
     (cond
      ((file-name-absolute-p file) file)
      ((file-exists-p direct-path) direct-path)
      ((file-exists-p project-path) project-path)
      ((and module-path-file (file-exists-p module-path-file)) module-path-file)
      (t project-path)))))

(defun jr/go-coverage--collect-line-counts (profile-file base-directory)
  "Collect coverage counts from PROFILE-FILE relative to BASE-DIRECTORY."
  (let ((coverage-table (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents profile-file)
      (goto-char (point-min))
      (forward-line 1)
      (while (not (eobp))
        (when (looking-at
               "^\\(.+\\):\\([0-9]+\\)\\.[0-9]+,\\([0-9]+\\)\\.[0-9]+ [0-9]+ \\([0-9]+\\)$")
          (let* ((file (jr/go-coverage--normalize-file (match-string 1) base-directory))
                 (start-line (string-to-number (match-string 2)))
                 (end-line (string-to-number (match-string 3)))
                 (count (string-to-number (match-string 4)))
                 (line-table (or (gethash file coverage-table)
                                 (puthash file (make-hash-table :test 'eql) coverage-table))))
            (dotimes (offset (1+ (- end-line start-line)))
              (let* ((line (+ start-line offset))
                     (existing (gethash line line-table -1)))
                (puthash line (max existing count) line-table)))))
        (forward-line 1)))
    coverage-table))

(defun jr/go-coverage--make-overlay (line face)
  "Create a coverage overlay for LINE using FACE."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (let* ((start (line-beginning-position))
           (end (min (point-max) (1+ (line-end-position))))
           (overlay (make-overlay start end))
           (indicator-face (if (eq face 'jr/go-coverage-covered-face)
                               'jr/go-coverage-covered-indicator-face
                             'jr/go-coverage-uncovered-indicator-face))
           (indicator (propertize "  " 'face indicator-face 'display '(space :width 1))))
      (overlay-put overlay 'face face)
      (overlay-put overlay 'font-lock-face face)
      (overlay-put overlay 'line-prefix indicator)
      (overlay-put overlay 'wrap-prefix indicator)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'priority 100)
      overlay)))

(defun jr/go-coverage--apply-to-buffer (buffer coverage-table)
  "Apply COVERAGE-TABLE overlays to BUFFER."
  (with-current-buffer buffer
    (when buffer-file-name
      (jr/go-coverage-clear-buffer buffer)
      (let ((line-table (gethash (file-truename buffer-file-name) coverage-table)))
        (when line-table
          (maphash
           (lambda (line count)
             (push (jr/go-coverage--make-overlay
                    line
                    (if (> count 0)
                        'jr/go-coverage-covered-face
                      'jr/go-coverage-uncovered-face))
                   jr/go-coverage-overlays))
           line-table))))))

(defun jr/go-coverage--apply-to-open-buffers (coverage-table package-directory)
  "Apply COVERAGE-TABLE to open Go buffers within PACKAGE-DIRECTORY."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (and buffer-file-name
                 (derived-mode-p 'go-mode 'go-ts-mode)
                 (string-prefix-p (file-truename package-directory)
                                  (file-truename (file-name-directory buffer-file-name))))
        (jr/go-coverage--apply-to-buffer buffer coverage-table)))))

(defun jr/go--current-test-name ()
  "Return the Go test name at point."
  (or
   (when-let ((fn (and (fboundp 'which-function)
                       (which-function))))
     (when (string-match "\\(Test[[:alnum:]_]+\\)" fn)
       (match-string 1 fn)))
   (save-excursion
     (ignore-errors
       (beginning-of-defun)
       (when (looking-at
              "func[[:space:]]+\\([[:alnum:]_]+\\)[[:space:]]*(")
          (let ((candidate (match-string-no-properties 1)))
            (when (string-prefix-p "Test" candidate)
              candidate)))))
   (save-excursion
     (end-of-line)
     (let (test-name)
       (while (and (not test-name)
                   (re-search-backward
                    "^func[[:space:]]+\\([[:alnum:]_]+\\)[[:space:]]*("
                    nil
                    t))
         (let ((candidate (match-string-no-properties 1)))
           (when (string-prefix-p "Test" candidate)
             (setq test-name candidate))))
       test-name))))

(defun jr/go-coverage--run (command package-directory success-message)
  "Run coverage COMMAND in PACKAGE-DIRECTORY and show SUCCESS-MESSAGE."
  (let* ((profile-file (jr/go-coverage--profile-path))
         (output-buffer (get-buffer-create "*go coverage*"))
         (origin-buffer (current-buffer))
         (full-command (format "%s -covermode=count -coverprofile=%s && go tool cover -func=%s"
                               command
                               (shell-quote-argument profile-file)
                               (shell-quote-argument profile-file))))
    (with-current-buffer output-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (format "Running coverage in %s\n\n$ %s\n\n"
                        package-directory
                        full-command))
        (compilation-mode)))
    (let ((default-directory package-directory))
      (make-process
       :name "go-coverage"
       :buffer output-buffer
       :command (list shell-file-name shell-command-switch full-command)
       :noquery t
       :sentinel
       (lambda (process _event)
         (when (memq (process-status process) '(exit signal))
           (if (= (process-exit-status process) 0)
               (let ((coverage-table (jr/go-coverage--collect-line-counts profile-file package-directory)))
                 (jr/go-coverage--apply-to-open-buffers coverage-table package-directory)
                 (when (buffer-live-p origin-buffer)
                   (with-current-buffer origin-buffer
                     (message "%s" success-message)))
                 (display-buffer output-buffer))
             (display-buffer output-buffer)
             (message "Go coverage failed; see *go coverage*"))))))
    (message "Running Go coverage...")))

(defun jr/go-coverage-current-package ()
  "Run coverage for the current Go package and paint it in open buffers."
  (interactive)
  (let ((package-directory (jr/go--current-package-directory)))
    (jr/go-coverage--run
     "go test ."
     package-directory
     (format "Go coverage updated for package %s"
             (file-name-nondirectory (directory-file-name package-directory))))))

(defun jr/go-coverage-current-test ()
  "Run coverage for the current Go test and paint it in open buffers."
  (interactive)
  (let ((test-name (jr/go--current-test-name)))
    (unless test-name
      (user-error "Point is not inside a Go test function"))
    (let ((package-directory (jr/go--current-package-directory)))
      (jr/go-coverage--run
       (format "go test . -run %s"
               (shell-quote-argument (format "^%s$" test-name)))
       package-directory
       (format "Go coverage updated for test %s" test-name)))))

(use-package! gotest
  :after go-mode
  :commands (go-test-current-test go-test-current-file)
  :config
  (setq go-test-verbose t))

(map! :after go-mode
      :map go-mode-map
      :localleader
      (:prefix ("t" . "test")
       :desc "Run current test" "t" #'go-test-current-test
       :desc "Run current file tests" "f" #'go-test-current-file
       :desc "Run current test coverage" "C" #'jr/go-coverage-current-test
       :desc "Run current package tests" "p" #'jr/go-test-current-package
       :desc "Run project tests" "P" #'jr/go-test-current-project
       :desc "Run package coverage" "c" #'jr/go-coverage-current-package
       :desc "Clear coverage" "x" #'jr/go-coverage-clear-project))

(map! :after go-mode
      :map go-ts-mode-map
      :localleader
      (:prefix ("t" . "test")
       :desc "Run current test" "t" #'go-test-current-test
       :desc "Run current file tests" "f" #'go-test-current-file
       :desc "Run current test coverage" "C" #'jr/go-coverage-current-test
       :desc "Run current package tests" "p" #'jr/go-test-current-package
       :desc "Run project tests" "P" #'jr/go-test-current-project
       :desc "Run package coverage" "c" #'jr/go-coverage-current-package
       :desc "Clear coverage" "x" #'jr/go-coverage-clear-project))

(provide 'config-go)
;;; config-go.el ends here
