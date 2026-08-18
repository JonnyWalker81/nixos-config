;;; test-config-go.el --- Tests for Go config helpers -*- lexical-binding: t; -*-

(require 'ert)

(defmacro use-package! (&rest _args))
(defmacro after! (&rest body)
  `(progn ,@(cdr body)))
(defmacro map! (&rest _args))
(defmacro set-formatter! (&rest _args))

(defun lsp-register-custom-settings (&rest _args))

(unless (boundp 'doom-cache-dir)
  (defvar doom-cache-dir temporary-file-directory))

(defvar lsp-file-watch-ignored-directories nil)
(defvar lsp-gopls-server-args nil)
(defvar lsp-gopls-staticcheck nil)
(defvar lsp-gopls-complete-unimported nil)
(defvar lsp-gopls-use-placeholders nil)

(unless (fboundp 'go-mode)
  (define-derived-mode go-mode prog-mode "Go"))

(defun flycheck-mode (&rest _args))
(defun lsp-deferred (&rest _args))
(defun gofmt-before-save (&rest _args))

(load-file "/home/cipher/nixos-config/users/doom.d/config-go.el")

(defun jr/test-go--current-test-name (contents marker)
  "Return detected test name from CONTENTS at MARKER."
  (with-temp-buffer
    (insert contents)
    (goto-char (point-min))
    (search-forward marker)
    (jr/go--current-test-name)))

(defun jr/test-go--with-temp-project (files fn)
  "Create a temp Go project with FILES, then call FN with its root.
FILES is an alist of relative path to contents."
  (let ((root (make-temp-file "jr-go-project" t)))
    (unwind-protect
        (progn
          (dolist (file files)
            (let ((path (expand-file-name (car file) root)))
              (make-directory (file-name-directory path) t)
              (with-temp-file path
                (insert (cdr file)))))
          (funcall fn root))
      (delete-directory root t))))

(ert-deftest jr/go--current-test-name-detects-plain-test ()
  (should (equal
           (jr/test-go--current-test-name
            "func TestProjectResolver_projectThumbnails(t *testing.T) {\n  result := 1\n  _ = result\n}\n"
            "result := 1")
           "TestProjectResolver_projectThumbnails")))

(ert-deftest jr/go--current-test-name-detects-test-with-subtest-body ()
  (should (equal
           (jr/test-go--current-test-name
            "func TestProjectResolver_projectThumbnails(t *testing.T) {\n  t.Run(\"sub\", func(t *testing.T) {\n    value := 1\n    _ = value\n  })\n}\n"
            "value := 1")
           "TestProjectResolver_projectThumbnails")))

(ert-deftest jr/go--current-test-name-ignores-non-test-functions ()
  (should-not
   (jr/test-go--current-test-name
    "func helper() {\n  value := 1\n  _ = value\n}\n"
    "value := 1")))

(ert-deftest jr/go-coverage--apply-to-buffer-creates-overlays ()
  (let ((file "/tmp/project.go")
        (coverage-table (make-hash-table :test 'equal)))
    (puthash file
             (let ((line-table (make-hash-table :test 'eql)))
               (puthash 2 1 line-table)
               (puthash 3 0 line-table)
               line-table)
             coverage-table)
    (with-temp-buffer
      (insert "package main\nline two\nline three\n")
      (setq buffer-file-name file)
      (go-mode)
      (jr/go-coverage--apply-to-buffer (current-buffer) coverage-table)
      (should (= 2 (length jr/go-coverage-overlays)))
      (should (overlay-get (car jr/go-coverage-overlays) 'line-prefix)))))

(ert-deftest jr/go-coverage--normalize-file-handles-module-import-paths ()
  (jr/test-go--with-temp-project
   '(("go.mod" . "module github.com/example/project\n\ngo 1.24\n")
     ("pkg/project.go" . "package pkg\n"))
   (lambda (root)
     (let ((default-directory temporary-file-directory))
       (should
        (equal (jr/go-coverage--normalize-file
                "github.com/example/project/pkg/project.go"
                root)
               (file-truename (expand-file-name "pkg/project.go" root))))))))

(ert-deftest jr/go-coverage--collect-and-apply-module-profile-to-buffer ()
  (jr/test-go--with-temp-project
   '(("go.mod" . "module github.com/example/project\n\ngo 1.24\n")
     ("pkg/project.go" . "package pkg\n\nfunc Run() int {\n  return 1\n}\n"))
   (lambda (root)
     (let* ((default-directory temporary-file-directory)
            (profile (expand-file-name "coverage.out" root))
            (file (expand-file-name "pkg/project.go" root)))
       (with-temp-file profile
         (insert "mode: count\n")
         (insert "github.com/example/project/pkg/project.go:3.1,3.12 1 1\n")
         (insert "github.com/example/project/pkg/project.go:4.1,4.13 1 0\n"))
       (with-current-buffer (find-file-noselect file)
         (unwind-protect
             (progn
               (jr/go-coverage--apply-to-buffer
                (current-buffer)
                (jr/go-coverage--collect-line-counts profile (expand-file-name "pkg/" root)))
               (should (= 2 (length jr/go-coverage-overlays)))
               (should (seq-some (lambda (ov) (overlay-get ov 'line-prefix)) jr/go-coverage-overlays)))
           (kill-buffer (current-buffer))))))))

;;; test-config-go.el ends here
