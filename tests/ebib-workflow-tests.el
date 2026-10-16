;;; ebib-workflow-tests.el --- Metadata-only Ebib configuration -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar +self/ebib-base-dir)
(defconst ebib-workflow-test-init-file (expand-file-name "../init.el" (file-name-directory load-file-name)))
(with-temp-buffer
  (insert-file-contents (expand-file-name "../init.el" (file-name-directory load-file-name)))
  (emacs-lisp-mode) (check-parens) (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'use-package) (eq (cadr form) 'ebib))
            (dolist (item (cddr form))
              (when (and (eq (car-safe item) 'defun) (eq (cadr item) '+my/search-ebib-notes)) (eval item t))))))
    (end-of-file nil)))

(ert-deftest ebib-workflow-no-emacs-url-import-command-or-bindings ()
  (with-temp-buffer
    (insert-file-contents ebib-workflow-test-init-file)
    (goto-char (point-min))
    (should-not (search-forward "ebib-import-ref" nil t))))

(ert-deftest ebib-workflow-search-uses-existing-notes-directory ()
  (let ((+self/ebib-base-dir "/private/tmp/library with spaces/") received)
    (cl-letf (((symbol-function 'consult-ripgrep) (lambda (directory _query) (setq received directory))))
      (+my/search-ebib-notes)
      (should (equal received "/private/tmp/library with spaces/notes/")))))
