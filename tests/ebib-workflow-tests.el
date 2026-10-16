;;; ebib-workflow-tests.el --- Metadata-only Ebib configuration -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defvar +self/ebib-base-dir)
(with-temp-buffer
  (insert-file-contents (expand-file-name "../init.el" (file-name-directory load-file-name)))
  (emacs-lisp-mode) (check-parens) (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (and (eq (car-safe form) 'defun) (eq (cadr form) 'ebib-import-ref)) (eval form t))
          (when (and (eq (car-safe form) 'use-package) (eq (cadr form) 'ebib))
            (dolist (item (cddr form))
              (when (and (eq (car-safe item) 'defun) (eq (cadr item) '+my/search-ebib-notes)) (eval item t))))))
    (end-of-file nil)))

(ert-deftest ebib-workflow-url-import-delegates-without-a-pdf-downloader ()
  (let (received)
    (cl-letf (((symbol-function 'ebib-browser-collect-url) (lambda (url) (setq received url)))
              ((symbol-function 'make-process) (lambda (&rest _) (ert-fail "Old PDF process was launched")))
              ((symbol-function 'run-with-idle-timer) (lambda (&rest _) (ert-fail "Old PDF timer was scheduled"))))
      (ebib-import-ref "https://arxiv.org/abs/2610.20608")
      (should (equal received "https://arxiv.org/abs/2610.20608")))))

(ert-deftest ebib-workflow-missing-bridge-is-an-actionable-error ()
  (let ((old (and (fboundp 'ebib-browser-collect-url) (symbol-function 'ebib-browser-collect-url))))
    (unwind-protect
        (progn (fmakunbound 'ebib-browser-collect-url)
               (should (string-match-p "install.py" (error-message-string (should-error (ebib-import-ref "https://example.org") :type 'user-error)))))
      (when old (fset 'ebib-browser-collect-url old)))))

(ert-deftest ebib-workflow-search-uses-existing-notes-directory ()
  (let ((+self/ebib-base-dir "/private/tmp/library with spaces/") received)
    (cl-letf (((symbol-function 'consult-ripgrep) (lambda (directory _query) (setq received directory))))
      (+my/search-ebib-notes)
      (should (equal received "/private/tmp/library with spaces/notes/")))))
