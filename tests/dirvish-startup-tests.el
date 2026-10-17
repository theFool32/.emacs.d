;;; dirvish-startup-tests.el --- Dirvish loading regression -*- lexical-binding: t; -*-

;; Run with: emacs --batch -Q -l tests/dirvish-startup-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'use-package)

(defconst dirvish-startup-test-root
  (expand-file-name ".." (file-name-directory load-file-name)))
(dolist (package '("dirvish" "compat" "transient" "evil" "goto-chg"
                   "evil-collection"))
  (add-to-list 'load-path
               (expand-file-name (concat "elpaca/builds/" package)
                                 dirvish-startup-test-root)))

;; Load the actual declarations, without Elpaca or the rest of the init file.
;; Default to first input before Dired loads, which reproduces the regression.
;; DIRVISH_TEST_EARLY_DIRED=1 exercises the opposite startup order.
(defvar +my/first-input-hook nil)
(when (getenv "DIRVISH_TEST_EARLY_DIRED") (require 'dired))
(defconst dirvish-startup-test-dired-loaded (featurep 'dired))
(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" dirvish-startup-test-root))
  (emacs-lisp-mode)
  (check-parens)
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (when (or (and (eq (car-safe form) 'defun)
                         (eq (cadr form) '+my/first-input-hook-fun))
                    (and (eq (car-safe form) 'use-package)
                         (memq (cadr form) '(dired dirvish))))
            (when-let* ((position (cl-position :ensure form)))
              (setf (nth (1+ position) form) nil))
            (eval form t))))
    (end-of-file nil)))

(defconst dirvish-startup-test-was-deferred (not (featurep 'dirvish)))
(+my/first-input-hook-fun)
(require 'dired)

;; Load Evil after Dirvish too: the dedicated bindings must still be installed.
(setq evil-want-keybinding nil)
(require 'evil-collection)
(evil-collection-init '(dired))
(evil-mode 1)

(ert-deftest dirvish-startup-enables-regardless-of-first-input-order ()
  (should (eq dirvish-startup-test-was-deferred
              (not dirvish-startup-test-dired-loaded)))
  (should (featurep 'dirvish))
  (should (bound-and-true-p dirvish-override-dired-mode))
  (should (advice-member-p #'dirvish-dired-noselect-a #'dired-noselect))
  (should-not (memq 'dirvish-override-dired-mode +my/first-input-hook)))

(ert-deftest dirvish-startup-command-creates-a-managed-directory-buffer ()
  (let ((dir (make-temp-file "dirvish-startup-" t))
        (dirvish-default-layout nil)
        (dirvish-attributes nil)
        (dirvish-preview-dispatchers nil)
        (dirvish-mode-line-format nil)
        (dirvish-header-line-format nil)
        buffer)
    (unwind-protect
        (save-window-excursion
          (dirvish dir)
          (setq buffer (current-buffer))
          (should (derived-mode-p 'dired-mode))
          (should (dirvish-curr))
          (should (equal default-directory (file-name-as-directory dir)))
          (should (eq revert-buffer-function #'dirvish-revert)))
      (when (buffer-live-p buffer) (kill-buffer buffer))
      (delete-directory dir))))

(ert-deftest dirvish-startup-menus-preserve-dired-mark-prefixes ()
  ;; Dired-x adds bindings such as `* (' after the original Dired setup.
  (require 'dired-x)
  (with-temp-buffer
    (dired-mode default-directory)
    (evil-local-mode 1)
    (evil-normal-state)
    (should (keymapp (key-binding "*")))
    (should (eq (key-binding (kbd "* (")) #'dired-mark-sexp))
    (use-local-map dirvish-mode-map)
    (evil-normalize-keymaps)
    (should (eq (key-binding "*") #'dirvish-mark-menu))
    (should (eq (key-binding "q") #'dirvish-quit))
    (should (eq (key-binding (kbd "TAB")) #'dirvish-subtree-toggle))))

;;; dirvish-startup-tests.el ends here
