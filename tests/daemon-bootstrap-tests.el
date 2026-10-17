;;; daemon-bootstrap-tests.el --- Daemon bootstrap dependencies -*- lexical-binding: t; -*-

;; emacs --batch -Q -l tests/daemon-bootstrap-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'use-package)
(setq native-comp-enable-subr-trampolines nil)

(defconst daemon-bootstrap-test-root
  (expand-file-name ".." (file-name-directory load-file-name)))
(dolist (package '("evil" "goto-chg" "general" "dash" "elpaca-use-package"))
  (add-to-list 'load-path
               (expand-file-name (concat "elpaca/builds/" package)
                                 daemon-bootstrap-test-root)))
(require 'elpaca-use-package)
(elpaca-use-package-mode 1)

;; Use real packages while reproducing Elpaca's delayed configuration bodies.
;; No installation, server, user buffers, or other init sections are started.
(defvar daemon-bootstrap-test-queue nil)
(defmacro elpaca (_order &rest body)
  `(setq daemon-bootstrap-test-queue
         (append daemon-bootstrap-test-queue (list ',body))))
(defun elpaca-wait ()
  (while daemon-bootstrap-test-queue
    (eval (cons 'progn (pop daemon-bootstrap-test-queue)) t)))

(defvar daemon-bootstrap-test-forms nil)
(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" daemon-bootstrap-test-root))
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (cond
           ((or (and (eq (car-safe form) 'defun)
                     (eq (cadr form) '+my/use-package-eager-in-daemon))
                (and (eq (car-safe form) 'advice-add)
                     (equal (cadr form) ''use-package-normalize-keywords)))
            (eval form t))
           ((or (and (eq (car-safe form) 'use-package)
                     (memq (cadr form) '(evil general dash smerge-mode eglot)))
                (equal form '(when (daemonp) (elpaca-wait))))
            (push form daemon-bootstrap-test-forms)))))
    (end-of-file nil)))
(setq daemon-bootstrap-test-forms (nreverse daemon-bootstrap-test-forms))

(ert-deftest daemon-bootstrap-definers-and-dash-are-ready-before-built-in-configs ()
  (cl-letf (((symbol-function 'daemonp) (lambda () t)))
    (catch 'checked
      (dolist (form daemon-bootstrap-test-forms)
        (unless (eq (cadr form) 'eglot)
          (eval form t)
          (when (eq (cadr form) 'smerge-mode)
            (should (featurep 'general))
            (should (featurep 'dash))
            (should (fboundp 'leader-def))
            (should (fboundp 'local-leader-def))
            (should (featurep 'smerge-mode))
            (with-temp-buffer
              (smerge-mode 1)
              (evil-local-mode 1)
              (evil-normal-state)
              (should (eq (key-binding (kbd ", n")) #'smerge-next)))
            (throw 'checked t))))
      (ert-fail "No eager Smerge declaration was checked"))))

(ert-deftest daemon-bootstrap-eglot-loads-without-its-optional-booster ()
  (should-not (fboundp 'eglot-booster-mode))
  (cl-letf (((symbol-function 'daemonp) (lambda () t)))
    (eval (cl-find 'eglot daemon-bootstrap-test-forms :key #'cadr) t))
  (should (featurep 'eglot))
  (should (fboundp 'lsp-capf))
  (should-not (featurep 'eglot-booster)))

;;; daemon-bootstrap-tests.el ends here
