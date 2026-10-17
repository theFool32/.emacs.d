;;; daemon-packages-tests.el --- Eager daemon loading -*- lexical-binding: t; -*-

;; emacs --batch -Q -l tests/daemon-packages-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'use-package)
(setq native-comp-enable-subr-trampolines nil)

(defvar +my/first-input-hook nil)
(defvar daemon-packages-test-events nil)
(defvar daemon-packages-test-hook nil)
(defvar elpaca-after-init-hook nil)
(defvar daemon-packages-test-input-registration nil)
(defvar daemon-packages-test-startup-registrations nil)

;; Read only the loading policy, without starting Elpaca or user packages.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "../init.el" (file-name-directory load-file-name)))
  (emacs-lisp-mode)
  (check-parens)
  (goto-char (point-min))
  (condition-case nil
      (while t
        (let ((form (read (current-buffer))))
          (cond
           ((or (and (eq (car-safe form) 'defun)
                     (memq (cadr form) '(+my/use-package-eager-in-daemon
                                        +my/first-input-hook-fun
                                        use-package-handler/:after-call)))
                (and (eq (car-safe form) 'advice-add)
                     (equal (cadr form) ''use-package-normalize-keywords))
                (and (eq (car-safe form) 'add-to-list)
                     (equal (cadr form) ''use-package-deferring-keywords))
                (and (eq (car-safe form) 'setq)
                     (eq (cadr form) 'use-package-keywords))
                (and (eq (car-safe form) 'defalias)
                     (equal (cadr form) ''use-package-normalize/:after-call)))
            (eval form t))
           ((equal form '(unless (daemonp)
                           (add-hook 'pre-command-hook '+my/first-input-hook-fun)))
            (setq daemon-packages-test-input-registration form))
           ((or (equal form '(when (daemonp)
                               (add-hook 'elpaca-after-init-hook
                                         #'+my/first-input-hook-fun t)))
                (equal form '(add-hook 'elpaca-after-init-hook
                                      #'+my/desktop-startup-finished t)))
            (push form daemon-packages-test-startup-registrations)))))
    (end-of-file nil)))
(setq daemon-packages-test-startup-registrations
      (nreverse daemon-packages-test-startup-registrations))

(defmacro daemon-packages-test-session (daemon &rest body)
  (declare (indent 1))
  `(let* ((directory (make-temp-file "daemon-packages-" t))
          (load-path (cons directory load-path))
          (after-load-alist (copy-tree after-load-alist))
          (use-package-always-defer t)
          (use-package-always-demand nil)
          (use-package-expand-minimally t)
          (daemon-packages-test-events nil)
          (daemon-packages-test-hook nil))
     (unwind-protect
         (cl-letf (((symbol-function 'daemonp) (lambda () ,daemon))
                   ((symbol-function 'daemon-packages-test-trigger) #'ignore))
           (with-temp-file (expand-file-name "daemon-packages-test-fixture.el" directory)
             (insert ";;; -*- lexical-binding: t; -*-\n"
                     "(defun daemon-packages-test-command () (interactive))\n"
                     "(push 'loaded daemon-packages-test-events)\n"
                     "(provide 'daemon-packages-test-fixture)\n"))
           ,@body)
       (dolist (feature '(daemon-packages-test-fixture daemon-packages-test-dependency))
         (when (featurep feature) (unload-feature feature t)))
       (fmakunbound 'daemon-packages-test-command)
       (delete-directory directory t))))

(ert-deftest daemon-packages-loads-before-commands-or-mode-hooks ()
  (daemon-packages-test-session t
    (eval '(use-package daemon-packages-test-fixture
             :ensure nil
             :commands daemon-packages-test-command
             :hook (daemon-packages-test . daemon-packages-test-command)
             :config (push 'configured daemon-packages-test-events)) t)
    (should (equal daemon-packages-test-events '(configured loaded)))
    (should (memq 'daemon-packages-test-command daemon-packages-test-hook))))

(ert-deftest daemon-packages-ignores-explicit-deferral-and-after-call ()
  (dolist (options '((:defer t) (:defer 60) (:demand nil :defer t)
                     (:after-call daemon-packages-test-trigger)
                     (:after-call daemon-packages-test-hook)))
    (daemon-packages-test-session t
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (&rest _) (ert-fail "Daemon scheduled an idle load"))))
        (eval `(use-package daemon-packages-test-fixture :ensure nil
                 ,@options :config (push 'configured daemon-packages-test-events)) t))
      (should (equal daemon-packages-test-events '(configured loaded)))
      (should-not daemon-packages-test-hook)
      (let (advice)
        (advice-mapc (lambda (&rest _) (setq advice t)) 'daemon-packages-test-trigger)
        (should-not advice)))))

(ert-deftest daemon-packages-preserves-dependency-order ()
  (daemon-packages-test-session t
    (eval '(use-package daemon-packages-test-fixture :ensure nil
             :after daemon-packages-test-dependency :defer t) t)
    (should-not daemon-packages-test-events)
    (with-temp-file (expand-file-name "daemon-packages-test-dependency.el" directory)
      (insert ";;; -*- lexical-binding: t; -*-\n"
              "(push 'dependency daemon-packages-test-events)\n"
              "(provide 'daemon-packages-test-dependency)\n"))
    (require 'daemon-packages-test-dependency)
    (should (equal daemon-packages-test-events '(loaded dependency)))))

(ert-deftest daemon-packages-respects-disabled-packages-and-conditions ()
  (daemon-packages-test-session t
    (eval '(use-package daemon-packages-test-fixture :ensure nil :if nil) t)
    (eval '(use-package daemon-packages-test-fixture :ensure nil :disabled) t)
    (should-not daemon-packages-test-events)))

(ert-deftest daemon-packages-gui-retains-loading-on-first-use ()
  (daemon-packages-test-session nil
    (eval '(use-package daemon-packages-test-fixture :ensure nil
             :after-call daemon-packages-test-trigger
             :config (push 'configured daemon-packages-test-events)) t)
    (should-not daemon-packages-test-events)
    (daemon-packages-test-trigger)
    (should (equal daemon-packages-test-events '(configured loaded)))))

(ert-deftest daemon-packages-gui-retains-idle-timers ()
  (daemon-packages-test-session nil
    (let (delay)
      (cl-letf (((symbol-function 'run-with-idle-timer)
                 (lambda (seconds &rest _) (setq delay seconds))))
        (eval '(use-package daemon-packages-test-fixture :ensure nil :defer 60) t))
      (should (= delay 60))
      (should-not daemon-packages-test-events))))

(ert-deftest daemon-packages-enables-first-input-modes-before-startup-is-ready ()
  (should daemon-packages-test-input-registration)
  (should (= (length daemon-packages-test-startup-registrations) 2))
  (dolist (daemon '(t nil))
    (let ((pre-command-hook nil)
          (elpaca-after-init-hook nil)
          (+my/first-input-hook '(daemon-packages-test-enable-mode))
          events)
      (cl-letf (((symbol-function 'daemonp) (lambda () daemon))
                ((symbol-function 'daemon-packages-test-enable-mode)
                 (lambda () (push 'mode events)))
                ((symbol-function '+my/desktop-startup-finished)
                 (lambda () (push 'ready events))))
        (eval daemon-packages-test-input-registration t)
        (mapc (lambda (form) (eval form t)) daemon-packages-test-startup-registrations)
        (run-hooks 'elpaca-after-init-hook)
        (if daemon
            (progn
              (should (equal events '(ready mode)))
              (should-not pre-command-hook)
              (should-not +my/first-input-hook))
          (should (equal events '(ready)))
          (run-hooks 'pre-command-hook)
          (should (equal events '(mode ready))))))))

;;; daemon-packages-tests.el ends here
