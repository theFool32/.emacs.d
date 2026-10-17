;;; daemon-lifecycle-tests.el --- Shutdown and startup regressions -*- lexical-binding: t; -*-

;; emacs --batch -Q -l tests/daemon-lifecycle-tests.el -f ert-run-tests-batch-and-exit
(defconst daemon-lifecycle-test-init-file
  (expand-file-name "../init.el" (file-name-directory load-file-name)))
(load (expand-file-name "desktop-startup-tests.el" (file-name-directory load-file-name)) nil t)
(defvar doom-modeline-height nil)

(ert-deftest daemon-lifecycle-rime-finalizes-once-before-native-teardown ()
  (let ((rime--lib-loaded t)
        (calls 0))
    (cl-letf (((symbol-function 'rime-lib-finalize)
               (lambda () (cl-incf calls))))
      (+my/desktop-finalize-rime)
      (+my/desktop-finalize-rime)
      (should (= calls 1))
      (should-not rime--lib-loaded))))

(ert-deftest daemon-lifecycle-rime-cleanup-does-not-load-an-unused-engine ()
  (let ((rime--lib-loaded nil))
    (cl-letf (((symbol-function 'rime-lib-finalize)
               (lambda () (ert-fail "Rime was not initialized")))
              ((symbol-function 'require)
               (lambda (&rest _) (ert-fail "Exiting loaded a new package"))))
      (+my/desktop-finalize-rime))))

(ert-deftest daemon-lifecycle-modeline-fits-graphical-fonts-after-preloading ()
  (dolist (settings '((t 5 24) (t 30 30) (nil 12 12)))
    (let ((doom-modeline-mode nil)
          (doom-modeline-height (nth 1 settings)))
      (cl-letf (((symbol-function 'daemonp) (lambda () (car settings)))
                ((symbol-function 'window-font-height) (lambda (&rest _) 20))
                ((symbol-function 'doom-modeline-mode)
                 (lambda (_) (setq doom-modeline-mode t)))
                ((symbol-function 'doom-modeline--reset-font-height-cache) #'ignore)
                ((symbol-function 'doom-modeline-refresh-bars) #'ignore)
                ((symbol-function 'doom-modeline-invalidate-huds) #'ignore)
                ((symbol-function 'force-mode-line-update) #'ignore))
        (+my/desktop-setup-modeline)
        (should doom-modeline-mode)
        (should (= doom-modeline-height (nth 2 settings)))))))

(ert-deftest daemon-lifecycle-warning-filter-keeps-initialization-errors ()
  (require 'warnings)
  (let ((warning-suppress-log-types nil)
        (warning-minimum-level :emergency)
        (name " *daemon-warning-filter-test*"))
    (with-temp-buffer
      (insert-file-contents daemon-lifecycle-test-init-file)
      (goto-char (point-min))
      (let (form found)
        (while (not found)
          (setq form (read (current-buffer)))
          (when (and (eq (car-safe form) 'add-to-list)
                     (equal (cadr form) ''warning-suppress-log-types))
            (eval form t)
            (setq found t)))))
    (unwind-protect
        (progn
          (display-warning '(files missing-lexbind-cookie) "Missing binding cookie" :warning name)
          (should-not (get-buffer name))
          (display-warning '(initialization) "Test initialization error" :error name)
          (with-current-buffer name
            (should (string-match-p "Test initialization error" (buffer-string)))))
      (when-let* ((buffer (get-buffer name))) (kill-buffer buffer)))))

;;; daemon-lifecycle-tests.el ends here
