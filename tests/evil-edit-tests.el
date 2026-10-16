;;; evil-edit-tests.el --- Enclosing delimiter edits -*- lexical-binding: t; -*-
(require 'ert)
(require 'cl-lib)
(defconst evil-edit-test-root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))
(dolist (package '("evil" "goto-chg" "general"))
  (add-to-list 'load-path (expand-file-name (concat "elpaca/sources/" package) evil-edit-test-root)))
(require 'evil)
(require 'general)
(evil-mode 1)

;; Evaluate only this block, without loading packages or touching Org files.
(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" evil-edit-test-root))
  (emacs-lisp-mode)
  (check-parens)
  (goto-char (point-min))
  (let (form loaded)
    (condition-case nil
        (while (not loaded)
          (setq form (read (current-buffer)))
          (when (and (eq (car-safe form) 'with-eval-after-load)
                     (equal (cadr form) '(quote evil))
                     (eq (caaddr form) 'defconst)
                     (eq (cadr (nth 2 form)) 'my/evil-paren-pairs))
            (eval form t)
            (setq loaded t)))
      (end-of-file (error "Delimiter configuration was not found")))))

(defmacro evil-edit-test-buffer (text &rest body)
  (declare (indent 1))
  `(save-window-excursion
     (with-temp-buffer
       (switch-to-buffer (current-buffer))
       (emacs-lisp-mode)
       (insert ,text)
       (goto-char (point-min))
       (evil-local-mode 1)
       (evil-normal-state)
       ,@body)))

(ert-deftest evil-edit-normal-binding-overrides-outline-only-in-normal-state ()
  (evil-edit-test-buffer ";; heading\n(value)"
    (outline-minor-mode 1)
    (evil-define-minor-mode-key 'normal 'outline-minor-mode (kbd "C-k") #'outline-backward-same-level)
    (should (eq (key-binding (kbd "C-k")) #'my/edit-kill))
    (evil-insert-state)
    (should-not (eq (key-binding (kbd "C-k")) #'my/edit-kill))))

(ert-deftest evil-edit-selects-smallest-nested-pair-and-keeps-point ()
  (evil-edit-test-buffer "(outer [inner])"
    (search-forward "inn")
    (let ((position (point)))
      (dolist (inclusive '(nil t))
        (let ((range (my/evil-paren-range 1 nil nil nil inclusive)))
          (should (equal (buffer-substring (car range) (cadr range))
                         (if inclusive "[inner]" "inner")))
          (should (= position (point))))))))

(ert-deftest evil-edit-quote-text-objects-include-delimiters-for-ag ()
  (evil-edit-test-buffer "\"hello\""
    (forward-char 3)
    (should (equal (cl-subseq (my/evil-paren-range 1 nil nil nil nil) 0 2) '(2 7)))
    (should (equal (cl-subseq (my/evil-paren-range 1 nil nil nil t) 0 2) '(1 8)))))

(ert-deftest evil-edit-c-k-deletes-inner-and-updates-delete-register ()
  (evil-edit-test-buffer "(outer [inner])"
    (search-forward "inn")
    (execute-kbd-macro (kbd "C-k"))
    (should (equal (buffer-string) "(outer [])"))
    (should (equal (evil-get-register ?-) "inner"))
    (should-not unread-command-events)))

(ert-deftest evil-edit-c-k-deletes-both-opening-and-closing-delimiters ()
  (dolist (text '("(hello)" "[hello]" "{hello}" "<hello>" "\"hello\""))
    (dolist (closing '(nil t))
      (evil-edit-test-buffer text
        (when closing (goto-char (1- (point-max))))
        (execute-kbd-macro (kbd "C-k"))
        (should (equal (buffer-string) ""))))))

(ert-deftest evil-edit-single-quotes-excluded-only-in-lisp ()
  (evil-edit-test-buffer "'hello'"
    (forward-char 3)
    (should-not (my/evil-paren-range 1 nil nil nil nil))
    (text-mode)
    (evil-normal-state)
    (my/edit-kill 1)
    (should (equal (buffer-string) "''"))))

(ert-deftest evil-edit-no-surrounding-pair-does-not-delete-a-later-one ()
  (evil-edit-test-buffer "plain text (later)"
    (should-error (my/edit-kill 1) :type 'user-error)
    (should (equal (buffer-string) "plain text (later)"))
    (should-not unread-command-events)))

(ert-deftest evil-edit-whitespace-outside-quotes-does-not-count-as-enclosed ()
  (dolist (text '("   \"later\"" "\"earlier\"   "))
    (evil-edit-test-buffer text
      (when (= (char-after) ?\") (goto-char (1- (point-max))))
      (should-error (my/edit-kill 1) :type 'user-error)
      (should (equal text (buffer-string))))))

(ert-deftest evil-edit-ig-and-ag-work-as-real-evil-text-objects ()
  (dolist (keys '("dig" "dag"))
    (evil-edit-test-buffer "(outer [inner])"
      (search-forward "inn")
      (execute-kbd-macro keys)
      (should (equal (buffer-string) (if (equal keys "dig") "(outer [])" "(outer )")))))
  (evil-edit-test-buffer "\"hello\""
    (forward-char 3)
    (execute-kbd-macro "dag")
    (should (equal (buffer-string) ""))))

(ert-deftest evil-edit-c-k-escaped-quote-deletes-contents ()
  (evil-edit-test-buffer "\"a \\\" b\""
    (search-forward "\\")
    (my/edit-kill 1)
    (should (equal (buffer-string) "\"\""))))

(ert-deftest evil-edit-c-k-is-one-undo-step ()
  (evil-edit-test-buffer "(hello)"
    (buffer-enable-undo)
    (setq buffer-undo-list nil)
    (execute-kbd-macro (kbd "C-k"))
    (undo-boundary)
    (undo-only 1)
    (should (equal (buffer-string) "(hello)"))))

(ert-deftest evil-edit-c-k-repeats-with-dot ()
  (evil-edit-test-buffer "(one) (two)"
    (execute-kbd-macro (kbd "C-k"))
    (search-forward "(")
    (backward-char)
    (execute-kbd-macro (kbd "."))
    (should (equal (string-trim (buffer-string)) ""))))

;;; evil-edit-tests.el ends here
