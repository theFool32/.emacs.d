;;; init-package.el --- -*- lexical-binding: t -*-

(defvar +use-package--deferred-pkgs '(t))
(defun use-package-handler/:after-call (name _keyword hooks rest state)
  "Add keyword `:after-call' to `use-package'.
The purpose of this keyword is to expand the lazy-loading
capabilities of `use-package'.  Consult `use-package-concat' and
`use-package-process-keywords' for documentations of NAME, HOOKS,
REST and STATE."
  (if (plist-get state :demand)
      (use-package-process-keywords name rest state)
    (let ((fn (make-symbol (format "grandview--after-call-%s-h" name))))
      (use-package-concat
       `((fset ',fn
               (lambda (&rest _)
                 (condition-case e
                     (let ((default-directory user-emacs-directory))
                       (require ',name))
                   ((debug error)
                    (message "Failed to load deferred package %s: %s" ',name e)))
                 (when-let (deferral-list (assq ',name +use-package--deferred-pkgs))
                   (dolist (hook (cdr deferral-list))
                     (advice-remove hook #',fn)
                     (remove-hook hook #',fn))
                   (setq +use-package--deferred-pkgs
                         (delq deferral-list +use-package--deferred-pkgs))
                   (unintern ',fn nil)))))
       (cl-loop for hook in hooks
                collect (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                            `(add-hook ',hook #',fn)
                          `(advice-add #',hook :before #',fn)))
       `((unless (assq ',name +use-package--deferred-pkgs)
           (push '(,name) +use-package--deferred-pkgs))
         (nconc (assq ',name +use-package--deferred-pkgs)
                '(,@hooks)))
       (use-package-process-keywords name rest state)))))
(require 'use-package-core)
(push :after-call use-package-deferring-keywords)
(setq use-package-keywords (use-package-list-insert :after-call use-package-keywords :after))
(defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)


;; Straight
(setq straight--process-log nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      ;; straight-check-for-modifications '(check-on-save find-when-checking)
      straight-check-for-modifications nil)

(unless (featurep 'straight)
  (defvar bootstrap-version)

  (let ((bootstrap-file (concat user-emacs-directory
                                "straight/repos/straight.el/bootstrap.el"))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; (defun +set-github-mirror (oldfunc &rest args)
;;   (let ((url (apply oldfunc args)))
;;     (replace-regexp-in-string (rx (group "github.com"))
;;                               "hub.fastgit.org" url nil nil 1)))
;; (advice-add 'straight-vc-git--encode-url :around #'+set-github-mirror)

(add-to-list 'straight-built-in-pseudo-packages 'eglot)
(add-to-list 'straight-built-in-pseudo-packages 'tramp)
(add-to-list 'straight-built-in-pseudo-packages 'use-package)
(add-to-list 'straight-built-in-pseudo-packages 'project)
(add-to-list 'straight-built-in-pseudo-packages 'org)
(add-to-list 'straight-built-in-pseudo-packages 'xref)
;; -Straight

(defun +my/check-straight-repos ()
  (interactive)
  (find-file (read-file-name "Repos: " "~/.emacs.d/straight/repos/")))

;; DimPac
(use-package diminish)
;; -DimPac

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
