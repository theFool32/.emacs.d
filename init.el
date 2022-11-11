;;; init.el --- -*- lexical-binding: t -*-

(load (concat user-emacs-directory "early-init") nil 'nomessage)
(setq auto-mode-case-fold nil)
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

;; LoadPath
(let ((base (expand-file-name "elisp" user-emacs-directory)))
    (unless (member base load-path)
      (add-to-list 'load-path base))
    (dolist (f (directory-files base))
      (let ((name (concat base "/" f)))
        (when (and (file-directory-p name)
                   (not (equal f ".."))
                   (not (equal f ".")))
          (unless (member base load-path)
            (add-to-list 'load-path name))))))
;; -LoadPath

;; InitPrivate
;; Load init-custom.el if it exists
(load (expand-file-name "init-custom.el" user-emacs-directory) nil 'nomessage)
;; -InitPrivate

(defvar +my/first-input-hook nil)
(defun +my/first-input-hook-fun ()
  (when +my/first-input-hook
    (run-hooks '+my/first-input-hook)
    (setq +my/first-input-hook nil))
  (remove-hook 'pre-command-hook '+my/first-input-hook-fun))
(add-hook 'pre-command-hook '+my/first-input-hook-fun)

(require 'init-package)
(require 'init-const)
;; (use-package esup)

;; Global Functionalities
(require 'init-evil)
(require 'init-func)
(require 'init-global-config)
(require 'init-search)
(require 'init-mini-buffer)
(require 'init-tree-sitter)

(require 'init-dired)
(require 'init-utils)
(require 'init-bindings)

;; User Interface Enhancements
(require 'init-ui-config)
(require 'init-pretty-code)
(require 'init-scroll)
(require 'init-highlight)

;; General Programming
(require 'init-magit)
(require 'init-flycheck)
(require 'init-parens)
(require 'init-indent)
(require 'init-edit)
(require 'init-lsp)
(require 'init-complete)

;; Programming
(require 'init-prog)
(require 'init-python)
(require 'init-latex)

;; Miscellaneous
(require 'init-org)
(require 'init-ebib)
(require 'init-rime)
(require 'init-lookup)
(require 'init-shell)
(require 'init-mail)
(require 'init-persp)

(add-hook 'window-setup-hook
          #'(lambda ()
              ;; (cfw:open-org-calendar)
              (+my/open-org-agenda)
              (evil-window-right 1)))

(provide 'init)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init.el ends here
