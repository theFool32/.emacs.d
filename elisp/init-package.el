;;; init-package.el --- -*- lexical-binding: t -*-

;; Straight

(setq straight--process-log nil
      straight-vc-git-default-clone-depth 1
      straight-use-package-by-default t
      ;; straight-check-for-modifications '(check-on-save find-when-checking)
      straight-check-for-modifications nil
      )

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

(defun +set-github-mirror (oldfunc &rest args)
  (let ((url (apply oldfunc args)))
    (replace-regexp-in-string (rx (group "github.com"))
                              "hub.fastgit.org" url nil nil 1)))
;; (advice-add 'straight-vc-git--encode-url :around #'+set-github-mirror)
;; -Straight

(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)
(setq byte-compile-warnings '(cl-functions))

(add-to-list 'straight-built-in-pseudo-packages 'eglot)
(add-to-list 'straight-built-in-pseudo-packages 'tramp)
(add-to-list 'straight-built-in-pseudo-packages 'use-package)

(defun +my/check-straight-repos ()
  (interactive)
  (find-file (read-file-name "Repos: " "~/.emacs.d/straight/repos/")))

;; DimPac
(use-package diminish)
;; -DimPac

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
