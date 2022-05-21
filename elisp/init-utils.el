;;; init-utils.el ---
;;
;; Filename: init-utils.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Thu May 27 22:07:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 39
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(use-package restart-emacs
  :commands restart-emacs)

(use-package atomic-chrome
  :defer
  :commands (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist
	    '(("overleaf\\.com" . LaTeX-mode))))

(use-package tramp
  :defer 1
  :straight nil)

(use-package leetcode
  :straight (:type git :host github :repo "kaiwk/leetcode.el")
  :commands (leetcode)
  :config
  (setq leetcode-prefer-language "python3"))

(use-package project
  :straight nil
  :defer
  :config
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))

  (cl-defmethod project-root ((project (head local)))
    (nth 1 project))
  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project" ".projectile")
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (list 'local root))))))))

  (setq project-find-functions '(project-try-vc my/project-try-local))

  ;;  HACK: auto remember project
  (add-hook 'change-major-mode-hook (lambda ()
                                      (when (and (buffer-file-name)
                                                 (fboundp 'project-current))
                                        (when-let ((root (my-project-root)))
                                          (project-remember-project (project-current)))))))

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :commands vundo
  :defer t
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
