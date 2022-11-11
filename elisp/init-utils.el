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

(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  ;; (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,+self/org-base-dir
                     ,(expand-file-name "~\/.mail\/*")
                     ;; "^/\\(?:ssh\\|scp\\|su\\|sudo\\)?:"
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
                     "COMMIT_EDITMSG\\'"))
  :config
  (defun recentd-track-opened-file ()
    "Insert the name of the directory just opened into the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-add-file default-directory))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (defun recentd-track-closed-file ()
    "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-remove-if-non-kept default-directory)))

  (add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
  (add-hook 'kill-buffer-hook 'recentd-track-closed-file)

  (defun recentf-keep-tramp-predicate (file)
    "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well."
    (cond
     ((file-remote-p file))
     ((file-readable-p file))))
  (custom-set-variables '(recentf-keep '(recentf-keep-tramp-predicate)))

  (defun do-recentf-cleanup ()
    "Clean up not existed files for recentf"
    (interactive)
    (let ((recentf-keep '(recentf-keep-default-predicate)))
      (recentf-cleanup)))
  )

(use-package sudo-edit
  :commands (sudo-edit))

(use-package gcmh
  :hook (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

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
  :straight nil
  :config
  (setq tramp-completion-use-auth-sources nil
        tramp-verbose 0
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil)
  (setq remote-file-name-inhibit-cache nil
        vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp)))

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :commands vundo
  :defer t
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O))

(use-package super-save
  :diminish
  :defer 0.5
  :init
  (setq auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (add-to-list 'super-save-triggers 'switch-to-buffer)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 5)
  (setq save-silently t)
  (super-save-mode 1)
  (advice-add 'super-save-command :override 'save-all-buffers))

(use-package ztree
  :commands ztree-diff
  :defer t)

(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package tab-bar
  :ensure nil
  :hook (after-init . tab-bar-mode)
  :config
  (setq tab-bar-show nil))

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
