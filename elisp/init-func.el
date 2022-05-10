;;; init-func.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-func.el
;; Description: Initialize Functions
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Sun Jun  9 17:53:44 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri Mar  4 20:35:31 2022 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes functions
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

(eval-when-compile
  (require 'init-global-config))

;; BetterMiniBuffer
(defun abort-minibuffer-using-mouse ()
  "Abort the minibuffer when using the mouse."
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'abort-minibuffer-using-mouse)

;; keep the point out of the minibuffer
(setq-default minibuffer-prompt-properties '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt))
;; -BetterMiniBuffer

(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))


(random t)
(defun get-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation.  The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)

  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6)) ) )


(defun my-rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      (kill-buffer)
      (find-file file-to))))

(defun my-copy-file()
  "Copy file while using current file as default."
  (interactive)
  (copy-file
   (read-file-name "Copy from: " default-directory buffer-file-name)
   (read-file-name "Copy to:" default-directory)))

(defun my-delete-file()
  "Delete file while using current file as default."
  (interactive)
  (let ((file-name (read-file-name "Delete: " default-directory (buffer-file-name))))
    (cond
     ((file-directory-p file-name) (delete-directory file-name t))
     ((file-exists-p file-name) (delete-file file-name))
     (t (message "Not found!")))
    (unless (file-exists-p (buffer-file-name))
      (kill-current-buffer))))

(defun +my-imenu ()
  "Consult-outline in `org-mode' unless imenu."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (consult-outline)
    (consult-imenu)))


(defun my-open-recent ()
  "Open recent directory in dired or file otherwise."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let* ((candidates (if (derived-mode-p 'dired-mode)
                         (delete-dups
                          (append (mapcar 'file-name-directory recentf-list)))
                       (mapcar #'abbreviate-file-name
                               (-filter (lambda (filename) (not (file-directory-p filename)))  recentf-list)))))
    (find-file
     (consult--read
      candidates
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history
      ))))

(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
