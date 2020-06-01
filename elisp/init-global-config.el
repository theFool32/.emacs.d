;;; init-global-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-global-config.el
;; Description: Initialize Global Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 14:01:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Mon Jun  1 15:20:58 2020 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes global configurations
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
  (require 'init-const))

;; SudoEditPac
(use-package sudo-edit
  :commands (sudo-edit))
;; -SudoEditPac

;; UTF8Coding
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

;; EditExp
;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

;; Replace selection on insert
(delete-selection-mode 1)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
;; -EditExp

;; History
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude '((expand-file-name package-user-dir)
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
                     "COMMIT_EDITMSG\\'")))

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
;; -History

;; SmallConfigs
;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq-default create-lockfiles nil)
(setq-default make-backup-files nil)
(setq create-lockfiles nil)
(setq make-backup-files nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

;; Sync my code on save
(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))
;; (add-hook 'after-save-hook (lambda () (call-process-shell-command "rc" nil 0)))
(defun +my-sync-code()
  (call-process-shell-command "rc" nil 0)
  )
(advice-add #'save-buffer :after (η #'+my-sync-code))

;; _ as part of a word
(modify-syntax-entry ?_ "w")
(defalias 'forward-evil-word 'forward-evil-symbol)

(if (and *sys/gui* *sys/linux*)
    (setq default-directory "/home/lijie")
  )
(if *sys/mac*
    (setq default-directory "/Users/lijie"))

;; Auto save session
;; (setq desktop-save-mode 1)


;; For GC
(setq gc-max (* 512 1024 1024))
(setq gc-min (* 16 1024 1024))

(setq gc-cons-threshold gc-min)

(defun my-minibuffer-setup-hook nil (setq gc-cons-threshold gc-max))
(defun my-minibuffer-exit-hook nil (setq gc-cons-threshold gc-min))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; TODO: check the influence of GC
;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t
;;                        'garbage-collect))

;; GC measure
;; (defmacro k-time (&rest body)
;;   "Measure and return the time it takes evaluating BODY."
;;   `(let ((time (current-time)))
;;      ,@body
;;      (float-time (time-since time))))

;; (defvar k-gc-timer
;;   (run-with-idle-timer 15 t
;;                        (lambda ()
;;                          (message "Garbage Collector has run for %.06fsec"
;;                                   (k-time (garbage-collect))))))

(advice-add #'save-buffers-kill-terminal :before
            (lambda (&optional args)
              (interactive) (setq client-revert-bufferp (current-buffer))))

(defun client-revert-buffer ()
  "revert the buffer if client-revert-bufferp is defined"
  (interactive)
  (if (boundp 'client-revert-bufferp)
      (switch-to-buffer client-revert-bufferp)))

(provide 'init-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global-config.el ends here
