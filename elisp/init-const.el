;;; init-const.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-const.el
;; Description: Initialize Constants
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Mar 18 14:20:54 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri Sep  3 23:24:04 2021 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d constants
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes constants
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
  (require 'init-custom))

;; Load env
(defconst my-env-file (concat user-emacs-directory "env"))
(when (and (or (display-graphic-p)
               (daemonp))
           (file-exists-p my-env-file))
  (defun my-load-envvars-file (file &optional noerror)
    "Read and set envvars from FILE.
If NOERROR is non-nil, don't throw an error if the file doesn't exist or is
unreadable. Returns the names of envvars that were changed."
    (if (not (file-readable-p file))
        (unless noerror
          (signal 'file-error (list "Couldn't read envvar file" file)))
      (let (envvars environment)
        (with-temp-buffer
          (save-excursion
            (insert "\n")
            (insert-file-contents file))
          (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
            (push (match-string 1) envvars)
            (push (buffer-substring
                   (match-beginning 1)
                   (1- (or (save-excursion
                             (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                               (line-beginning-position)))
                           (point-max))))
                  environment)))
        (when environment
          (setq process-environment
                (append (nreverse environment) process-environment)
                exec-path
                (if (member "PATH" envvars)
                    (append (split-string (getenv "PATH") path-separator t)
                            (list exec-directory))
                  exec-path)
                shell-file-name
                (if (member "SHELL" envvars)
                    (or (getenv "SHELL") shell-file-name)
                  shell-file-name))
          envvars))))

  (my-load-envvars-file my-env-file))

;; UserInfo
(setq user-full-name "theFool32")
(setq user-mail-address "saber.rl32@gmail.com")
;; -UserInfo

(setq default-directory (concat (getenv "HOME") "/"))

;; Consts
(defconst *sys/gui*
  (or (display-graphic-p) (daemonp))
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *sys/mac_arm*
  (and *sys/mac* (string-prefix-p "aarch" system-configuration))
  "Are we running on a Arm Mac system?")

(defconst *sys/root*
  (string-equal "root" (getenv "USER"))
  "Are you a ROOT user?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *find*
  (executable-find "find")
  "Do we have GNU find?")

(defconst *python*
  (executable-find "python")
  "Do we have python?")

(defconst *python3*
  (executable-find "python3")
  "Do we have python3?")

(defconst *gcc*
  (executable-find "gcc")
  "Do we have gcc?")

(defconst *git*
  (executable-find "git")
  "Do we have git?")

(defconst *pdflatex*
  (executable-find "pdflatex")
  "Do we have pdflatex?")

(defconst *clangd*
  (or (executable-find "clangd")  ;; usually
      (executable-find "/usr/local/opt/llvm/bin/clangd"))  ;; macOS
  "Do we have clangd?")

(defconst *rga*
  (executable-find "rga")
  "Do we have rga")

(defconst *fd*
  (executable-find "fd")
  "Do we have fd")

;; -Consts

(provide 'init-const)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-const.el ends here
