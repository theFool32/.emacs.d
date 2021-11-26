;;; init-package.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-package.el
;; Description: Initialize Package Management for M-EMACS
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 10:53:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Nov 24 14:46:23 2021 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d packages use-package
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file initializes packages from melpa using use-package macro
;; as well as auto-package-update, diminish, gnu-elpa-keyring-update
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
                              "github.com.cnpmjs.org" url nil nil 1)))
(advice-add 'straight-vc-git--encode-url :around #'+set-github-mirror)
;; -Straight

(setq use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics t)
(setq byte-compile-warnings '(cl-functions))
(straight-use-package 'use-package)

;; DimPac
(use-package diminish)
;; -DimPac

(provide 'init-package)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
