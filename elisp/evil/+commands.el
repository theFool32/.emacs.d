;;; evil/+commands.el --- -*- lexical-binding: t -*-
;;
;; Filename: +commands.el
;; Description: Evil commands map
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Fri Mar  6 19:52:14 2020 (+0800)
;; Last-Updated:
;;           By:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
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
;;
;;; Custom commands
(evil-ex-define-cmd "git"         #'magit-status)         ; open magit status window
(evil-ex-define-cmd "gstage"      #'magit-stage)
(evil-ex-define-cmd "gunstage"    #'magit-unstage)
(evil-ex-define-cmd "gblame"      #'magit-blame)

;;; Dealing with buffers
(evil-ex-define-cmd "messages"    #'view-echo-area-messages)

;;; Project tools
(evil-ex-define-cmd "er[rors]"    #'flycheck-list-errors)

;;; Org-mode
(evil-ex-define-cmd "cap"         #'org-capture)

(provide 'evil/+commands)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +commands.el ends here
