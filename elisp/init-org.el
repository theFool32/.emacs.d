;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Wed Mar 25 21:17:12 2020 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d org toc-org htmlize ox-gfm
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes org toc-org htmlize ox-gfm
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

(require 'org/+tables)
(require 'org/+funcs)

;; OrgPac
(defvar org-self-dir "~/Dropbox/org-notes/")
(use-package org
  :ensure nil
  :hook (org-mode . org-indent-mode)
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "|" "DONE")))
  (org-agenda-window-setup 'other-window)
  (org-directory (expand-file-name org-self-dir))
  (org-ellipsis " ▼ ")
  (org-babel-python-command "python3")
  (org-bullets-bullet-list '("#"))
  (org-tags-column -77)
  :config
  (when (file-directory-p org-directory)
    (setq org-agenda-files (list org-directory)))
  (unless (version< org-version "9.2")
    (require 'org-tempo))

  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-capture-defaults-h)

  ;; binding
  (evil-set-initial-state 'org-agenda-mode 'motion)
  ;; (evil-define-key 'normal org-mode-map
  ;;   "o" #'+org/insert-item-below
  ;;   "O" #'+org/insert-item-above
  ;;   )

  ;; org screenshot for macos
  (require 'org/+screenshot)

  (defvar +org-capture-file (concat org-self-dir "main.org"))
  (setq org-capture-templates
        '(("t" "Journal" entry
           (file+headline +org-capture-file "GTD")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ))

  (setq org-log-into-drawer "LOGBOOK")


  ;; Schedule/deadline popup with default time
  ;; (defvar org-default-time "08:30"
  ;; "The default time for deadlines.")

  ;; (defun advise-org-default-time (func arg &optional time)
  ;;   (let ((old-time (symbol-function #'org-read-date)))
  ;;     (cl-letf (((symbol-function #'org-read-date)
  ;;                #'(lambda (&optional a b c d default-time f g)
  ;;                    (let ((default-time (or default-time
  ;;                                            org-default-time)))
  ;;                      (apply old-time a b c d f default-time g)
  ;;                      ))))
  ;;       (apply func arg time))))

  ;; (advice-add #'org-deadline :around #'advise-org-default-time)
  ;; (advice-add #'org-schedule :around #'advise-org-default-time)


  ;; (setq bibtex-completion-bibliography '( "~/Dropbox/Paper/egbib.bib" ) ;the major bibtex file
  ;;       bibtex-completion-library-path "~/Dropbox/org-notes/reference/pdf/" ;the directory to store pdfs
  ;;       bibtex-completion-notes-path "~/Dropbox/org-notes/ref.org" ;the note file for reference notes
  ;;       ;; org-directory "~/Dropbox/org"
  ;;       org-ref-default-bibliography '( "~/Dropbox/Paper/egbib.bib" )
  ;;       org-ref-bibliography-notes "~/Dropbox/org-notes/ref.org"
  ;;       org-ref-pdf-directory "~/Dropbox/org-notes/reference/pdf/"
  ;;       )

  (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  ;; TocOrgPac
  (use-package toc-org
    :after org
    :hook (org-mode . toc-org-enable)
    :config (setq toc-org-hrefify-default "gh")
    )
  ;; -TocOrgPac

  )
;; -OrgPac

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
