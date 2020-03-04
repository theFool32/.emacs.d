;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Dec 24 14:05:45 2019 (-0500)
;;           By: Mingde (Matthew) Zeng
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
;; (defvar org-self-dir "~/org-notes/")
(defvar org-self-dir "~/Dropbox/org-notes")
(use-package org
  :ensure t
  :hook (org-mode . org-indent-mode)
  :custom
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-todo-keywords
   '((sequence "TODO" "IN-PROGRESS" "REVIEW" "|" "DONE")))
  (org-agenda-window-setup 'other-window)
  (org-directory (expand-file-name org-self-dir))
  (org-ellipsis " ▼ ")
  (org-babel-python-command "python3")
  (org-bullets-bullet-list '("#"))
  :config
  (when (file-directory-p org-directory)
    (setq org-agenda-files (list org-directory)))
  (unless (version< org-version "9.2")
    (require 'org-tempo))

  ;; org 截图
  (require 'org/+screenshot)

  (defvar +org-capture-todo-file (concat org-self-dir "todo.org"))
  (defvar +org-capture-note-file (concat org-self-dir "note.org"))
  (defvar +org-capture-journal-file (concat org-self-dir "journal.org"))
  (defvar +org-capture-papers-file (concat org-self-dir "papers.org"))
  (defvar +org-capture-ideas-file (concat org-self-dir "ideas.org"))
  (setq org-capture-templates
        '(("t" "Personal todo" entry
           (file+headline +org-capture-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Personal notes" entry
           (file+headline +org-capture-note-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("j" "Journal" entry
           (file+headline +org-capture-journal-file "Inbox")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("r" "Paper" entry
           (file+headline +org-capture-papers-file "Papers")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-ideas-file "Idea")
           "* %u %?\n%i" :prepend t :kill-buffer t)

          ("pt" "Project todo" entry    ; {project-root}/todo.org
           (file+headline +org-capture-project-todo-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pn" "Project notes" entry   ; {project-root}/notes.org
           (file+headline +org-capture-project-notes-file "Inbox")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("pc" "Project changelog" entry ; {project-root}/changelog.org
           (file+headline +org-capture-project-notes-file "Unreleased")
           "* TODO %?\n%i" :prepend t :kill-buffer t)))

  (setq org-log-into-drawer "LOGBOOK")


  ;; Schedule/deadline popup with default time
  (defvar org-default-time "08:30"
    "The default time for deadlines.")

  (defun advise-org-default-time (func arg &optional time)
    (let ((old-time (symbol-function #'org-read-date)))
      (cl-letf (((symbol-function #'org-read-date)
                 #'(lambda (&optional a b c d default-time f g)
                     (let ((default-time (or default-time
                                            org-default-time)))
                       (apply old-time a b c d f default-time g)
                       ))))
        (apply func arg time))))

  (advice-add #'org-deadline :around #'advise-org-default-time)
  (advice-add #'org-schedule :around #'advise-org-default-time)


  ;; (setq bibtex-completion-bibliography '( "~/Dropbox/Paper/egbib.bib" ) ;the major bibtex file
  ;;       bibtex-completion-library-path "~/Dropbox/org-notes/reference/pdf/" ;the directory to store pdfs
  ;;       bibtex-completion-notes-path "~/Dropbox/org-notes/ref.org" ;the note file for reference notes
  ;;       ;; org-directory "~/Dropbox/org"
  ;;       org-ref-default-bibliography '( "~/Dropbox/Paper/egbib.bib" )
  ;;       org-ref-bibliography-notes "~/Dropbox/org-notes/ref.org"
  ;;       org-ref-pdf-directory "~/Dropbox/org-notes/reference/pdf/"
  ;;       )


  (defun org-export-turn-on-syntax-highlight ()
    "Setup variables to turn on syntax highlighting when calling `org-latex-export-to-pdf'."
    (interactive)
    (setq org-latex-listings 'minted
          org-latex-packages-alist '(("" "minted"))
          org-latex-pdf-process
          '("pdflatex -shelnl-escape -interaction nonstopmode -output-directory %o %f"
            "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f")))

  (defun org-export-as-pdf-and-open ()
    "Run `org-latex-export-to-pdf', delete the tex file and open pdf in a new buffer."
    (interactive)
    (save-buffer)
    (let* ((pdf-path (org-latex-export-to-pdf))
           (pdf-name (file-name-nondirectory pdf-path)))
      (if (try-completion pdf-name (mapcar #'buffer-name (buffer-list)))
          (progn
            (kill-matching-buffers (concat "^" pdf-name) t t)
            (find-file-other-window pdf-name))
        (find-file-other-window pdf-name))
      (delete-file (concat (substring pdf-path 0 (string-match "[^\.]*\/?$" pdf-path)) "tex"))))

  (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  (add-hook 'org-load-hook
            #'+org-init-appearance-h
            #'+org-init-agenda-h
            #'+org-init-capture-defaults-h
            )
  )
;; -OrgPac

;; TocOrgPac
(use-package toc-org
  :hook (org-mode . toc-org-enable)
  :config (setq toc-org-hrefify-default "gh")
  )
;; -TocOrgPac

(use-package evil-org
  :hook (org-mode . evil-org-mode)
  :init
  (defvar evil-org-retain-visual-state-on-shift t)
  (defvar evil-org-special-o/O '(table-row))
  (defvar evil-org-use-additional-insert t)
  :config
  (evil-org-set-key-theme)
  (add-hook 'org-tab-first-hook :append
            ;; Only fold the current tree, rather than recursively
            #'+org-cycle-only-current-subtree-h
            ;; Clear babel results if point is inside a src block
            #'+org-clear-babel-results-h)
  )
;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
