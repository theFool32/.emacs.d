;;; init-org.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun Mar 15 14:35:13 2020 (+0800)
;;           By: Jie Li
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
(defvar org-self-dir "~/org-notes/")
;; (defvar org-self-dir "~/Dropbox/org-notes/")
(use-package org
  :ensure nil
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

  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-capture-defaults-h)

  ;; binding
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (evil-define-key 'normal org-mode-map
    "o" #'+org/insert-item-below
    "O" #'+org/insert-item-above
    )

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

  (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  ;; (add-hook 'org-load-hook
  ;;           #'+org-init-appearance-h
  ;;           #'+org-init-agenda-h
  ;;           #'+org-init-capture-defaults-h
  ;;           )
  ;; TocOrgPac
  (use-package toc-org
    :after org
    :hook (org-mode . toc-org-enable)
    :config (setq toc-org-hrefify-default "gh")
    )
  ;; -TocOrgPac

  ;; not work for saving
  ;; (use-package evil-org
  ;;   :after (evil org)
  ;;   :hook (org-mode . evil-org-mode)
  ;;   :quelpa (evil-org-mode :fetcher github :repo "hlissner/evil-org-mode")
  ;;   :init
  ;;   (defvar evil-org-retain-visual-state-on-shift t)
  ;;   (defvar evil-org-special-o/O '(table-row))
  ;;   (defvar evil-org-use-additional-insert t)
  ;;   :config
  ;;   (add-hook 'evil-org-mode-hook
  ;;             (lambda ()
  ;;               (evil-org-set-key-theme)))
  ;;   (require 'evil-org-agenda)
  ;;   (evil-org-agenda-set-keys)
  ;;   (add-hook 'org-tab-first-hook
  ;;             ;; Only fold the current tree, rather than recursively
  ;;             #'+org-cycle-only-current-subtree-h
  ;;             ;; Clear babel results if point is inside a src block
  ;;             #'+org-clear-babel-results-h)
  ;;   )
  ;; (require 'init-site)
  )
;; -OrgPac

;; (use-package org-bullets
;;   :hook (org-mode . org-bullets-mode))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
