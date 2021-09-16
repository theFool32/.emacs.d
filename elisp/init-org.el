;;; init-org.el --- -*- lexical-binding: t -*-
;;


;; Filename: init-org.el
;; Description: Initialize Org, Toc-org, HTMLize, OX-GFM
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 11:09:30 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Sep 16 11:28:40 2021 (+0800)
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
(defvar +org-capture-file-gtd (concat +self/org-base-dir "gtd.org"))
(defvar +org-capture-file-idea (concat +self/org-base-dir "ideas.org"))
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat +self/org-base-dir "tickler.org"))
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-enable-auto-update-cookies-h))
  :bind (:map org-mode-map
              ([tab] . org-cycle))
  :custom
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-capture-bookmark nil) ;; TODO: no bookmark for refile
  (org-log-done 'time)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-directory (expand-file-name +self/org-base-dir))
  (org-ellipsis " ▼ ")
  (org-babel-python-command "python3")
  (org-bullets-bullet-list '("#"))

  :config
  ;; TODO: slow
  ;; (defvar load-language-list '((emacs-lisp . t)
  ;;                              (python . t)
  ;;                              (C . t)))
  ;; (org-babel-do-load-languages 'org-babel-load-languages
  ;;                              load-language-list)

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-capture-defaults-h)
  ;; org screenshot for macos
  ;; (require 'org/+screenshot)

  (setq org-log-into-drawer "LOGBOOK")
  (setq org-agenda-files (list +org-capture-file-gtd
                               +org-capture-file-tickler))
  (setq org-refile-targets '((+org-capture-file-gtd :level . 1)
                             (+org-capture-file-someday :level . 1)
                             (+org-capture-file-tickler :level . 1)))
  (setq org-log-into-drawer t)
  (setq org-tag-alist '(("lab" . ?L) ("academic" . ?a) ("life" . ?l) ("paper" . ?p) ("emacs" . ?e)))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file+headline +org-capture-file-gtd "Next Actions")
           "* TODO %i%? [/] \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
          ("w" "Waiting for" entry
           (file+headline +org-capture-file-tickler "Tickler")
           "* %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("m" "Maybe" entry
           (file+headline +org-capture-file-someday "Some day/maybe")
           "* %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file-idea "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          )
        org-todo-keywords
        '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))
        ;; org-agenda-window-setup 'other-window
        )

  ;; (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  (add-hook 'after-change-major-mode-hook
            (lambda () (if (equal show-paren-mode 't)
    		          (when (derived-mode-p 'org-mode)
    		            (show-paren-mode -1))
                    (show-paren-mode 1))))

  ;; https://emacs-china.org/t/topic/2119/15
  (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
    (require 'cal-china)
    (if year
        (let* ((d-date (diary-make-date lunar-month lunar-day year))
               (a-date (calendar-absolute-from-gregorian d-date))
               (c-date (calendar-chinese-from-absolute a-date))
               (cycle (car c-date))
               (yy (cadr c-date))
               (y (+ (* 100 cycle) yy)))
          (diary-chinese-anniversary lunar-month lunar-day y mark))
      (diary-chinese-anniversary lunar-month lunar-day year mark)))

  ;; binding
  (with-eval-after-load 'general
    (general-define-key :states '(normal insert)
                        :keymaps 'org-mode-map
                        "C-<return>" #'+org/insert-item-below
                        "C-S-<return>" 'org-insert-subheading
                        )
    (general-define-key :states '(normal)
                        :keymaps 'org-mode-map
                        "<return>" #'+org/dwim-at-point)

    (local-leader-def
      :keymaps 'org-mode-map
      "'" 'org-edit-special
      "*" 'org-ctrl-c-star
      "+" 'org-ctrl-c-minus
      "," 'org-switchb
      ;; "." 'org-goto

      "." 'consult-org-heading

      "A" 'org-archive-subtree
      "e" 'org-export-dispatch
      "f" 'org-footnote-new
      "h" 'org-toggle-heading
      "i" 'org-toggle-item
      "I" 'org-toggle-inline-images
      "n" 'org-store-link
      "o" 'org-set-property
      "q" 'org-set-tags-command
      "t" 'org-todo
      "T" 'org-todo-list
      "x" 'org-toggle-checkbox

      "a" '(:wk "attackments")
      "aa" 'org-attach
      "ad" 'org-attach-delete-one
      "aD" 'org-attach-delete-all
      "an" 'org-attach-new
      "ao" 'org-attach-open
      "aO" 'org-attach-open-in-emacs
      "ar" 'org-attach-reveal
      "aR" 'org-attach-reveal-in-emacs
      "au" 'org-attach-url
      "as" 'org-attach-set-directory
      "aS" 'org-attach-sync

      "b"  '(:wk "tables")
      "b-" 'org-table-insert-hline
      "ba" 'org-table-align
      "bb" 'org-table-blank-field
      "bc" 'org-table-create-or-convert-from-region
      "be" 'org-table-edit-field
      "bf" 'org-table-edit-formulas
      "bh" 'org-table-field-info
      "bs" 'org-table-sort-lines
      "br" 'org-table-recalculate
      "bR" 'org-table-recalculate-buffer-tables
      "bd" '(:wk "delete")
      "bdc" 'org-table-delete-column
      "bdr" 'org-table-kill-row
      "bi" '(:wk "insert")
      "bic" 'org-table-insert-column
      "bih" 'org-table-insert-hline
      "bir" 'org-table-insert-row
      "biH" 'org-table-hline-and-move
      "bt" '("toggle")
      "btf" 'org-table-toggle-formula-debugger
      "bto" 'org-table-toggle-coordinate-overlays

      "c" '(:wk "clock")
      "cc" 'org-clock-cancel
      "cd" 'org-clock-mark-default-task
      "ce" 'org-clock-modify-effort-estimate
      "cE" 'org-set-effort
      "cg" 'org-clock-goto
      "ci" 'org-clock-in
      "cI" 'org-clock-in-last
      "co" 'org-clock-out
      "cr" 'org-resolve-clocks
      "cR" 'org-clock-report
      "ct" 'org-evaluate-time-range
      "c=" 'org-clock-timestamps-up
      "c-" 'org-clock-timestamps-down

      "d" '(:wk "date/deadline")
      "dd" 'org-deadline
      "ds" 'org-schedule
      "dt" 'org-time-stamp
      "dT" 'org-time-stamp-inactive

      "D" '+org/archive-done-tasks

      "g" '(:wk "goto")
      "gc" 'org-clock-goto
      "gi" 'org-id-goto
      "gr" 'org-refile-goto-last-stored
      "gx" 'org-capture-goto-last-stored

      "l" '(:wk "links")
      "lc" 'org-cliplink
      "ld" '+org/remove-link
      "li" 'org-id-store-link
      "ll" 'org-insert-link
      "lL" 'org-insert-all-links
      "ls" 'org-store-link
      "lS" 'org-insert-last-stored-link
      "lt" 'org-toggle-link-display

      "P" '(:wk "publish")
      "Pa" 'org-publish-all
      "Pf" 'org-publish-current-file
      "Pp" 'org-publish
      "PP" 'org-publish-current-project
      "Ps" 'org-publish-sitemap

      "r" '(:wk "refile")
      "r." '+org/refile-to-current-file
      "rc" '+org/refile-to-running-clock
      "rl" '+org/refile-to-last-location
      "rf" '+org/refile-to-file
      "ro" '+org/refile-to-other-window
      "rr" 'org-refile

      "s" '(:wk "tree/subtree")
      "sa" 'org-toggle-archive-tag
      "sb" 'org-tree-to-indirect-buffer
      "sd" 'org-cut-subtree
      "sh" 'org-promote-subtree
      "sj" 'org-move-subtree-down
      "sk" 'org-move-subtree-up
      "sl" 'org-demote-subtree
      "sn" 'org-narrow-to-subtree
      "sr" 'org-refile
      "ss" 'org-sparse-tree
      "sA" 'org-archive-subtree
      "sN" 'widen
      "sS" 'org-sort

      "p" '(:wk "priority")
      "pd" 'org-priority-down
      "pp" 'org-priority
      "pu" 'org-priority-up

      "z" '(:wk "Download")
      "zc" 'org-download-clipboard
      "zd" 'org-download-delete
      "zi" 'org-download-image
      "zy" 'org-download-yank
      "ze" 'org-download-edit
      "zr" 'org-download-rename-at-point
      "zR" 'org-download-rename-last-file
      "zs" 'org-download-screenshot
      )

    (local-leader-def
      :keymaps 'org-agenda-mode-map
      "d" '(:wk "date/deadline")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule

      "c" '(:wk "clock")
      "cc" 'org-agenda-clock-cancel
      "cg" 'org-agenda-clock-goto
      "ci" 'org-agenda-clock-in
      "co" 'org-agenda-clock-out
      "cr" 'org-agenda-clockreport-mode
      "cs" 'org-agenda-show-clocking-issues

      "p" '(:wk "priority")
      "pd" 'org-agenda-priority-down
      "pp" 'org-agenda-priority
      "pu" 'org-agenda-priority-up

      "q" 'org-agenda-set-tags
      "r" 'org-agenda-refile
      "t" 'org-agenda-todo)
    )
  )
;; -OrgPac

(use-package org-download
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :after org
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))

(use-package org-contrib
  :after org)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :custom
  (org-superstar-remove-leading-stars t)
  (org-superstar-special-todo-items t))

(use-package calfw
  :commands (cfw:open-org-calendar)
  :straight (:host github :repo "zemaye/emacs-calfw")
  :bind (:map cfw:calendar-mode-map
              ("s" . cfw:show-details-command))
  :custom
  (cfw:display-calendar-holidays nil)
  :config
  (with-eval-after-load 'calfw
	(use-package calfw-ical
	  :straight (:host github :repo "zemaye/emacs-calfw"))
	(use-package calfw-org
	  :straight (:host github :repo "zemaye/emacs-calfw"))
	(use-package calfw-cal
	  :straight (:host github :repo "zemaye/emacs-calfw"))))

;; -Notification only for mac os
(when *sys/mac*
  (require 'appt)

  (setq appt-time-msg-list nil) ;; clear existing appt list
  (setq appt-display-interval '5) ;; warn every 5 minutes from t - appt-message-warning-time
  (setq
   appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
   appt-display-mode-line nil ;; don't show in the modeline
   appt-display-format 'window) ;; pass warnings to the designated window function
  (setq appt-disp-window-function (function ct/appt-display-native))

  (appt-activate 1) ;; activate appointment notification
                                        ; (display-time) ;; Clock in modeline

  ;; brew install terminal-notifier
  (defun ct/send-notification (title msg)
    (let ((notifier-path (executable-find "terminal-notifier")))
      (start-process
       "Appointment Alert"
       nil
       notifier-path
       "-message" msg
       "-title" title
       "-sender" "org.gnu.Emacs"
       "-activate" "org.gnu.Emacs")))
  (defun ct/appt-display-native (min-to-app new-time msg)
    (ct/send-notification
     (format "Appointment in %s minutes" min-to-app) ; Title
     (format "%s" msg))) ; Message/detail text
  ;; Agenda-to-appointent hooks
  (org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
  (run-at-time "24:01" 3600 'org-agenda-to-appt) ;; update appt list hourly
  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
  )
;; -Notification

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
