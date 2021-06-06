;;; org/+funcs.el --- -*- lexical-binding: t -*-
;;
;; Filename: +funcs.el
;; Description: Org-mode helper
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
;;; Helpers

(defun +org--refresh-inline-images-in-subtree ()
  "Refresh image previews in the current heading/tree."
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

(defun +org--insert-item (direction)
  (let* ((context
          (save-excursion
            (when (bolp)
              (back-to-indentation)
              (forward-char))
            (org-element-lineage
             (org-element-context)
             '(table table-row headline inlinetask item plain-list)
             t)))
         (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (org-beginning-of-item)
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (save-match-data
               (pcase direction
                 (`below
                  (org-end-of-item)
                  (backward-char)
                  (end-of-line)
                  (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
                      (let ((l (line-number-at-pos)))
                        (org-insert-item)
                        (when (= l (line-number-at-pos))
                          (org-next-item)
                          (org-end-of-line)))
                    (insert "\n" (make-string pad 32) (or marker ""))))
                 (`above
                  (org-beginning-of-item)
                  (if (and marker (string-match-p "[0-9]+[).]" marker))
                      (org-insert-item)
                    (insert (make-string pad 32) (or marker ""))
                    (save-excursion (insert "\n")))))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (save-excursion (org-table-insert-row t))
                     (org-table-next-row))
             ('above (save-excursion (org-shiftmetadown))
                     (+org/table-previous-row))))

          ((let ((level (or (org-current-level) 1)))
             (pcase direction
               (`below
                (let (org-insert-heading-respect-content)
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert "\n" (make-string level ?*) " ")))
               (`above
                (org-back-to-heading)
                (insert (make-string level ?*) " ")
                (save-excursion (insert "\n"))))
             (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                         (todo-type (org-element-property :todo-type context)))
               (org-todo (cond ((eq todo-type 'done)
                                (car (+org-get-todo-keywords-for todo-keyword)))
                               (todo-keyword)
                               ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

(defun +org--get-property (name &optional bound)
  (save-excursion
    (let ((re (format "^#\\+%s:[ \t]*\\([^\n]+\\)" (upcase name))))
      (goto-char (point-min))
      (when (re-search-forward re bound t)
        (buffer-substring-no-properties (match-beginning 1) (match-end 1))))))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

;;
;;; Commands

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_ (+org--refresh-inline-images-in-subtree)))))


;; I use this instead of `org-insert-item' or `org-insert-heading' which are too
;; opinionated and perform this simple task incorrectly (e.g. whitespace in the
;; wrong places).
;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;;###autoload
(defun +org/insert-item-above (count)
  "Inserts a new heading, table cell or item above the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'above)))

;;
;;; Hooks

;;;###autoload
(defun +org-update-cookies-h ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      ;; (org-update-parent-todo-statistics) ;; HACK: it does not work to update statistics while the below one works
      (call-interactively 'org-update-statistics-cookies))))

;;;###autoload
(defun +org-enable-auto-update-cookies-h ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (bound-and-true-p evil-local-mode)
    (add-hook 'evil-insert-state-exit-hook #'+org-update-cookies-h nil t))
  (add-hook 'before-save-hook #'+org-update-cookies-h nil t))

(defun +org-init-appearance-h ()
  "Configures the UI for `org-mode'."
  (setq org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-entities-user
        '(("flat"  "\\flat" nil "" "" "266D" "♭")
          ("sharp" "\\sharp" nil "" "" "266F" "♯"))
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-fontify-whole-heading-line t
        org-footnote-auto-label 'plain
        org-hide-leading-stars t
        org-hide-leading-stars-before-indent-mode t
        org-image-actual-width nil
        org-list-description-max-indent 4
        org-priority-faces
        '((?A . error)
          (?B . warning)
          (?C . success))
        org-startup-indented t
        org-tags-column -77
        org-use-sub-superscripts '{}
        ))

(defun +org-init-agenda-h ()
  (unless org-agenda-files
    (setq org-agenda-files (list org-directory)))
  (setq-default
   ;; Don't monopolize the whole frame just for the agenda
   org-agenda-window-setup 'current-window
   org-agenda-inhibit-startup t
   org-agenda-skip-unavailable-files t
   ;; Move the agenda to show the previous 3 days and the next 7 days for a bit
   ;; better context instead of just the current week which is a bit confusing
   ;; on, for example, a sunday
   org-agenda-span 10
   org-agenda-start-on-weekday nil
   org-agenda-start-day "-3d"))

(defun +org-init-capture-defaults-h()
  ;; enter insert state for org-capture
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  )

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))



;; REVIEW These are all proof-of-concept. Refactor me!

;;;###autoload
(defun +org/refile-to-current-file (arg &optional file)
  "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path nil)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-file (arg file)
  "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
  (interactive
   (list current-prefix-arg
         (read-file-name "Select file to refile to: "
                         default-directory
                         (buffer-file-name (buffer-base-buffer))
                         t nil
                         (lambda (f) (string-match-p "\\.org$" f)))))
  (+org/refile-to-current-file arg file))

;;;###autoload
(defun +org/refile-to-other-window (arg)
  "Refile current heading to an org buffer visible in another window.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (win (delq (selected-window) (window-list)))
      (with-selected-window win
        (let ((file (buffer-file-name (buffer-base-buffer))))
          (and (eq major-mode 'org-mode)
               file
               (cl-pushnew (cons file (cons :maxlevel 10))
                           org-refile-targets)))))
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-running-clock (arg)
  "Refile current heading to the currently clocked in task.
If prefix ARG, copy instead of move."
  (interactive "P")
  (unless (bound-and-true-p org-clock-current-task)
    (user-error "No active clock to refile to"))
  (let ((org-refile-keep arg))
    (org-refile 2)))

;;;###autoload
(defun +org/refile-to-last-location (arg)
  "Refile current heading to the last node you refiled to.
If prefix ARG, copy instead of move."
  (interactive "P")
  (or (assoc (plist-get org-bookmark-names-plist :last-refile)
             bookmark-alist)
      (user-error "No saved location to refile to"))
  (let ((org-refile-keep arg)
        (completing-read-function
         (lambda (_p _coll _pred _rm _ii _h default &rest _)
           default)))
    (org-refile)))

(provide 'org/+funcs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +funcs.el ends here
