;;; init-ebib.el ---

;;
;; Filename: init-ebib.el
;; Description:  for bib reference
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Sat Apr 11 00:07:04 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 289
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
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

(require 'init-func)

(use-package ebib
  :commands ebib
  :straight (:host github :repo "theFool32/ebib" :depth 1)
  ;; :straight (ebib :local-repo "/Users/lijie/dev/ebib")
  :custom
  (ebib-citation-description-function 'ebib-title-description)
  (ebib-preload-bib-files (list (concat +self/ebib-base-dir "ref.bib")))
  (ebib-file-search-dirs (list (concat +self/ebib-base-dir "pdfs/")))
  (ebib-notes-directory (concat +self/ebib-base-dir "notes/"))
  (ebib-keywords (concat +self/ebib-base-dir "ebib-keywords.txt"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-save-on-exit 'always)
  (ebib-filters-default-file (concat +self/ebib-base-dir "ebib-filters"))
  ;; (ebib-index-window-size 30)
  (ebib-timestamp-format "%Y-%m-%d,%T")
  (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-extra-fields
   '((BibTeX "keywords" "abstract" "timestamp" "read"
             "file"  "url" "crossref" "annote" "doi")))
  (ebib-hidden-fields
   '("volume" "number" "series" "editor" "pages" "address" "month" "organization" "publisher" "crossref"))
  (ebib-index-columns '(
                        ("Title" 70 t)
                        ("Author/Editor" 33 t)
                        ("Year" 4 t)
                        ("timestamp" 19 t)
                        ("read" 1 t)
                        ("readtime" 19 t)
                        )
                      )
  :bind
  (
   :map ebib-index-mode-map
   ("?" . ebib-jump-to-entry)
   ("/" . ebib-swiper)
   ;; ("?" . ebib-search)
   ("D" . ebib-delete-entry-with-file)
   ("s"   . ebib-save-all-databases)
   ("B"   . ebib-import-ref)
   :map ebib-multiline-mode-map
   ("C-c C-c" . ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . ebib-cancel-multiline-buffer)
   ("C-c C-s" . ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . insert-to-bib)
   )
  :init
  (defun ebib-swiper ()
    (interactive)
    (progn (consult-line) (ebib--update-entry-buffer))
    )

  (setq my-needed-fields '("author" "booktitle" "year" "title" "journal"))
  (defun my-ebib--format-entry (key db &optional timestamp sort)
    "Write entry KEY in DB into the current buffer in BibTeX format.
If TIMESTAMP is non-nil and `ebib-use-timestamp' is set, a
timestamp is added to the entry, possibly overwriting an existing
timestamp.  If SORT is non-nil, the fields are sorted before
formatting the entry."
    (let* ((entry (copy-alist (ebib-db-get-entry key db 'noerror)))
           (type (cdr (assoc "=type=" entry))))
      (when entry
        (if (and timestamp ebib-use-timestamp)
            (setcdr (assoc-string "timestamp" entry 'case-fold) (format-time-string (concat "{" ebib-timestamp-format "}"))))
        (setq entry (seq-filter (lambda (field)
                                  (and (cdr field) ; Remove fields with value nil. See `ebib-set-field-value'.
                                       (not (ebib--special-field-p (car field))))
                                  (member (car field) my-needed-fields))
                                entry))
        (setq entry (if sort
                        (cl-sort entry #'string< :key #'car)
                      ;; When reading, fields are stored with `push', so if we don't
                      ;; sort, we need to reverse them to get the original order
                      ;; back.  See github issues #42, #55, #62.
                      (reverse entry)))
        (insert (format "@%s{%s,\n" type key))
        (insert (mapconcat (lambda (field)
                             (format "\t%s = %s" (car field) (cdr field)))
                           entry
                           ",\n"))
        (insert "\n}\n\n"))))
  (defun my-ebib-copy-entry ()
    "Copy the current entry.
The entry is copied to the kill ring."
    (interactive)
    (ebib--execute-when
      (entries
       (let ((key (ebib--get-key-at-point)))
         (with-temp-buffer
           (my-ebib--format-entry key ebib--cur-db)
           (kill-new (buffer-substring-no-properties (point-min) (point-max))))
         (message (format "Entry `%s' copied to kill ring.  Use `y' to yank (or `C-y' outside Ebib)." key))))
      (default
        (beep))))
  (defun insert-to-bib ()
    (interactive)
    (call-interactively 'ebib-jump-to-entry)
    (call-interactively 'my-ebib-copy-entry)
    (yank))

  (if *sys/mac*
      (setq ebib-file-associations '(("pdf" . "open"))
            ebib-index-window-size 30
            ))

  (setq download-dir (concat +self/ebib-base-dir "/pdfs"))

  (defun ebib-import-ref (url)
    (interactive "sUrl:")
    (setq buffername (concat "*ref-" (get-random-uuid) "*"))
    (setq pdf-url url)
    (run-with-idle-timer
     0.1
     nil
     (lambda ()
       (let ((tempbuff (get-buffer-create buffername)))
         (make-process
          :name ""
          :buffer tempbuff
          ;; :command (list "ref_down.py" (shell-quote-argument url) download-dir)
          :command (list "ref_down.py" pdf-url download-dir)
          :sentinel (lambda (process event)
                      ;; Render result to content buffer when subprocess finish.
                      (when (string= (substring event 0 -1) "finished")
                        (let ((buffer (process-buffer process)))
                          ;; Do nothing if process buffer has killed.
                          (when (get-buffer buffer)
                            (with-current-buffer buffer
                              (ebib-import-entries)
                              (kill-buffer buffer)
                              (ebib--update-buffers)))))))))))
  :config
  (use-package org-ebib
    :straight (:host github :repo "theFool32/ebib" :depth 1)
    :config
    (local-leader-def
      :keymaps 'org-mode-map
      "lb" 'org-ebib-insert-link
      )
    ))

(provide 'init-ebib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ebib ends here
