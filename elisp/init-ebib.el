;;; init-ebib.el --- -*- lexical-binding: t -*-

(require 'init-func)

(use-package ebib
  :commands ebib
  :straight (ebib :includes (org-ebib) :host github :repo "theFool32/ebib")
  :hook (ebib-index-mode . hl-line-mode)
  :custom
  (ebib-file-associations '(("pdf" . "open")))
  (ebib-index-window-size 30)
  (ebib-citation-description-function 'ebib-title-description)
  (ebib-preload-bib-files (list (concat +self/ebib-base-dir "ref.bib")))
  (ebib-file-search-dirs (list (concat +self/ebib-base-dir "pdfs/")))
  (ebib-notes-directory (concat +self/ebib-base-dir "notes/"))
  (ebib-keywords (concat +self/ebib-base-dir "ebib-keywords.txt"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-save-on-exit 'always)
  (ebib-filters-default-file (concat +self/ebib-base-dir "ebib-filters"))
  (ebib-timestamp-format "%Y-%m-%d,%T")
  (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-extra-fields
   '((BibTeX "keywords" "abstract" "timestamp" "read"
             "file"  "url" "crossref" "annote" "doi")))
  (ebib-hidden-fields
   '("volume" "number" "series" "editor" "pages" "address" "month" "organization" "publisher" "crossref"))
  (ebib-index-columns '(("Title" 70 t)
                        ("Author/Editor" 33 t)
                        ("Year" 4 t)
                        ("timestamp" 19 t)
                        ("read" 1 t)
                        ("readtime" 19 t)))
  :bind
  (:map ebib-index-mode-map
        ("/" . ebib-jump-to-entry)
        ("?" . ebib-swiper)
        ;; ("?" . ebib-search)
        ("D" . ebib-delete-entry-with-file)
        ("s"   . ebib-save-all-databases)
        ("S"   . +my/search-pdf)
        ("B"   . ebib-import-ref)
        :map ebib-multiline-mode-map
        ("C-c C-c" . ebib-quit-multiline-buffer-and-save)
        ("C-c C-q" . ebib-cancel-multiline-buffer)
        ("C-c C-s" . ebib-save-from-multiline-buffer)
        :map bibtex-mode-map
        ("C-c C-i" . insert-to-bib))
  :init
  (defun ebib-swiper ()
    (interactive)
    (progn (consult-line) (ebib--update-entry-buffer)))

  ;;  TODO: search only the current pdf
  ;;  TODO: split the files and the contents
  (defun +my/search-pdf ()
    (interactive)
    (consult-ripgrep (concat +self/ebib-base-dir "/pdfs") ""))

  (random t)
  (defun get-random-uuid ()
    "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d

WARNING: this is a simple implementation.  The chance of generating the same UUID is much higher than a robust algorithm.."
    (interactive)

    (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
            (random (expt 16 4))
            (random (expt 16 4))
            (random (expt 16 4))
            (random (expt 16 4))
            (random (expt 16 4))
            (random (expt 16 6))
            (random (expt 16 6))))


  (defun my-ebib--format-entry (key db &optional timestamp sort)
    "Write entry KEY in DB into the current buffer in BibTeX format.
If TIMESTAMP is non-nil and `ebib-use-timestamp' is set, a
timestamp is added to the entry, possibly overwriting an existing
timestamp.  If SORT is non-nil, the fields are sorted before
formatting the entry."
    (let* ((entry (copy-alist (ebib-db-get-entry key db 'noerror)))
           (type (cdr (assoc "=type=" entry)))
           (my-needed-fields '("author" "booktitle" "year" "title" "journal")))
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

  (defun insert-to-bib ()
    (interactive)
    (call-interactively 'ebib-jump-to-entry)
    (ebib--execute-when
     (entries
      (let ((key (ebib--get-key-at-point)))
        (with-temp-buffer
          (my-ebib--format-entry key ebib--cur-db)
          (kill-new (buffer-substring-no-properties (point-min) (point-max))))
        (message (format "Entry `%s' copied to kill ring.  Use `y' to yank (or `C-y' outside Ebib)." key))))
     (default
      (beep)))
    (yank))

  (defun ebib-import-ref (url)
    (interactive "sUrl:")
    (let ((buffername (concat "*ref-" (get-random-uuid) "*"))
          (pdf-url url))
      (run-with-idle-timer
       0.1
       nil
       (lambda ()
         (let ((tempbuff (get-buffer-create buffername)))
           (make-process
            :name ""
            :buffer tempbuff
            :command (list "ref_down.py" pdf-url (concat +self/ebib-base-dir "/pdfs"))
            :sentinel (lambda (process event)
                        ;; Render result to content buffer when subprocess finish.
                        (when (string= (substring event 0 -1) "finished")
                          (let ((buffer (process-buffer process)))
                            ;; Do nothing if process buffer has killed.
                            (when (get-buffer buffer)
                              (with-current-buffer buffer
                                (ebib-import-entries)
                                (kill-buffer buffer)
                                (ebib--update-buffers))))))))))))
  :config
  (use-package org-ebib
    :config
    (local-leader-def
      :keymaps 'org-mode-map
      "lb" 'org-ebib-insert-link)))

(provide 'init-ebib)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ebib ends here
