;;; private/reference/config.el -*- lexical-binding: t; -*-

(use-package! ebib
  :commands ebib
  :custom
  (ebib-preload-bib-files '("~/Dropbox/Ref/ref.bib"))
  (ebib-file-search-dirs '("~/Dropbox/Ref/pdfs/"))
  (ebib-notes-directory "~/Dropbox/Ref/notes/")
  (ebib-keywords-file "~/Dropbox/Ref/ebib-keywords.txt")
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-timestamp-format "%Y-%m-%d,%T")
  (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-extra-fields
   '((BibTeX "keywords" "abstract" "timestamp" "read"
             "file"  "url" "crossref" "annote" "doi")))
  (ebib-hidden-fields
   '("volume" "number" "series" "editor" "pages" "address" "month" "organization" "publisher" "crossref"))
  (ebib-index-columns '(
                        ("Title" 60 t)
                        ("Author/Editor" 40 t)
                        ("Year" 6 t)
                        ("timestamp" 15 t)
                        ("read" 1 t)
                        ("readtime" 15 t)
                        )
                      )
  :init
  (defun insert-to-bib ()
    (interactive)
    (progn
      (call-interactively 'ebib-jump-to-entry)
      (call-interactively 'ebib-copy-entry)
      (call-interactively 'popup-kill-ring)
      )
    )
  (if IS-MAC
      (setq ebib-file-associations '(("pdf" . "open"))
            ebib-index-window-size 30
            ))
  :bind
  (
   :map ebib-index-mode-map
   ("/" . 'ebib-jump-to-entry)
   ("?" . 'ebib-search)
   ("D" . 'ebib-delete-entry-with-file)
   :map ebib-multiline-mode-map
   ("C-c C-c" . 'ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . 'ebib-cancel-multiline-buffer)
   ("C-c C-s" . 'ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . 'insert-to-bib)
   )
  )

(setq arxiv-dir "~/Dropbox/Ref/pdfs")    ; change dir as desired

(random t)
(defun get-random-uuid ()
  "Insert a random UUID.
Example of a UUID: 1df63142-a513-c850-31a3-535fc3520c3d
WARNING: this is a simple implementation. The chance of generating the same UUID is much higher than a robust algorithm.."
  (interactive)

  (format "%04x%04x-%04x-%04x-%04x-%06x%06x"
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 4))
          (random (expt 16 6))
          (random (expt 16 6)) ) )

(defun ebib-import-ref (url)
  ;; TODO: async-start
  (interactive "sUrl:")
  (setq buffername (concat "*ref-" (get-random-uuid) "*"))
  (let ((tempbuff (get-buffer-create buffername)))
    (call-process-shell-command (concat "ref_down.py " (shell-quote-argument url) " " arxiv-dir) nil tempbuff nil)
    (with-current-buffer tempbuff
      (ebib-import)
      (kill-buffer tempbuff)
      (ebib--update-buffers))))

(defun ebib-import-arxiv (arxiv-url)
  (interactive "sUrl:")

  (let ((tempbuff (get-buffer-create "*arxiv*"))
        (arxiv-id (car (cdr (split-string arxiv-url "abs/"))))
        (arxiv-pdf-url (concat (replace-regexp-in-string "abs" "pdf" arxiv-url) ".pdf")))

    (call-process-shell-command "arxiv2bib" nil tempbuff nil arxiv-id)

    ;; (call-process-shell-command "links" nil nil nil
    ;;                             "-source" arxiv-pdf-url "> " (concat arxiv-dir arxiv-id ".pdf"))

    (with-current-buffer tempbuff
      (ebib-import))))
