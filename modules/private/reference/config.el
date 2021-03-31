;;; private/reference/config.el -*- lexical-binding: t; -*-

(use-package! ebib
  ;; :load-path "/Users/lijie/dev/ebib"
  :defer t
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
      (call-interactively '+default/yank-pop) ;; FIXME: new function, not focus on the latest
      )
    )
  :config
  (if IS-MAC
      (setq ebib-file-associations '(("pdf" . "open"))
            ebib-index-window-size 30
            ))
  (map! :map ebib-index-mode-map
        ;; :nmv "/" #'ebib-jump-to-entry)
        :nmv "/" (λ! (progn
                       (swiper)
                       (ebib--update-entry-buffer))))
  :bind
  (
   :map ebib-index-mode-map
   ;; ("?"   . 'ebib-search)
   ("?"   . 'ebib-jump-to-entry)
   ("D"   . 'ebib-delete-entry-with-file)
   ("s"   . 'ebib-save-all-databases)
   ("C-k" . 'ebib-keywords-add)
   :map ebib-multiline-mode-map
   ("C-c C-c" . 'ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . 'ebib-cancel-multiline-buffer)
   ("C-c C-s" . 'ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . 'insert-to-bib) ;; C-c <Tab>
   )
  )

(setq download-dir "/Users/lijie/Dropbox/Ref/pdfs")    ; change dir as desired

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
  (interactive "sUrl:")
  (setq buffername (concat "*ref-" (get-random-uuid) "*"))
  (run-with-idle-timer
   0.1
   nil
   (lambda ()
     (let ((tempbuff (get-buffer-create buffername)))
       (make-process
        :name ""
        :buffer tempbuff
        ;; :command (list "ref_down.py" (shell-quote-argument url) download-dir) ;; FIXME: http\\:
        :command (list "ref_down.py" url download-dir)
        :sentinel (lambda (process event)
                    ;; Render result to content buffer when subprocess finish.
                    (when (string= (substring event 0 -1) "finished")
                      (let ((buffer (process-buffer process)))
                        ;; Do nothing if process buffer has killed.
                        (when (get-buffer buffer)
                          (with-current-buffer buffer
                            (ebib-import)
                            (kill-buffer buffer)
                            (ebib--update-buffers)))))))))))
