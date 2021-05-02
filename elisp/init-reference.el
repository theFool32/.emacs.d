;;; init-reference.el ---

;;
;; Filename: init-reference.el
;; Description:  for bib reference
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Sat Apr 11 00:07:04 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 237
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

;; FIXME: too small for splitting (have no idea now)
(use-package ebib
  :straight (:host github :repo "theFool32/ebib" :depth 1)
  ;; :straight (ebib :local-repo "/Users/lijie/dev/ebib")
  :custom
  (ebib-preload-bib-files (list (concat ebib-base-dir "ref.bib")))
  (ebib-notes-directory (concat ebib-base-dir "notes/"))
  (ebib-keywords-file (concat ebib-base-dir "ebib-keywords.txt"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
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
   ("s"   . 'ebib-save-all-databases)
   ("C-k" . 'ebib-keywords-add)
   :map ebib-multiline-mode-map
   ("C-c C-c" . 'ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . 'ebib-cancel-multiline-buffer)
   ("C-c C-s" . 'ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . 'insert-to-bib)
   )
  :init
  (defun ebib-swiper ()
    (interactive)
    (progn (swiper) (ebib--update-entry-buffer)))
  (defun insert-to-bib ()
    (interactive)
    (progn
      (call-interactively 'ebib-jump-to-entry)
      (call-interactively 'ebib-copy-entry)
      (call-interactively 'popup-kill-ring)
      )
    )
  (if *sys/mac*
      (setq ebib-file-associations '(("pdf" . "open"))
            ebib-index-window-size 30
            ))

  (setq download-dir (concat ebib-base-dir "/pdfs"))

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
          ;; :command (list "ref_down.py" (shell-quote-argument url) download-dir) ;; FIXME: http\\:
          :command (list "ref_down.py" pdf-url download-dir)
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
  :config
  (org-link-set-parameters "ebib" :follow #'org-ebib-open :store #'org-ebib-store-link)
  )

(provide 'init-reference)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reference.el ends here
