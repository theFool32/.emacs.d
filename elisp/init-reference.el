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
;;     Update #: 136
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
  ;; :load-path "~/proj/ebib"
  :straight (:host github :repo "theFool32/ebib" :depth 1)
  :ensure nil
  :custom
  (ebib-preload-bib-files '("~/Dropbox/Ref/ref.bib"))
  (ebib-file-search-dirs '("~/Dropbox/Ref/pdfs/"))
  (ebib-notes-directory "~/Dropbox/Ref/notes/")
  (ebib-keywords-file "~/Dropbox/Ref/ebib-keywords.txt")
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-file-save-on-exit 'always)
  (ebib-index-window-size 30)
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
  (if *sys/linux*
      (setq ebib-file-associations '(("pdf" . "/mnt/c/Program Files/SumatraPDF/SumatraPDF.exe"))
            ebib-browser-command "/mnt/c/Windows/System32/cmd.exe /c start" ;; TODO: not work
            ))
  (if *sys/mac*
      (setq ebib-file-associations '(("pdf" . "open"))
            ebib-index-window-size 60
            ))
  :bind
  (
   :map ebib-index-mode-map
   ("/" . ebib-jump-to-entry)
   ("?" . ebib-search)
   ("D" . ebib-delete-entry-with-file)
   :map ebib-multiline-mode-map
   ("C-c C-c" . 'ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . 'ebib-cancel-multiline-buffer)
   ("C-c C-s" . 'ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . 'insert-to-bib)
   )
  )

(setq arxiv-dir "~/Dropbox/Ref/pdfs")    ; change dir as desired
(defun ebib-import-ref (url)
  (interactive "sUrl:")
  ;; TODO: different buffer name
  (setq buffername (concat "*ref-" (get-random-uuid) "*"))
  (let ((tempbuff (get-buffer-create buffername)))
    (call-process-shell-command (concat "ref_down.py " url " " arxiv-dir) nil tempbuff nil)
    ;; TODO: auto kill buffer
    (with-current-buffer tempbuff
      (ebib-import)
      (kill-buffer tempbuff))))

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

(provide 'init-reference)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reference.el ends here
