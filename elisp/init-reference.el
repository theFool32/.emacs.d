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
;;     Update #: 95
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

(use-package ebib
  :straight (:host github :repo "theFool32/ebib" :depth 1)
  :ensure nil
  :custom
  (ebib-preload-bib-files '("~/Ref/ref.bib"))
  (ebib-file-search-dirs '("~/Ref/pdfs/"))
  (ebib-notes-directory "~/Ref/notes/")
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
            )
    )
  :bind
  (
   :map ebib-index-mode-map
   ("/" . ebib-jump-to-entry)
   ("?" . ebib-search)
   :map ebib-multiline-mode-map
   ("C-c C-c" . 'ebib-quit-multiline-buffer-and-save)
   ("C-c C-q" . 'ebib-cancel-multiline-buffer)
   ("C-c C-s" . 'ebib-save-from-multiline-buffer)
   :map bibtex-mode-map
   ("C-c C-i" . 'insert-to-bib)
   )
  )


(provide 'init-reference)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-reference.el ends here
