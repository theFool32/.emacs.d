;;; init-latex.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-latex.el
;; Description: Initialize AUCTex
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Wed Sep  4 16:35:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri Mar  4 20:06:15 2022 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d auctex
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes AUCTex
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

(eval-when-compile
  (require 'init-const)
  (require 'init-global-config)
  (require 'init-bindings)
  (require 'init-func))

;; Fontification taken from https://tex.stackexchange.com/a/86119/81279
(setq font-latex-match-reference-keywords
      '(;; biblatex
        ("printbibliography" "[{")
        ("addbibresource" "[{")
        ;; Standard commands
        ("cite" "[{")
        ("citep" "[{")
        ("citet" "[{")
        ("Cite" "[{")
        ("parencite" "[{")
        ("Parencite" "[{")
        ("footcite" "[{")
        ("footcitetext" "[{")
        ;; Style-specific commands
        ("textcite" "[{")
        ("Textcite" "[{")
        ("smartcite" "[{")
        ("Smartcite" "[{")
        ("cite*" "[{")
        ("parencite*" "[{")
        ("supercite" "[{")
        ;; Qualified citation lists
        ("cites" "[{")
        ("Cites" "[{")
        ("parencites" "[{")
        ("Parencites" "[{")
        ("footcites" "[{")
        ("footcitetexts" "[{")
        ("smartcites" "[{")
        ("Smartcites" "[{")
        ("textcites" "[{")
        ("Textcites" "[{")
        ("supercites" "[{")
        ;; Style-independent commands
        ("autocite" "[{")
        ("Autocite" "[{")
        ("autocite*" "[{")
        ("Autocite*" "[{")
        ("autocites" "[{")
        ("Autocites" "[{")
        ;; Text commands
        ("citeauthor" "[{")
        ("Citeauthor" "[{")
        ("citetitle" "[{")
        ("citetitle*" "[{")
        ("citeyear" "[{")
        ("citedate" "[{")
        ("citeurl" "[{")
        ;; Special commands
        ("fullcite" "[{")
        ;; cleveref
        ("cref" "{")
        ("Cref" "{")
        ("cpageref" "{")
        ("Cpageref" "{")
        ("cpagerefrange" "{")
        ("Cpagerefrange" "{")
        ("crefrange" "{")
        ("Crefrange" "{")
        ("labelcref" "{")))

(setq font-latex-match-textual-keywords
      '(;; biblatex brackets
        ("parentext" "{")
        ("brackettext" "{")
        ("hybridblockquote" "[{")
        ;; Auxiliary Commands
        ("textelp" "{")
        ("textelp*" "{")
        ("textins" "{")
        ("textins*" "{")
        ;; subcaption
        ("subcaption" "[{")))

(setq font-latex-match-variable-keywords
      '(;; amsmath
        ("numberwithin" "{")
        ;; enumitem
        ("setlist" "[{")
        ("setlist*" "[{")
        ("newlist" "{")
        ("renewlist" "{")
        ("setlistdepth" "{")
        ("restartlist" "{")
        ("crefname" "{")))



(use-package reftex
  :after tex
  :hook (LaTeX-mode . reftex-mode)
  :custom
  (reftex-plug-into-AUCTeX t)
  (reftex-toc-split-windows-fraction 0.3)
  :config

  (defvar +my/reftex-citation-completion-table nil)

  (defun +my/reftex-find-citation (prefix)
    (reftex-access-scan-info)
    (setq +my/reftex-citation-completion-table
          (cl-letf (((symbol-function 'reftex--query-search-regexps)
                     (lambda (_) (if (string= prefix "")
                                (list ".+")
                              (list (regexp-quote prefix))))))
            (let* ((bibtype (reftex-bib-or-thebib))
                   (candidates
                    (cond
                     ((eq 'thebib bibtype)
                      (reftex-extract-bib-entries-from-thebibliography
                       (reftex-uniquify
                        (mapcar 'cdr
                                (reftex-all-assq
                                 'thebib (symbol-value reftex-docstruct-symbol))))))
                     ((eq 'bib bibtype)
                      (reftex-extract-bib-entries (reftex-get-bibfile-list)))
                     (reftex-default-bibliography
                      (reftex-extract-bib-entries (reftex-default-bibliography))))))
              (cl-loop
               for entry in candidates
               collect
               (let ((key (substring-no-properties (car entry)))
                     (annotate (reftex-format-citation entry "%t")))
                 (cons
                  (format "%s -> %s" key annotate)
                  key)))))
          )
    +my/reftex-citation-completion-table)

  (defcustom company-reftex-citations-regexp
    (rx "\\"
        ;; List taken from `reftex-cite-format-builtin'
        (or "autocite"
            "autocite*"
            "bibentry"
            "cite"
            "cite*"
            "citeA"
            "citeaffixed"
            "citeasnoun"
            "citeauthor"
            "citeauthor*"
            "citeauthory"
            "citefield"
            "citeN"
            "citename"
            "cites"
            "citet"
            "citet*"
            "citetitle"
            "citetitle*"
            "citep"
            "citeyear"
            "citeyear*"
            "footcite"
            "footfullcite"
            "fullcite"
            "fullocite"
            "nocite"
            "ocite"
            "ocites"
            "parencite"
            "parencite*"
            "possessivecite"
            "shortciteA"
            "shortciteN"
            "smartcite"
            "textcite"
            "textcite*"
            "ycite"
            "ycites")
        (* (not (any "[{")))
        (* (seq "[" (* (not (any "]"))) "]"))
        "{"
        (* (seq (* (not (any "},"))) ","))
        (group (* (not (any "},")))))
    "Regular expression to use when lookng for the citation prefix.
Group number 1 should be the prefix itself."
    :type 'string
    :group 'company-reftex)
  (defun company-reftex-prefix (regexp)
    "Return the prefix for matching given REGEXP."
    (and (derived-mode-p 'latex-mode)
         reftex-mode
         (when (looking-back regexp nil)
           (match-string-no-properties 1))))

  (defun +my/reftex-citation-completion ()
    (when (company-reftex-prefix company-reftex-citations-regexp)
      (let ((bounds (bounds-of-thing-at-point 'symbol)))
        `(,(car bounds) ,(cdr bounds)
          ,(cape--table-with-properties
            (cape--cached-table (car bounds) (cdr bounds) #'+my/reftex-find-citation 'substring)
            :category 'cape-table)
          :exclusive 'no
          ;; :annotation-function (lambda (k) (cdr (assoc k table)))
          :exit-function (lambda (str sta)
                           (backward-delete-char (length str))
                           (insert (cdr (assoc str +my/reftex-citation-completion-table)))
                           )))))

  )

(use-package bibtex
  :after (org tex)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  )


(use-package latex
  :straight auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
  (TeX-insert-braces nil)
  (TeX-parse-self t) ; parse on load
  (TeX-auto-save t)  ; parse on save
  ;; use hidden dirs for auctex files
  (TeX-auto-local ".auctex-auto")
  (TeX-style-local ".auctex-style")
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctex)
  ;; don't start the emacs server when correlating sources
  (TeX-source-correlate-start-server nil)
  ;; automatically insert braces after sub/superscript in math mode
  (TeX-electric-sub-and-superscript t)
  (TeX-master nil)

  (LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
   '(LaTeX-section-heading
     LaTeX-section-title
     LaTeX-section-toc
     LaTeX-section-section
     LaTeX-section-label))
  (LaTeX-fill-break-at-separators nil)

  :config
  (general-define-key :states 'normal :keymaps 'LaTeX-mode-map (kbd "zo") #'TeX-fold-clearout-item)
  (general-define-key :states 'normal :keymaps 'LaTeX-mode-map (kbd "zc") #'TeX-fold-env)
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf -pvc -view=none %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)
                               (setq TeX-command-list (delete-dups TeX-command-list))
                               ))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (progn
    (assq-delete-all 'output-pdf TeX-view-program-selection)
    (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Viewer")))
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Fold TeX macros
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)

  (defun my-TeX-compile ()
    (interactive)
    (TeX-master-file nil nil t)
    (TeX-command-sequence '("BibTeX" "LaTeX" "LaTeX") t))
  (local-leader-def
    :keymaps 'LaTeX-mode-map
    "m" '(TeX-master-file-ask :wk "Master file")
    "c" '(my-TeX-compile :wk "Compile")
    "v" '(TeX-view :wk "View"))
  )

(use-package cdlatex
  :after tex
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  :config
  (add-to-list 'cdlatex-math-modify-alist-default '(?b "\\bm" nil t t nil))
  )
(use-package asymbol
  :straight (:host github :repo "dwuggh/asymbol" :depth 1)
  :hook (LaTeX-mode . asymbol-mode)
  :init
  ;; a little customization
  (setq asymbol-help-symbol-linewidth 110
	    asymbol-help-tag-linewidth 110)

  :config
  (add-hook 'org-cdlatex-mode-hook
            (lambda () (interactive)
              (define-key org-cdlatex-mode-map "`" 'asymbol-insert-text-or-symbol)))
  )
;; -AUCTeXPac

(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
