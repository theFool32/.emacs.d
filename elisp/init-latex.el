;;; init-latex.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-latex.el
;; Description: Initialize AUCTex
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Wed Sep  4 16:35:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Tue Dec 24 12:00:08 2019 (-0500)
;;           By: Mingde (Matthew) Zeng
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

;;;###autoload
(defun +latex/LaTeX-indent-item ()
  "Provide proper indentation for LaTeX \"itemize\",\"enumerate\", and
\"description\" environments.
\"\\item\" is indented `LaTeX-indent-level' spaces relative to the the beginning
of the environment.
Continuation lines are indented either twice `LaTeX-indent-level', or
`LaTeX-indent-level-item-continuation' if the latter is bound."
  (save-match-data
    (let* ((offset LaTeX-indent-level)
           (contin (or (and (boundp '+latex-indent-level-item-continuation)
                            +latex-indent-level-item-continuation)
                       (* 4 offset)))
           (re-beg "\\\\begin{")
           (re-end "\\\\end{")
           (re-env "\\(itemize\\|\\enumerate\\|description\\)")
           (indent (save-excursion
                     (when (looking-at (concat re-beg re-env "}"))
                       (end-of-line))
                     (LaTeX-find-matching-begin)
                     (current-column))))
      (cond ((looking-at (concat re-beg re-env "}"))
             (or (save-excursion
                   (beginning-of-line)
                   (ignore-errors
                     (LaTeX-find-matching-begin)
                     (+ (current-column)
                        (if (looking-at (concat re-beg re-env "}"))
                            contin
                          offset))))
                 indent))
            ((looking-at (concat re-end re-env "}"))
             indent)
            ((looking-at "\\\\item")
             (+ offset indent))
            ((+ contin indent))))))

;;;###autoload
(defun +latex-symbols-company-backend (command &optional arg &rest _ignored)
  "A wrapper backend for `company-mode' that either uses
`company-math-symbols-unicode' or `company-math-symbols-latex'. If
`+latex-enable-unicode-math' is non-nil use the former, otherwise the latter."
  (if +latex-enable-unicode-math
      (company-math-symbols-unicode command arg)
    (company-math-symbols-latex command arg)))


(defvar +latex-indent-level-item-continuation 4
  "Custom indentation level for items in enumeration-type environments")

(defvar +latex-bibtex-file nil
  "File AUCTeX (specifically RefTeX) uses to search for citations.")

(defvar +latex-enable-unicode-math nil
  "If non-nil, use `company-math-symbols-unicode' backend in LaTeX-mode,
  enabling unicode symbols in math regions. This requires the unicode-math latex
  package to be installed.")

(defvar +latex-viewers '(skim evince sumatrapdf zathura okular pdf-tools)
  "A list of enabled latex viewers to use, in this order. If they don't exist,
  they will be ignored. Recognized viewers are skim, evince, sumatrapdf, zathura,
  okular and pdf-tools.
  If no viewers are found, `latex-preview-pane' is used.")

;;
(defvar +latex--company-backends nil)


;;
;; Packages

;; (when (stringp +latex-bibtex-file)
;;   (setq bibtex-completion-bibliography (list (expand-file-name +latex-bibtex-file))
;;         reftex-default-bibliography bibtex-completion-bibliography))



(use-package reftex
  :hook (LaTeX-mode . reftex-mode)
  ;; :bind
  ;; (:map reftex-mode-map
  ;;       ".;" 'reftex-toc)
  :custom
  (reftex-cite-format
   '((?a . "\\autocite[]{%l}")
     (?b . "\\blockcquote[]{%l}{}")
     (?c . "\\cite[]{%l}")
     (?f . "\\footcite[]{%l}")
     (?n . "\\nocite{%l}")
     (?p . "\\parencite[]{%l}")
     (?s . "\\smartcite[]{%l}")
     (?t . "\\textcite[]{%l}")))
  (reftex-plug-into-AUCTeX t)
  (reftex-toc-split-windows-fraction 0.3)
  :config
  ;; set up completion for citations and references
  (set-company-backend! 'reftex-mode 'company-reftex-labels 'company-reftex-citations)
  ;; (add-hook 'reftex-mode-hook (lambda() (setq company-backends '(company-reftex-labels company-reftex-citations))))
  ;; Get ReTeX working with biblatex
  ;; http://tex.stackexchange.com/questions/31966/setting-up-reftex-with-biblatex-citation-commands/31992#31992
  ;; (add-hook! 'reftex-toc-mode-hook
  ;;            (reftex-toc-rescan)
  ;;            (map! :map 'local
  ;;                  :e "j"   #'next-line
  ;;                  :e "k"   #'previous-line
  ;;                  :e "q"   #'kill-buffer-and-window
  ;;                  :e "ESC" #'kill-buffer-and-window))
  )

(use-package bibtex
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  )


(use-package latex
  :defer t
  :ensure auctex
  :mode ("\\.tex\\'" . LaTeX-mode)
  :custom
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
  (TeX-master t)

  (LaTeX-section-hook ; Add the toc entry to the sectioning hooks.
   '(LaTeX-section-heading
     LaTeX-section-title
     LaTeX-section-toc
     LaTeX-section-section
     LaTeX-section-label))
  (LaTeX-fill-break-at-separators nil)
  (LaTeX-item-indent 0)

  :config
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf -pvc -view=none %s" TeX-run-TeX nil t
                                  :help "Run latexmk on file")
                                TeX-command-list)
                               (setq TeX-command-list (delete-dups TeX-command-list))
                               ))
  ;; (add-hook 'TeX-mode-hook '(lambda () (setq TeX-command-default "latexmk")))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (with-eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Viewer"))))
  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")
  (add-hook 'TeX-mode-hook (lambda() (setq ispell-parser 'tex
                                      fill-nobreak-predicate (cons #'texmathp fill-nobreak-predicate))))
  ;; Enable word wrapping
  (add-hook 'TeX-mode-hook #'visual-line-mode)
  ;; Fold TeX macros
  (add-hook 'TeX-mode-hook #'TeX-fold-mode)
  ;; Enable rainbow mode after applying styles to the buffer
  (add-hook 'TeX-update-style-hook #'rainbow-delimiters-mode)
  ;; display output of latex commands in popup
  ;; (with-eval-after-load smartparens-latex
  ;;   (let ((modes '(tex-mode plain-tex-mode latex-mode LaTeX-mode)))
  ;;     ;; All these excess pairs dramatically slow down typing in latex buffers,
  ;;     ;; so we remove them. Let snippets do their job.
  ;;     (dolist (open '("\\left(" "\\left[" "\\left\\{" "\\left|"
  ;;                     "\\bigl(" "\\biggl(" "\\Bigl(" "\\Biggl(" "\\bigl["
  ;;                     "\\biggl[" "\\Bigl[" "\\Biggl[" "\\bigl\\{" "\\biggl\\{"
  ;;                     "\\Bigl\\{" "\\Biggl\\{"
  ;;                     "\\lfloor" "\\lceil" "\\langle"
  ;;                     "\\lVert" "\\lvert" "`"))
  ;;       (sp-local-pair modes open nil :actions :rem))
  ;;     ;; And tweak these so that users can decide whether they want use latex
  ;;     ;; quotes or not, via `+latex-enable-plain-double-quotes'
  ;;     (sp-local-pair modes "``" nil :unless '(:add sp-in-math-p))))

  (when +latex--company-backends
    (set-company-backend! 'latex-mode +latex--company-backends))
  ;; (add-hook 'LaTeX-mode-hook (lambda() (setq company-backends +latex--company-backends))))

  ;; Provide proper indentation for LaTeX "itemize","enumerate", and
  ;; "description" environments. See
  ;; http://emacs.stackexchange.com/questions/3083/how-to-indent-items-in-latex-auctex-itemize-environments
  (dolist (env '("itemize" "enumerate" "description"))
    (add-to-list 'LaTeX-indent-environment-list `(,env +latex/LaTeX-indent-item)))

  ;; Fix #1849: allow fill-paragraph in itemize/enumerate
  ;; (defadvice! +latex--re-indent-itemize-and-enumerate-a (orig-fn &rest args)
  ;;   :around #'LaTeX-fill-region-as-para-do
  ;;   (let ((LaTeX-indent-environment-list
  ;;          (append LaTeX-indent-environment-list
  ;;                  '(("itemize"   +latex/LaTeX-indent-item)
  ;;                    ("enumerate" +latex/LaTeX-indent-item)))))
  ;;     (apply orig-fn args)))
  ;; (defadvice! +latex--dont-indent-itemize-and-enumerate-a (orig-fn &rest args)
  ;;   :around #'LaTeX-fill-region-as-paragraph
  ;;   (let ((LaTeX-indent-environment-list LaTeX-indent-environment-list))
  ;;     (delq! "itemize" LaTeX-indent-environment-list 'assoc)
  ;;     (delq! "enumerate" LaTeX-indent-environment-list 'assoc)
  ;;     (apply orig-fn args)))
  )

;; (use-package auctex-latexmk
;;   :after latex
;;   :custom
;;   ;; Pass the -pdf flag when TeX-PDF-mode is active
;;   (auctex-latexmk-inherit-TeX-PDF-mode t)
;;   :init
;;   ;; Set LatexMk as the default
;;   ;; (add-hook LaTeX-mode-hook (lambda() (setq TeX-command-default "LatexMk")))
;;   :config
;;   ;; Add latexmk as a TeX target
;;   (auctex-latexmk-setup))

(use-package cdlatex
  :ensure t
  :hook (LaTeX-mode . cdlatex-mode)
  :config
  ;; Disabling keys that have overlapping functionality with other parts of Doom
  )
(use-package adaptive-wrap
  :ensure t
  :hook (LaTeX-mode . adaptive-wrap-prefix-mode)
  :init (setq-default adaptive-wrap-extra-indent 0))
(use-package company-auctex
  :defer t
  :init
  (add-to-list '+latex--company-backends #'company-auctex-environments nil #'eq)
  (add-to-list '+latex--company-backends #'company-auctex-macros nil #'eq)
  )
(use-package company-reftex)
(use-package company-math
  :defer t
  :init
  (add-to-list '+latex--company-backends #'+latex-symbols-company-backend nil #'eq))
(use-package ivy-bibtex)
;; -AUCTeXPac

;; OrgLatexPac
(use-package org-edit-latex
  :defer t
  :after org)
;; -OrgLatexPac

(provide 'init-latex)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-latex.el ends here
