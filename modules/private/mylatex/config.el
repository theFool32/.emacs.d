;;; private/mylatex/config.el -*- lexical-binding: t; -*-
(after! latex
  :init
  :config
  (setq TeX-PDF-mode t)

  (add-hook 'LaTeX-mode-hook (lambda ()
                               (add-to-list 'TeX-command-list
                                            '("latexmk" "latexmk -pdf -pvc -view=none %s" TeX-run-TeX nil t
                                              :help "Run latexmk on file"))))
  (add-hook 'TeX-mode-hook (lambda () (setq TeX-command-default "latexmk")))

  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))

  (eval-after-load 'tex
    '(progn
       (assq-delete-all 'output-pdf TeX-view-program-selection)
       (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Viewer"))))
  )
