(defvar org-self-dir "~/Dropbox/org-notes/")
(defvar +org-capture-file-gtd (concat org-self-dir "main.org"))
(defvar +org-capture-file-idea (concat org-self-dir "ideas.org"))
(defvar +org-capture-file-note (concat org-self-dir "notes.org"))
(after! org
  (setq org-capture-templates
        '(("t" "Next actions" entry
           (file+headline +org-capture-file-gtd "Next actions")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("w" "Waiting for" entry
           (file+headline +org-capture-file-gtd "Waiting for")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("s" "Some day/maybe" entry
           (file+headline +org-capture-file-gtd "Some day/maybe")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file-idea "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          )
        org-todo-keywords
        '((sequence "TODO" "IN-PROGRESS" "|" "DONE"))
        org-agenda-window-setup 'other-window))

;; (use-package org-latex-instant-preview
;;   ;; npm install mathjax-node-cli
;;   :straight (:host github :repo "yangsheng6810/org-latex-instant-preview" :depth 1)
;;   :hook (org-mode . org-latex-instant-preview-mode)
;;   :init
;;   (setq org-latex-instant-preview-tex2svg-bin
;;         ;; location of tex2svg executable
;;         "~/node_modules/mathjax-node-cli/bin/tex2svg"))
