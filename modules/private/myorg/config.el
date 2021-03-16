(defvar org-self-dir "~/Dropbox/org-notes/")
(defvar +org-capture-file-gtd (concat org-self-dir "gtd.org"))
(defvar +org-capture-file-idea (concat org-self-dir "ideas.org"))
(defvar +org-capture-file-note (concat org-self-dir "notes.org"))
(defvar +org-capture-file-inbox (concat org-self-dir "inbox.org"))
(defvar +org-capture-file-someday (concat org-self-dir "someday.org"))
(defvar +org-capture-file-tickler (concat org-self-dir "tickler.org"))

(defun archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))

(map!
 (:after evil-org
  :map evil-org-mode-map
  :localleader
  :desc "archive tasks" "D" #'archive-done-tasks))

(after! org
  ;; (setq org-agenda-files '(+org-capture-file-inbox
  ;;                          +org-capture-file-gtd
  ;;                          +org-capture-file-tickler))
  (setq org-agenda-files '("~/Dropbox/org-notes/inbox.org"
                           "~/Dropbox/org-notes/gtd.org"
                           "~/Dropbox/org-notes/tickler.org"))
  (setq org-refile-targets '(("~/Dropbox/org-notes/gtd.org" :level . 1)
                           ("~/Dropbox/org-notes/someday.org" :level . 1)
                           ("~/Dropbox/org-notes/tickler.org" :level . 2)))
  (setq org-log-into-drawer t)
  (setq org-capture-templates
        '(("t" "Todo [inbox]" entry
           (file+headline +org-capture-file-inbox "Tasks")
           "* TODO %?\n%i" :prepend t :kill-buffer t)
          ("w" "Waiting for" entry
           (file+headline +org-capture-file-tickler "Tickler")
           "* %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file-idea "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          )
        org-todo-keywords
        '((sequence "TODO(t!)" "|" "DONE(d!)" "CANCELLED(c!)"))
        ;; org-agenda-window-setup 'other-window
        ))

;; (use-package org-latex-instant-preview
;;   ;; npm install mathjax-node-cli
;;   :straight (:host github :repo "yangsheng6810/org-latex-instant-preview" :depth 1)
;;   :hook (org-mode . org-latex-instant-preview-mode)
;;   :init
;;   (setq org-latex-instant-preview-tex2svg-bin
;;         ;; location of tex2svg executable
;;         "~/node_modules/mathjax-node-cli/bin/tex2svg"))
