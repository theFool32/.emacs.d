;;; init-org.el --- -*- lexical-binding: t -*-
;;


(require 'org/+tables)
(require 'org/+funcs)
(require 'init-custom)

;; OrgPac
(defvar +org-capture-file-gtd (concat +self/org-base-dir "gtd.org"))
(defvar +org-capture-file-idea (concat +self/org-base-dir "ideas.org"))
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-tickler (concat +self/org-base-dir "tickler.org"))
(defvar +org-capture-file-done (concat +self/org-base-dir "done.org"))
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-enable-auto-update-cookies-h)
         (org-mode . org-num-mode))
  :bind (:map org-mode-map
              ([tab] . org-cycle))
  :custom
  (org-element-use-cache nil)
  (org-src-preserve-indentation nil)
  (org-edit-src-content-indentation 0)
  (org-capture-bookmark nil) ;; TODO: no bookmark for refile
  (org-log-done 'time)
  (org-hide-emphasis-markers t)
  (org-deadline-warning-days 90)
  ;; (org-agenda-span 'day)
  (org-agenda-span 3)
  (org-agenda-start-with-log-mode t)
  (org-agenda-start-with-clockreport-mode nil)
  (org-agenda-start-on-weekday 1)
  (org-export-backends (quote (ascii html icalendar latex md odt)))
  (org-use-speed-commands t)
  (org-confirm-babel-evaluate 'nil)
  (org-directory (expand-file-name +self/org-base-dir))
  (org-ellipsis " ▼ ")
  (org-babel-python-command "python3")
  (org-bullets-bullet-list '("#"))
  (org-agenda-todo-ignore-scheduled 'future)

  :config
  (with-eval-after-load 'org (setq org-modules '()))
  (with-eval-after-load 'org-agenda
    (plist-put org-agenda-clockreport-parameter-plist :maxlevel 3))

  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (if (member (downcase lang) '("c" "cpp" "c++")) 'C (intern lang)))
           (backup-languages org-babel-load-languages)
           (pair (assoc sym backup-languages)))
      ;; - `(LANG . nil)' 是有意义的，不宜覆盖，详见 `org-babel-do-load-languages'。
      ;; - 只加载当前语言，「按需」到底。
      (unwind-protect
          (org-babel-do-load-languages 'org-babel-load-languages (list (cons sym t)))
        (setq-default org-babel-load-languages
                      (if pair
                          backup-languages
                        (append (list (cons sym t)) backup-languages))))))

  (advice-add 'org-babel-execute-src-block :before #'my/org-babel-execute-src-block )

  ;; (setq org-clock-persist t
  ;;       org-clock-persist-file (concat +self/org-base-dir "org-clock-save.el"))

  (set-face-attribute 'org-table nil :family "Sarasa Mono SC" :weight 'semi-bold)
  (with-eval-after-load 'org
    (org-clock-persistence-insinuate))

  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))

  (+org-init-appearance-h)
  (+org-init-agenda-h)
  (+org-init-capture-defaults-h)

  (setq org-log-into-drawer "LOGBOOK")
  (setq org-agenda-files (list +org-capture-file-gtd
                               +org-capture-file-tickler
                               +org-capture-file-done))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((+org-capture-file-gtd :level . 3)
                             (+org-capture-file-someday :level . 3)
                             (+org-capture-file-tickler :level . 3)))
  (setq org-log-into-drawer t)
  (setq org-tag-alist '(("lab" . ?L) ("academic" . ?a) ("life" . ?l) ("paper" . ?p) ("emacs" . ?e)
                        ("habit" . ?h)))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file +org-capture-file-gtd)
           "* ☞TODO %i%? \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
          ("w" "Tickler" entry
           (file +org-capture-file-tickler)
           "* ⚑WAITING %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("m" "Maybe" entry
           (file +org-capture-file-someday)
           "* %?\n%i" :prepend t :kill-buffer t)
          ("i" "Idea" entry
           (file+headline +org-capture-file-idea "Ideas")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ))
  (setq org-todo-keywords
        '((sequence
           "☞TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "⚔INPROCESS(i)"  ; A task that is in progress
           "⚑WAITING(w)"  ; Something is holding up this task; or it is paused
           "☟NEXT(n)"
           "|"
           "✔DONE(d)"  ; Task successfully completed
           "✘CANCELED(c)") ; Task was cancelled, aborted or is no longer applicable
          ) ; Task was completed

        org-todo-keyword-faces
        '(
          ("☞TODO" . (:foreground "#ff39a3" :weight bold))
          ("⚔INPROCESS"  . "orangered")
          ("✘CANCELED" . (:foreground "white" :background "#4d4d4d" :weight bold))
          ("⚑WAITING" . "pink")
          ("☟NEXT" . (:foreground "DeepSkyBlue"
                                  ;; :background "#7A586A"
                                  :weight bold))
          ("✔DONE" . "#008080")))


  ;; (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  (add-hook 'after-change-major-mode-hook
            (lambda () (if (equal show-paren-mode 't)
    		          (when (derived-mode-p 'org-mode)
    		            (show-paren-mode -1))
                    (show-paren-mode 1))))

  ;; https://emacs-china.org/t/topic/2119/15
  (defun my--diary-chinese-anniversary (lunar-month lunar-day &optional year mark)
    (require 'cal-china)
    (if year
        (let* ((d-date (diary-make-date lunar-month lunar-day year))
               (a-date (calendar-absolute-from-gregorian d-date))
               (c-date (calendar-chinese-from-absolute a-date))
               (cycle (car c-date))
               (yy (cadr c-date))
               (y (+ (* 100 cycle) yy)))
          (diary-chinese-anniversary lunar-month lunar-day y mark))
      (diary-chinese-anniversary lunar-month lunar-day year mark)))

  (defun my:org-agenda-time-grid-spacing ()
    "Set different line spacing w.r.t. time duration."
    (save-excursion
      (let* ((background (alist-get 'background-mode (frame-parameters)))
             (background-dark-p (string= background "dark"))
             (colors (if background-dark-p
                         (list "#aa557f" "DarkGreen" "DarkSlateGray" "DarkSlateBlue")
                       (list "#F6B1C3" "#FFFF9D" "#BEEB9F" "#ADD5F7")))
             pos
             duration)
        (nconc colors colors)
        (goto-char (point-min))
        (while (setq pos (next-single-property-change (point) 'duration))
          (goto-char pos)
          (when (and (not (equal pos (point-at-eol)))
                     (setq duration (org-get-at-bol 'duration)))
            (let ((line-height (if (< duration 30) 1.0 (+ 0.5 (/ duration 60))))
                  (ov (make-overlay (point-at-bol) (1+ (point-at-eol)))))
              (overlay-put ov 'face `(:background ,(car colors)
                                                  :foreground
                                                  ,(if background-dark-p "black" "white")))
              (setq colors (cdr colors))
              (overlay-put ov 'line-height line-height)
              (overlay-put ov 'line-spacing (1- line-height))))))))

  (add-hook 'org-agenda-finalize-hook #'my:org-agenda-time-grid-spacing)

  ;; binding
  (with-eval-after-load 'general
    (general-define-key :states '(normal insert)
                        :keymaps 'org-mode-map
                        "C-<return>" #'+org/insert-item-below
                        "C-S-<return>" 'org-insert-subheading
                        )
    (general-define-key :states '(normal)
                        :keymaps 'org-mode-map
                        "<return>" #'+org/dwim-at-point)

    (local-leader-def
      :keymaps 'org-mode-map
      "'" 'org-edit-special
      "*" 'org-ctrl-c-star
      "+" 'org-ctrl-c-minus
      "," 'org-switchb
      ;; "." 'org-goto

      "." 'consult-org-heading

      "A" 'org-archive-subtree
      "e" 'org-export-dispatch
      "f" 'org-footnote-new
      "h" 'org-toggle-heading
      "i" 'org-toggle-item
      "I" 'org-toggle-inline-images
      "n" 'org-store-link
      "o" 'org-set-property
      "q" 'org-set-tags-command
      "t" 'org-todo
      "T" 'org-todo-list
      "x" 'org-toggle-checkbox

      "a" '(:wk "attackments")
      "aa" 'org-attach
      "ad" 'org-attach-delete-one
      "aD" 'org-attach-delete-all
      "an" 'org-attach-new
      "ao" 'org-attach-open
      "aO" 'org-attach-open-in-emacs
      "ar" 'org-attach-reveal
      "aR" 'org-attach-reveal-in-emacs
      "au" 'org-attach-url
      "as" 'org-attach-set-directory
      "aS" 'org-attach-sync

      "b"  '(:wk "tables")
      "b-" 'org-table-insert-hline
      "ba" 'org-table-align
      "bb" 'org-table-blank-field
      "bc" 'org-table-create-or-convert-from-region
      "be" 'org-table-edit-field
      "bf" 'org-table-edit-formulas
      "bh" 'org-table-field-info
      "bs" 'org-table-sort-lines
      "br" 'org-table-recalculate
      "bR" 'org-table-recalculate-buffer-tables
      "bd" '(:wk "delete")
      "bdc" 'org-table-delete-column
      "bdr" 'org-table-kill-row
      "bi" '(:wk "insert")
      "bic" 'org-table-insert-column
      "bih" 'org-table-insert-hline
      "bir" 'org-table-insert-row
      "biH" 'org-table-hline-and-move
      "bt" '("toggle")
      "btf" 'org-table-toggle-formula-debugger
      "bto" 'org-table-toggle-coordinate-overlays

      "c" '(:wk "clock")
      "cc" 'org-clock-cancel
      "cd" 'org-clock-mark-default-task
      "ce" 'org-clock-modify-effort-estimate
      "cE" 'org-set-effort
      "cg" 'org-clock-goto
      "ci" 'org-clock-in
      "cI" 'org-clock-in-last
      "co" 'org-clock-out
      "cr" 'org-resolve-clocks
      "cR" 'org-clock-report
      "ct" 'org-evaluate-time-range
      "c=" 'org-clock-timestamps-up
      "c-" 'org-clock-timestamps-down

      "d" '(:wk "date/deadline")
      "dd" 'org-deadline
      "ds" 'org-schedule
      "dt" 'org-time-stamp
      "dT" 'org-time-stamp-inactive

      "D" '+my-org/mark-done

      "g" '(:wk "goto")
      "gc" 'org-clock-goto
      "gi" 'org-id-goto
      "gr" 'org-refile-goto-last-stored
      "gx" 'org-capture-goto-last-stored

      "l" '(:wk "links")
      "lc" 'org-cliplink
      "ld" '+org/remove-link
      "li" 'org-id-store-link
      "ll" 'org-insert-link
      "lL" 'org-insert-all-links
      "ls" 'org-store-link
      "lS" 'org-insert-last-stored-link
      "lt" 'org-toggle-link-display

      "P" '(:wk "publish")
      "Pa" 'org-publish-all
      "Pf" 'org-publish-current-file
      "Pp" 'org-publish
      "PP" 'org-publish-current-project
      "Ps" 'org-publish-sitemap

      "r" '(:wk "refile")
      "r." '+org/refile-to-current-file
      "rc" '+org/refile-to-running-clock
      "rl" '+org/refile-to-last-location
      "rf" '+org/refile-to-file
      "ro" '+org/refile-to-other-window
      "rr" 'org-refile

      "s" '(:wk "tree/subtree")
      "sa" 'org-toggle-archive-tag
      "sb" 'org-tree-to-indirect-buffer
      "sd" 'org-cut-subtree
      "sh" 'org-promote-subtree
      "sj" 'org-move-subtree-down
      "sk" 'org-move-subtree-up
      "sl" 'org-demote-subtree
      "sn" 'org-narrow-to-subtree
      "sr" 'org-refile
      "ss" 'org-sparse-tree
      "sA" 'org-archive-subtree
      "sN" 'widen
      "sS" 'org-sort

      "p" '(:wk "priority")
      "pd" 'org-priority-down
      "pp" 'org-priority
      "pu" 'org-priority-up

      "z" '(:wk "Download")
      "zc" 'org-download-clipboard
      "zd" 'org-download-delete
      "zi" 'org-download-image
      "zy" 'org-download-yank
      "ze" 'org-download-edit
      "zr" 'org-download-rename-at-point
      "zR" 'org-download-rename-last-file
      "zs" 'org-download-screenshot
      )

    (local-leader-def
      :keymaps 'org-agenda-mode-map
      "d" '(:wk "date/deadline")
      "dd" 'org-agenda-deadline
      "ds" 'org-agenda-schedule

      "c" '(:wk "clock")
      "cc" 'org-agenda-clock-cancel
      "cg" 'org-agenda-clock-goto
      "ci" 'org-agenda-clock-in
      "co" 'org-agenda-clock-out
      "cr" 'org-agenda-clockreport-mode
      "cs" 'org-agenda-show-clocking-issues

      "p" '(:wk "priority")
      "pd" 'org-agenda-priority-down
      "pp" 'org-agenda-priority
      "pu" 'org-agenda-priority-up

      "q" 'org-agenda-set-tags
      "r" 'org-agenda-refile
      "t" 'org-agenda-todo)
    )

  (defun +my-org/mark-done ()
    ""
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-back-to-heading)
      (when-let* ((close-time (org-entry-get (point) "CLOSED")) ;;  HACK: assume all DONE entries have CLOSED time
                  (close-time (org-time-string-to-time close-time))
                  (close-time (decode-time close-time))
                  (close-time (list (decoded-time-month close-time) (decoded-time-day close-time) (decoded-time-year close-time))))
        (org-cut-subtree)
        (with-current-buffer (find-file-noselect +org-capture-file-done)
          (org-datetree-find-iso-week-create close-time)
          (org-paste-subtree)
          (org-next-visible-heading 1)
          (when (and (null (nth 2 (org-heading-components)))
                     (= (nth 0 (org-heading-components)) 3))
            (org-cut-subtree))
          (save-buffer)
          (kill-buffer)))))


  (defun org-clock-merge (arg)
    "Merge the org CLOCK line with the next CLOCK line.

Requires that the time ranges in two lines overlap, i.e. the
start time of the first line and the second time of the second
line are identical.

If the testing fails, move the cursor one line down.

Universal argument ARG overrides the test and merges
the lines even if the ranges do not overlap."

    (interactive "P")
    (let* ((org-clock-regexp (concat "CLOCK: " org-ts-regexp3 "--" org-ts-regexp3))
           (first-line-start (line-beginning-position))
           (first-line (buffer-substring
                        (line-beginning-position) (line-end-position)))
           (first-line-t1 (if (string-match org-clock-regexp first-line)
                              (match-string 1 first-line)
                            (progn
                              (forward-line)
                              (user-error "The first line must have a valid CLOCK range"))))
           (first-line-t2 (match-string 9 first-line))
           (second-line (progn
                          (forward-line)
                          (buffer-substring
                           (line-beginning-position) (line-end-position))))
           (second-line-t1 (if (string-match org-clock-regexp second-line)
                               (match-string 1 second-line)
                             (user-error "The second line must have a valid CLOCK range")))
           (second-line-t2 (match-string 9 second-line)))

      ;; check if lines should be merged
      (unless (or arg (equal first-line-t1 second-line-t2))
        (user-error "Clock ranges not continuous. Override with universal argument"))

      ;; remove the two lines
      (delete-region first-line-start (line-end-position))
      ;; indent
      (org-cycle)
      ;; insert new time range
      (insert (concat "CLOCK: [" second-line-t1 "]--[" first-line-t2 "]"))
      ;; generate duration
      (org-ctrl-c-ctrl-c)))
  )
;; -OrgPac

(use-package org-download
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :after org
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))

(use-package org-contrib
  :after org)

(use-package org-habit
  :straight nil
  :after org)

(use-package calfw
  :commands (cfw:open-org-calendar)
  :straight (:host github :repo "zemaye/emacs-calfw")
  :bind (:map cfw:calendar-mode-map
              ("s" . cfw:show-details-command))
  :custom
  (cfw:display-calendar-holidays nil)
  :config
  (with-eval-after-load 'calfw
	(use-package calfw-ical
	  :straight (:host github :repo "zemaye/emacs-calfw"))
	(use-package calfw-org
	  :straight (:host github :repo "zemaye/emacs-calfw"))
	(use-package calfw-cal
	  :straight (:host github :repo "zemaye/emacs-calfw"))))

(use-package org-modern
  :straight (:host github :repo "minad/org-modern")
  :defer t
  :after org
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
  (setq
   org-modern-timestamp nil
   org-modern-table nil
   org-modern-block nil
   org-modern-keyword nil
   org-modern-todo nil ;;  TODO: no better way to define fine faces
   org-agenda-block-separator ?─
   org-agenda-time-grid
   '((daily today require-timed)
     (800 1000 1200 1400 1600 1800 2000)
     " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
   org-agenda-current-time-string
   "⭠ now ─────────────────────────────────────────────────")
  )

(defun +my/open-org-agenda ()
  "open org agenda in left window"
  (interactive)
  (org-agenda nil "n")
  (evil-window-move-far-left))

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
