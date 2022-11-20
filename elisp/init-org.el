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
(defvar +org-capture-file-done (concat +self/org-base-dir "done.org"))
(defvar +org-capture-file-goal (concat +self/org-base-dir "goals.org"))

(defvar +org-files (list +org-capture-file-gtd
                         +org-capture-file-someday
                         +org-capture-file-note
                         +org-capture-file-idea
                         +org-capture-file-goal))
(use-package org
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-enable-auto-update-cookies-h)
         (org-mode . org-num-mode))
  :bind (:map org-mode-map
              ([tab] . org-cycle))
  :custom
  (org-id-link-to-org-use-id t)
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


  (setq org-agenda-custom-commands
        '(("n" "Agenda"
           ((tags-todo
	         "ACT_WEEK"
	         ((org-agenda-files (list +org-capture-file-goal))
	          (org-agenda-overriding-header "Goals for this week")))
            (tags-todo
	         "ACT_MONTH"
	         ((org-agenda-files (list +org-capture-file-goal))
	          (org-agenda-overriding-header "Goals for this month")))
	        (agenda)
            (alltodo "")
            ))))

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
                               +org-capture-file-done))
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((+org-capture-file-gtd :level . 3)
                             (+org-capture-file-someday :level . 3)))
  (setq org-log-into-drawer t)
  (setq org-tag-alist '(("lab" . ?L) ("academic" . ?a) ("life" . ?l) ("emacs" . ?e)
                        ("habit" . ?h) ("ACT_MONTH" . ?m) ("ACT_WEEK" . ?w)))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file +org-capture-file-gtd)
           "* ☞TODO %i%? \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
          ("w" "Watting for" entry
           (file +org-capture-file-gtd)
           "* ⚑WAITING %?\n%i" :prepend t :kill-buffer t)
          ("n" "Note" entry
           (file+headline +org-capture-file-note "Notes")
           "* %u %?\n%i" :prepend t :kill-buffer t)
          ("s" "Someday maybe" entry
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
           "|"
           "✔DONE(d)"  ; Task successfully completed
           "✘CANCELED(c)") ; Task was cancelled, aborted or is no longer applicable
          ) ; Task was completed

        org-todo-keyword-faces
        '(
          ("☞TODO" . (:foreground "#ff39a3" :weight bold))
          ("⚔INPROCESS"  . "orangered")
          ("✘CANCELED" . (:foreground "gray" :weight bold))
          ("⚑WAITING" . "pink")
          ("✔DONE" . "#008080")))


  ;; (add-hook 'org-mode-hook #'+org-enable-auto-reformat-tables-h)
  (add-hook 'org-mode-hook (lambda ()
                             (show-paren-local-mode -1)))

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
  :straight (calfw :includes (calfw-org calfw-cal) :host github :repo "zemaye/emacs-calfw" :files ("*.el"))
  :bind (:map cfw:calendar-mode-map
              ("s" . cfw:show-details-command))
  :custom
  (cfw:display-calendar-holidays nil)
  :config
  (use-package calfw-org
    :after org))

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
  (evil-window-move-far-left)
  ;;  HACK: kill unneeded org buffer
  (kill-buffer "done.org")
  (kill-buffer "goals.org"))

(use-package org-pomodoro
  :after org
  :config
  (setq alert-default-style 'notifier)

  (defun org-pomodoro (&optional arg)
    "Start a new pomodoro or stop the current one.
When no timer is running for `org-pomodoro` a new pomodoro is started and
the current task is clocked in.  Otherwise EMACS will ask whether we´d like to
kill the current timer, this may be a break or a running pomodoro."
    (interactive "P")

    (when (and org-pomodoro-last-clock-in
               org-pomodoro-expiry-time
               (org-pomodoro-expires-p)
               (y-or-n-p "Reset pomodoro count? "))
      (setq org-pomodoro-count 0))
    (setq org-pomodoro-last-clock-in (current-time))

    (cond
     ;; possibly break from overtime
     ((and (org-pomodoro-active-p) (eq org-pomodoro-state :overtime))
      (org-pomodoro-finished))
     ;; Maybe kill running pomodoro
     ((org-pomodoro-active-p)
      (if (or (not org-pomodoro-ask-upon-killing)
              (y-or-n-p "There is already a running timer.  Would you like to stop it? "))
          (org-pomodoro-kill)
        (message "Alright, keep up the good work!")))
     ;; or start and clock in pomodoro
     (t
      (cond
       ((equal arg '(4))
        (let ((current-prefix-arg '(4)))
          (call-interactively 'org-clock-in)))
       ((equal arg '(16))
        (call-interactively 'org-clock-in-last))
       ((memq major-mode (list 'org-mode 'org-journal-mode))
        (call-interactively 'org-clock-in))
       ((eq major-mode 'org-agenda-mode)
        (org-with-point-at (org-get-at-bol 'org-hd-marker)
          (call-interactively 'org-clock-in)))
       (t (let ((current-prefix-arg '(4)))
            (call-interactively 'consult-clock-in))))
      (org-pomodoro-start :pomodoro))))
  )


;; -Notification only for mac os
(when *sys/mac*
  (add-hook '+my/first-input-hook
            (lambda ()
              (run-with-timer 5 nil
                              (lambda ()
                                (with-eval-after-load 'org
                                  (require 'appt)

                                  (setq appt-time-msg-list nil) ;; clear existing appt list
                                  (setq appt-display-interval '5) ;; warn every 5 minutes from t - appt-message-warning-time
                                  (setq
                                   appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
                                   appt-display-mode-line nil ;; don't show in the modeline
                                   appt-display-format 'window) ;; pass warnings to the designated window function
                                  (setq appt-disp-window-function (function ct/appt-display-native))

                                  (appt-activate 1) ;; activate appointment notification
                                        ; (display-time) ;; Clock in modeline

                                  ;; brew install terminal-notifier
                                  (defun ct/send-notification (title msg)
                                    (let ((notifier-path (executable-find "terminal-notifier")))
                                      (start-process
                                       "Appointment Alert"
                                       nil
                                       notifier-path
                                       "-message" msg
                                       "-title" title
                                       "-sender" "org.gnu.Emacs"
                                       "-activate" "org.gnu.Emacs")))
                                  (defun ct/appt-display-native (min-to-app new-time msg)
                                    (ct/send-notification
                                     (format "Appointment in %s minutes" min-to-app) ; Title
                                     (format "%s" msg))) ; Message/detail text
                                  ;; Agenda-to-appointent hooks
                                  (org-agenda-to-appt) ;; generate the appt list from org agenda files on emacs launch
                                  (run-at-time nil 900 'org-agenda-to-appt) ;; update appt list hourly
                                  (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
                                  ))))))
;; -Notification

(provide 'init-org)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-org.el ends here
