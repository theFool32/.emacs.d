;;; org-plan-tests.el --- Focus selection regression checks -*- lexical-binding: t; -*-

;; Run with: emacs --batch -Q -l tests/org-plan-tests.el -f ert-run-tests-batch-and-exit
(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'org-agenda)
(require 'org-capture)
(require 'org-id)
;; This batch suite must never write the user's ID cache on exit.
(remove-hook 'kill-emacs-hook #'org-id-locations-save)

(defvar +org-capture-file-gtd nil)
(defvar +org-capture-file-proj nil)
(defvar +org-capture-file-diary nil)
(defvar +org-capture-file-routine nil)
(defvar org-plan-test-agenda-files-expression nil)

(defconst org-plan-test-init-file
  (expand-file-name "../init.el" (file-name-directory (or load-file-name buffer-file-name))))

(dolist (package '("dash" "org-reverse-datetree"))
  (add-to-list 'load-path
               (expand-file-name (concat "elpaca/sources/" package)
                                 (file-name-directory org-plan-test-init-file))))
(require 'org-reverse-datetree)

;; Load only the relevant configuration, without starting package managers,
;; timers, or visiting the user's Org files.
(with-temp-buffer
  (insert-file-contents org-plan-test-init-file)
  (emacs-lisp-mode)
  (check-parens)
  (goto-char (point-min))
  (let (form)
    (condition-case nil
        (while t
          (setq form (read (current-buffer)))
          (cond
           ((and (eq (car-safe form) 'defvar)
                 (string-prefix-p "+my/org-plan-" (symbol-name (cadr form))))
            (eval form t))
           ((and (eq (car-safe form) 'defun)
                 (or (string-prefix-p "+my/org-plan-" (symbol-name (cadr form)))
                     (memq (cadr form) '(+my/open-org-agenda
                                        +my/org-datetree-find-date-create
                                        +my-org/mark-done))))
            (eval form t))
           ((and (eq (car-safe form) 'use-package)
                 (eq (cadr form) 'org-reverse-datetree))
            (dolist (item (cddr form))
              (when (and (eq (car-safe item) 'setq-default)
                         (eq (cadr item) 'org-reverse-datetree-level-formats))
                (eval item t))))
           ((and (eq (car-safe form) 'use-package)
                 (memq (cadr form) '(org org-agenda)))
            (dolist (item (cddr form))
              (when (and (eq (car-safe item) 'setq)
                         (eq (cadr item) 'org-agenda-files))
                (setq org-plan-test-agenda-files-expression (nth 2 item)))
              (when (or (and (eq (car-safe item) 'setq)
                             (memq (cadr item) '(org-capture-templates org-agenda-custom-commands
                                                org-todo-keywords)))
                        (and (eq (car-safe item) 'defun)
                             (eq (cadr item) 'my/org-save-after-command))
                        (and (eq (car-safe item) 'advice-add)
                             (equal (cadr item) '(quote org-agenda-todo))
                             (equal (nth 3 item) '(function my/org-save-after-command))))
                (eval item t))))))
      (end-of-file nil))))

(defmacro org-plan-test-at-task (contents &rest body)
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,contents)
     (org-mode)
     (goto-char (point-min))
     ,@body))

(defmacro org-plan-test-with-archive-files (source-text diary-text &rest body)
  (declare (indent 2))
  `(let* ((directory (make-temp-file "org-diary-archive-test-" t))
          (+org-capture-file-gtd (expand-file-name "gtd.org" directory))
          (+org-capture-file-diary (expand-file-name "diary.org" directory))
          (org-id-locations-file (expand-file-name "ids" directory))
          (org-id-locations (make-hash-table :test #'equal))
          (org-id-files nil)
          (system-time-locale "C")
          source)
     (unwind-protect
         (progn
           (with-temp-file +org-capture-file-gtd (insert ,source-text))
           (with-temp-file +org-capture-file-diary (insert ,diary-text))
           (setq source (find-file-noselect +org-capture-file-gtd))
           (with-current-buffer source
             (goto-char (point-min))
             ,@body))
       (dolist (buffer (buffer-list))
         (when (and (buffer-file-name buffer)
                    (string-prefix-p directory (buffer-file-name buffer)))
           (with-current-buffer buffer
             (setq buffer-read-only nil)
             (set-buffer-modified-p nil))
           (kill-buffer buffer)))
       (delete-directory directory t))))

(ert-deftest org-diary-archive-uses-closed-date-and-preserves-the-subtree ()
  (org-plan-test-with-archive-files
      "* DONE Archived task\nCLOSED: [2026-10-09 Fri 10:30]\n:PROPERTIES:\n:ID: archive-root\n:FOCUS_DAY: 2026-10-09\n:PLAN_DAYS: 2026-10-08 2026-10-09\n:END:\nTask body\n** DONE Child\n:PROPERTIES:\n:ID: archive-child\n:END:\nChild body\n* CANCELED Second task\nCLOSED: [2026-10-09 Fri 12:00]\n* UNDONE Earlier task\nCLOSED: [2026-10-08 Thu 18:00]\n* TODO Remaining\n"
      "* 2026\n** 2026-10 October\n*** W40 (2026-10-04 - 2026-10-10)\n**** 2026-10-09 Friday\n***** Daily Goals :ACT_TODAY:\nPreserve this existing goal text.\n***** Archived :legacy:\n****** DONE Existing archive\nExisting archive body.\n* End\n"
    (dotimes (_ 3)
      (with-current-buffer source
        (goto-char (point-min))
        (+my-org/mark-done)))
    (should (equal (with-temp-buffer
                     (insert-file-contents +org-capture-file-gtd)
                     (buffer-string))
                   "* TODO Remaining\n"))
    (with-current-buffer (find-file-noselect +org-capture-file-diary)
      (should-not (buffer-modified-p))
      (should (= (how-many "^\\*\\*\\*\\*\\* Archived" (point-min) (point-max)) 2))
      (should (= (how-many "^\\*\\*\\*\\*\\* Daily Goals" (point-min) (point-max)) 1))
      (should (string-match-p "Daily Goals :ACT_TODAY:\nPreserve this existing goal text\."
                              (buffer-string)))
      (should (string-match-p "Existing archive body\." (buffer-string)))
      (goto-char (org-find-exact-headline-in-buffer "Archived task"))
      (should (= (org-current-level) 6))
      (should (equal (org-entry-get nil "FOCUS_DAY") "2026-10-09"))
      (should (equal (org-entry-get nil "PLAN_DAYS") "2026-10-08 2026-10-09"))
      (should (equal (org-entry-get nil "ID") "archive-root"))
      (should (equal (org-entry-get nil "CLOSED") "[2026-10-09 Fri 10:30]"))
      (org-up-heading-safe)
      (should (equal (org-get-heading t t t t) "Archived"))
      (org-up-heading-safe)
      (should (equal (org-get-heading t t t t) "2026-10-09 Friday"))
      (goto-char (org-find-exact-headline-in-buffer "Child"))
      (should (= (org-current-level) 7))
      (should (equal (org-entry-get nil "ID") "archive-child"))
      (goto-char (org-find-exact-headline-in-buffer "Earlier task"))
      (org-up-heading-safe)
      (org-up-heading-safe)
      (should (equal (org-get-heading t t t t) "2026-10-08 Thursday")))
    (should (file-equal-p (car (org-id-find "archive-root")) +org-capture-file-diary))
    (should (file-equal-p (car (org-id-find "archive-child")) +org-capture-file-diary))))

(ert-deftest org-diary-archive-validates-before-moving-any-content ()
  (org-plan-test-with-archive-files "* TODO Open\n" "* Existing diary\n"
    (let ((original (buffer-string)))
      (should-error (+my-org/mark-done) :type 'user-error)
      (should (equal original (buffer-string))))
    (erase-buffer)
    (insert "* DONE Closed\nCLOSED: [2026-10-09 Fri 10:30]\n")
    (goto-char (point-min))
    (let* ((original (buffer-string))
           (diary (find-file-noselect +org-capture-file-diary)))
      (with-current-buffer diary (setq buffer-read-only t))
      (should-error (+my-org/mark-done) :type 'buffer-read-only)
      (should (equal original (buffer-string)))
      (should (equal "* Existing diary\n"
                     (with-current-buffer diary (buffer-string)))))))

(ert-deftest org-plan-period-uses-iso-year-and-week ()
  ;; January 1 can belong to the previous ISO week year.
  (let ((time (encode-time 0 0 12 1 1 2027)))
    (should (equal (+my/org-plan-period 'day time) "2027-01-01"))
    (should (equal (+my/org-plan-period 'week time) "2026-W53"))
    (should (equal (+my/org-plan-period 'month time) "2027-01"))))

(ert-deftest org-plan-toggle-keeps-task-state-and-dates ()
  (let ((org-todo-keywords '((sequence "TODO" "WIP" "|" "DONE"))))
    (org-plan-test-at-task
      "* WIP Task\nSCHEDULED: <2026-11-01 Sun> DEADLINE: <2026-11-10 Tue>\n"
    (let ((original (buffer-string)))
      (+my/org-plan-this-week)
      (should (equal (org-entry-get nil "FOCUS_WEEK") (+my/org-plan-period 'week)))
      (should (equal (org-get-todo-state) "WIP"))
      (should (equal (org-entry-get nil "SCHEDULED") "<2026-11-01 Sun>"))
      (should (equal (org-entry-get nil "DEADLINE") "<2026-11-10 Tue>"))
      (+my/org-plan-this-week)
      (should-not (org-entry-get nil "FOCUS_WEEK"))
      ;; Org may retain an empty PROPERTIES drawer; there is still one task.
      (should (= (how-many "^\\* " (point-min) (point-max)) 1))
      (should (string-match-p "WIP Task" original))))))

(ert-deftest org-plan-periods-are-independent-and-stale-values-update ()
  (org-plan-test-at-task "* TODO Task\n:PROPERTIES:\n:FOCUS_WEEK: 2020-W01\n:END:\n"
    (+my/org-plan-today)
    (+my/org-plan-this-week)
    (+my/org-plan-this-month)
    (+my/org-plan-this-week)
    (should-not (org-entry-get nil "FOCUS_WEEK"))
    (should (equal (org-entry-get nil "FOCUS_DAY") (+my/org-plan-period 'day)))
    (should (equal (org-entry-get nil "FOCUS_MONTH") (+my/org-plan-period 'month)))))

(ert-deftest org-plan-tag-follows-selections-without-removing-other-tags ()
  (org-plan-test-at-task "* TODO Task :work:company:\n"
    (dolist (command '(+my/org-plan-today +my/org-plan-this-week
                       +my/org-plan-this-month))
      (funcall command)
      (should (equal (org-get-tags nil t) '("work" "company" "plan"))))
    (+my/org-plan-today)
    (+my/org-plan-this-week)
    (should (member "plan" (org-get-tags nil t)))
    (+my/org-plan-this-month)
    (should (equal (org-get-tags nil t) '("work" "company")))))

(ert-deftest org-plan-tag-retains-history-and-does-not-mark-children ()
  (org-plan-test-at-task
      "* TODO Parent :plan:\n** TODO Task :work:\n:PROPERTIES:\n:PLAN_DAYS: 2020-01-01\n:END:\n*** TODO Child\n"
    (outline-next-heading)
    (+my/org-plan-today)
    (+my/org-plan-today)
    (should-not (org-entry-get nil "FOCUS_DAY"))
    (should (equal (org-get-tags nil t) '("work" "plan")))
    (let ((text (buffer-string)))
      (+my/org-plan-sync-tag)
      (should (equal text (buffer-string))))
    (org-entry-delete nil "PLAN_DAYS")
    (+my/org-plan-sync-tag)
    (should (equal (org-get-tags nil t) '("work")))
    (outline-next-heading)
    (should-not (org-get-tags nil t))))

(ert-deftest org-plan-refuses-non-task-headings ()
  (org-plan-test-at-task "* Notes\n"
    (let ((original (buffer-string)))
      (should-error (+my/org-plan-today) :type 'user-error)
      (should (equal (buffer-string) original)))))

(ert-deftest org-plan-bulk-fills-mixed-selection-and-removes-all ()
  (let ((source (generate-new-buffer " *plan-test-source*")))
    (unwind-protect
        (let (first second (redraws 0))
          (with-current-buffer source
            (org-mode)
            (insert "* TODO First\n* TODO Second\n")
            (goto-char (point-min))
            (org-entry-put nil "FOCUS_WEEK" (+my/org-plan-period 'week))
            (setq first (point-marker))
            (outline-next-heading)
            (setq second (point-marker)))
          (with-temp-buffer
            (org-agenda-mode)
            (setq org-agenda-bulk-marked-entries (list first first second))
            (cl-letf (((symbol-function 'org-agenda-redo)
                       (lambda (&rest _) (cl-incf redraws))))
              (+my/org-plan-this-week)
              (should-not org-agenda-bulk-marked-entries)
              (dolist (marker (list first second))
                (org-with-point-at marker
                  (should (equal (org-entry-get nil "FOCUS_WEEK")
                                 (+my/org-plan-period 'week)))))
              (setq org-agenda-bulk-marked-entries (list first second))
              (+my/org-plan-this-week)
              (dolist (marker (list first second))
                (org-with-point-at marker
                  (should-not (org-entry-get nil "FOCUS_WEEK"))))
              (should (= redraws 2)))))
      (kill-buffer source))))

(ert-deftest org-plan-bulk-validates-before-any-mutation ()
  (org-plan-test-at-task "* TODO Valid\n* Notes\n"
    (let ((first (point-marker)) second (original (buffer-string)))
      (outline-next-heading)
      (setq second (point-marker))
      (with-temp-buffer
        (org-agenda-mode)
        (setq org-agenda-bulk-marked-entries (list first second))
        (should-error (+my/org-plan-today) :type 'user-error))
      (should (equal (buffer-string) original)))))

(ert-deftest org-plan-captures-assign-current-period-to-one-task ()
  (dolist (spec '(("pd" . day) ("pw" . week) ("pm" . month)))
    (let* ((entry (assoc (car spec) org-capture-templates))
           (template (nth 4 entry))
           (org-capture-plist (list :type 'entry :default-time (current-time)))
           (text (org-capture-fill-template template)))
      (should (equal (nth 3 entry) '(file +org-capture-file-gtd)))
      (org-plan-test-at-task text
        (should (equal (org-get-tags nil t) '("plan"))))
      (should (string-match-p
               (regexp-quote (format ":%s: %s"
                                     (+my/org-plan-property (cdr spec))
                                     (+my/org-plan-period (cdr spec))))
               text))
      (should (= (with-temp-buffer
                   (insert text)
                   (how-many "^\\* TODO" (point-min) (point-max)))
                 1)))))

(ert-deftest org-plan-replanning-retains-past-membership-and-removes-only-current ()
  (dolist (period '(day week month))
    (let* ((previous (encode-time 0 0 12 13 10 2026))
           (time (encode-time 0 0 12 20 11 2026))
           (old-value (+my/org-plan-period period previous))
           (new-value (+my/org-plan-period period time)))
      (org-plan-test-at-task "* TODO Replanned\n"
        ;; Existing focus-only entries are retained on their next replan.
        (org-entry-put nil (+my/org-plan-property period) old-value)
        (cl-letf (((symbol-function 'current-time) (lambda () time)))
          (+my/org-plan-toggle period)
          (should (equal (org-entry-get nil (+my/org-plan-property period)) new-value))
          (dolist (date (list previous time))
            (should (= (length (org-map-entries
                                #'point (+my/org-plan-match period date)))
                       1)))
          (+my/org-plan-toggle period)
          (should (= (length (org-map-entries
                              #'point (+my/org-plan-match period previous)))
                     1))
          (should-not (org-map-entries #'point (+my/org-plan-match period time)))
          (should (= (how-many "^\\* TODO" (point-min) (point-max)) 1)))))))

(ert-deftest org-plan-history-query-matches-whole-period-identifiers ()
  (let ((time (encode-time 0 0 12 13 10 2026)))
    (dolist (spec '((day "PLAN_DAYS" "2026-10-130")
                    (week "PLAN_WEEKS" "2026-W420")
                    (month "PLAN_MONTHS" "2026-100")))
      (org-plan-test-at-task "* TODO Other period\n"
        (org-entry-put nil (nth 1 spec) (nth 2 spec))
        (should-not (org-map-entries #'point (+my/org-plan-match (car spec) time)))))))

(ert-deftest org-plan-unified-configuration-retires-old-goal-workflows ()
  (should (equal (mapcar #'car org-agenda-custom-commands) '("p")))
  (dolist (key '("g" "m" "W"))
    (should-not (assoc key org-capture-templates))))

(ert-deftest org-plan-progress-does-not-count-missed-or-canceled-as-done ()
  (let* ((entries '((:state "DONE") (:state "UNDONE") (:state "CANCELED")
                   (:state "TODO") (:state "WAITING") (:state "WIP")))
         (stats (+my/org-plan-stats entries)))
    (should (= (plist-get stats :total) 6))
    (should (= (plist-get stats :done) 1))
    (should (= (plist-get stats :open) 3))
    (should (= (plist-get stats :undone) 1))
    (should (= (plist-get stats :canceled) 1))
    (should (= (plist-get stats :percent) 16))
    (should (= (plist-get (+my/org-plan-stats nil) :percent) 0))))

(ert-deftest org-plan-changes-are-limited-to-gtd ()
  (let ((+org-capture-file-gtd "/private/tmp/plan-protection/gtd.org")
        (+org-capture-file-diary "/private/tmp/plan-protection/diary.org"))
    (dolist (file '("diary.org" "done.org" "proj.org"))
      (org-plan-test-at-task "* TODO Old entry\n"
        (setq buffer-file-name (expand-file-name file "/private/tmp/plan-protection"))
        (let ((original (buffer-string)))
          (should-error (+my/org-plan-today) :type 'user-error)
          (should (equal original (buffer-string))))))))

(ert-deftest org-plan-agenda-saver-skips-dirty-legacy-buffers ()
  (let* ((directory (make-temp-file "org-plan-save-test-" t))
         (+org-capture-file-gtd (expand-file-name "gtd.org" directory))
         (+org-capture-file-diary (expand-file-name "diary.org" directory))
         (done (expand-file-name "done.org" directory))
         (org-id-locations-file (expand-file-name "ids" directory))
         (org-id-locations nil)
         (original "* TODO Original\n"))
    (unwind-protect
        (progn
          (dolist (file (list +org-capture-file-gtd +org-capture-file-diary done))
            (with-temp-file file (insert original))
            (with-current-buffer (find-file-noselect file)
              (goto-char (point-max))
              (insert "Unsaved notes\n")))
          (with-current-buffer (find-file-noselect +org-capture-file-gtd)
            (my/org-save-after-command))
          (should-not (with-current-buffer (find-file-noselect +org-capture-file-gtd)
                        (buffer-modified-p)))
          (dolist (file (list +org-capture-file-diary done))
            (should (with-current-buffer (find-file-noselect file) (buffer-modified-p)))
            (should (equal original
                           (with-temp-buffer
                             (insert-file-contents file)
                             (buffer-string))))))
      (dolist (buffer (buffer-list))
        (when (and (buffer-file-name buffer)
                   (string-prefix-p directory (buffer-file-name buffer)))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(defmacro org-plan-test-with-review-files (contents &rest body)
  (declare (indent 1))
  `(let* ((directory (make-temp-file "org-plan-review-test-" t))
         (+org-capture-file-gtd (expand-file-name "gtd.org" directory))
         (+org-capture-file-diary (expand-file-name "diary.org" directory))
         (+org-capture-file-routine (expand-file-name "routine.org" directory))
         (org-agenda-files (list +org-capture-file-gtd +org-capture-file-diary
                                +org-capture-file-routine))
         (org-agenda-buffer-name "*Plan Review Test Agenda*")
         (enable-local-variables nil)
         (enable-local-eval nil))
    (unwind-protect
        (save-window-excursion
          (dolist (name '("gtd.org" "diary.org" "done.org" "routine.org" "proj.org"))
            (with-temp-file (expand-file-name name directory)
              (insert (or (cdr (assoc name ,contents)) ""))))
          ,@body)
      (dolist (buffer (buffer-list))
        (when (or (equal (buffer-name buffer) org-agenda-buffer-name)
                  (and (buffer-file-name buffer)
                       (string-prefix-p directory (buffer-file-name buffer))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

(ert-deftest org-plan-review-uses-calendar-months-and-includes-legacy-records ()
  (org-plan-test-with-review-files
      '(("gtd.org" . "* DONE This month\n:PROPERTIES:\n:FOCUS_MONTH: 2026-01\n:END:\n* TODO Still open\n:PROPERTIES:\n:FOCUS_MONTH: 2026-01\n:END:\n* DONE Ordinary\nCLOSED: [2026-01-04 Sun 12:00]\n* TODO Tag only :plan:\n* DONE Future\n:PROPERTIES:\n:FOCUS_MONTH: 2026-02\n:END:\n* DONE Outside year\n:PROPERTIES:\n:FOCUS_DAY: 2025-01-14\n:END:\n")
        ("diary.org" . "* 2025\n** 2025-12 December\n*** W52 (2025-12-28 - 2026-01-03)\n**** Weekly Goals :ACT_WEEK:\n***** UNDONE Previous week\nCLOSED: [2026-01-10 Sat 12:00]\n")
        ("done.org" . "* 2025\n** 2025-12 December\n*** Monthly Goals\n**** DONE Previous month\nCLOSED: [2026-01-08 Thu 12:00]\n**** CANCELED Previous cancellation\n*** W52 (2025-12-28 - 2026-01-03)\n**** 2025-12-31 Wednesday\n***** Daily Goals\n****** DONE Previous day\n****** TODO Legacy open\n******* TODO Subtask is not a goal\n***** Archived\n****** DONE Ordinary archived\nCLOSED: [2025-12-31 Wed 23:59]\n** 2025-01 January\n*** Monthly Goals\n**** DONE First year month\n")
        ("proj.org" . "* DONE Excluded project\n:PROPERTIES:\n:FOCUS_MONTH: 2026-01\n:END:\n"))
    (+my/org-plan-history (encode-time 0 0 12 15 1 2026))
    (with-current-buffer org-agenda-buffer-name
      (should (derived-mode-p 'org-agenda-mode))
      (should (string-match-p
               "这个月  2026-01-01 — 2026-01-15[[:space:]]+✓ 完成 1/2（50%）  ○ 待完成 1"
               (buffer-string)))
      (should (string-match-p
               "上个月  2025-12-01 — 2025-12-31[[:space:]]+✓ 完成 2/5（40%）  ○ 待完成 1  × 未达成 1  − 取消 1"
               (buffer-string)))
      (should (string-match-p
               "过去一年  2025-01-15 — 2026-01-15[[:space:]]+✓ 完成 4/8（50%）  ○ 待完成 2  × 未达成 1  − 取消 1"
               (buffer-string)))
      (dolist (title '("Ordinary" "Tag only" "Subtask is not a goal" "Future" "Outside year" "Excluded project"))
        (should-not (string-match-p title (buffer-string))))
      (should (string-match-p "Still open" (buffer-string)))
      (should (string-match-p "Legacy open" (buffer-string)))
      (goto-char (point-min))
      (search-forward "* 2026-01")
      (search-forward "* 2025-12")
      (search-forward "月 Goals  ✓ 完成 1/2")
      (search-forward "周 Goals  ✓ 完成 0/1")
      (search-forward "日 Goals  ✓ 完成 1/2")
      (search-forward "* 2025-01"))
    (dolist (buffer (org-buffer-list))
      (should-not (buffer-modified-p buffer)))))

(ert-deftest org-plan-review-folds-groups-sorts-tasks-and-jumps-to-source ()
  (org-plan-test-with-review-files
      '(("gtd.org" . "* DONE Oldest\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-01\n:END:\n* DONE Zulu\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-14\n:END:\n* DONE Alpha\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-14\n:END:\n")
        ("diary.org" . "* UNDONE Missed\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-15\n:END:\n* CANCELED Canceled task\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-15\n:END:\n")
        ("done.org" . "* DONE Newest\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-15\n:END:\n* DONE September\n:PROPERTIES:\n:FOCUS_MONTH: 2026-09\n:END:\n"))
    (+my/org-plan-history (encode-time 0 0 12 15 10 2026))
    (with-current-buffer org-agenda-buffer-name
      (should buffer-read-only)
      (goto-char (point-min))
      (search-forward "Newest")
      (should (invisible-p (1- (point))))
      (should (= (how-many "Newest" (point-min) (point-max)) 1))
      (goto-char (point-min))
      (search-forward "这个月")
      (call-interactively (key-binding (kbd "RET")))
      (dolist (title '("Missed" "Newest" "Alpha" "Zulu" "Oldest" "Canceled task"))
        (search-forward title)
        (should-not (invisible-p (1- (point)))))
      (search-forward "September")
      (should (invisible-p (1- (point))))
      (goto-char (point-min))
      (search-forward "** ✓ 已完成（4）")
      (call-interactively (key-binding (kbd "TAB")))
      (search-forward "Newest")
      (should (invisible-p (1- (point))))
      (search-backward "Missed")
      (should-not (invisible-p (1- (point))))
      (goto-char (point-min))
      (search-forward "** ✓ 已完成（4）")
      (call-interactively (key-binding (kbd "RET")))
      (search-forward "Newest")
      (call-interactively (key-binding (kbd "RET")))
      (should (equal buffer-file-name (expand-file-name "done.org" directory)))
      (should (equal (org-get-heading t t t t) "Newest")))))

(ert-deftest org-plan-review-refresh-rolls-over-and-preserves-explicit-reference ()
  (org-plan-test-with-review-files
      '(("done.org" . "* DONE January\n:PROPERTIES:\n:FOCUS_MONTH: 2026-01\n:END:\n* DONE February\n:PROPERTIES:\n:FOCUS_MONTH: 2026-02\n:END:\n"))
    (let ((time (encode-time 0 0 12 31 1 2026)))
      (cl-letf (((symbol-function 'current-time) (lambda () time)))
        (+my/org-plan-history)
        (should (string-match-p "这个月  2026-01-01 — 2026-01-31" (buffer-string)))
        (should-not (string-match-p "February" (buffer-string)))
        (setq time (encode-time 0 0 12 1 2 2026))
        (goto-char (point-min))
        (org-agenda-redo t)
        (should (string-match-p "这个月  2026-02-01 — 2026-02-01" (buffer-string)))
        (should (string-match-p "上个月  2026-01-01 — 2026-01-31" (buffer-string)))
        (should (string-match-p "February" (buffer-string)))
        (+my/org-plan-history (encode-time 0 0 12 31 1 2026))
        (goto-char (point-min))
        (org-agenda-redo t)
        (should (string-match-p "这个月  2026-01-01 — 2026-01-31" (buffer-string)))
        (should-not (string-match-p "February" (buffer-string)))))))

(ert-deftest org-plan-review-handles-leap-months-and-empty-periods ()
  (org-plan-test-with-review-files
      '(("done.org" . "* DONE Leap day\n:PROPERTIES:\n:FOCUS_DAY: 2024-02-29\n:END:\n"))
    (+my/org-plan-history (encode-time 0 0 12 1 3 2024))
    (should (string-match-p
             "上个月  2024-02-01 — 2024-02-29[[:space:]]+✓ 完成 1/1（100%）"
             (buffer-string)))
    (should (string-match-p
             "这个月  2024-03-01 — 2024-03-01[[:space:]]+无 Goals"
             (buffer-string)))
    (should (equal (format-time-string "%F" (+my/org-plan-review-start 12
                                              (encode-time 0 0 12 29 2 2024)))
                   "2023-02-28"))))

(ert-deftest org-plan-review-merges-old-and-new-membership-without-counting-copies ()
  (org-plan-test-with-review-files
      '(("gtd.org" . "* DONE Canonical\nCLOSED: [2026-11-10 Tue 12:00]\n:PROPERTIES:\n:ID: same-goal\n:FOCUS_DAY: 2026-10-15\n:FOCUS_WEEK: 2026-W42\n:PLAN_WEEKS: 2026-W42\n:FOCUS_MONTH: 2026-10\n:PLAN_MONTHS: 2026-09 2026-10\n:END:\n** TODO Inherited child\n* WIP Pending\n:PROPERTIES:\n:FOCUS_MONTH: 2026-11\n:END:\n* DONE Not a goal\nCLOSED: [2026-11-10 Tue 13:00]\n")
        ("diary.org" . "* 2026\n** 2026-10 October\n*** W41 (2026-10-11 - 2026-10-17)\n**** 2026-10-15 Thursday\n***** Daily Goals\n****** UNDONE Old copy\n:PROPERTIES:\n:ID: same-goal\n:END:\n"))
    (+my/org-plan-history (encode-time 0 0 12 15 11 2026))
    (should (string-match-p "这个月  2026-11-01 — 2026-11-15[[:space:]]+✓ 完成 0/1（0%）  ○ 待完成 1"
                            (buffer-string)))
    (should (string-match-p "上个月  2026-10-01 — 2026-10-31[[:space:]]+✓ 完成 1/1（100%）"
                            (buffer-string)))
    (should (string-match-p "过去一年  2025-11-15 — 2026-11-15[[:space:]]+✓ 完成 1/2（50%）"
                            (buffer-string)))
    (should (= (how-many "Canonical" (point-min) (point-max)) 2))
    (dolist (title '("Old copy" "Not a goal" "Inherited child"))
      (should-not (string-match-p title (buffer-string))))
    (goto-char (point-min))
    (search-forward "* 2026-10")
    (dolist (label '("月 Goals" "周 Goals" "日 Goals"))
      (search-forward (concat label "  ✓ 完成 1/1（100%）")))))

(ert-deftest org-plan-review-recognizes-old-week-spelling-and-valid-iso-weeks ()
  (should (equal (format-time-string "%F" (+my/org-plan-review-date 'week "2026-W01"))
                 "2025-12-29"))
  (should-not (+my/org-plan-review-date 'week "2026-W54"))
  (should-not (+my/org-plan-review-date 'day "2026-02-30"))
  (dolist (heading '("Weekly Goals" "Weakly Goals" "Weeakly Goals"))
    (org-plan-test-at-task
        (concat "* 2025\n** 2025-12 December\n*** W52 (2025-12-28 - 2026-01-03)\n**** "
                heading " :ACT_WEEK:\n***** TODO Legacy\n")
      (goto-char (point-max))
      (org-back-to-heading t)
      (should (equal (+my/org-plan-review-legacy-selection) '(week . "2025-12-28"))))))

(ert-deftest org-plan-review-keeps-state-colors-in-summary-and-rendered-groups ()
  (let ((summary (+my/org-plan-review-summary
                  '((:state "DONE") (:state "TODO") (:state "UNDONE") (:state "CANCELED")))))
    (dolist (entry '(("✓ 完成" . success) ("○ 待完成" . warning)
                     ("× 未达成" . error) ("− 取消" . shadow)))
      (string-match (regexp-quote (car entry)) summary)
      (should (eq (get-text-property (match-beginning 0) 'face summary) (cdr entry)))))
  (org-plan-test-with-review-files
      '(("gtd.org" . "* TODO Pending\n:PROPERTIES:\n:FOCUS_MONTH: 2026-10\n:END:\n"))
    (+my/org-plan-history (encode-time 0 0 12 15 10 2026))
    (goto-char (point-min))
    (search-forward "○ 待完成 1")
    (should (eq (get-text-property (1- (point)) 'face) 'warning))
    (search-forward "** ○ 待完成（1）")
    (should (eq (plist-get (get-text-property (1- (point)) 'face) :inherit) 'warning))))

(ert-deftest org-plan-agenda-reads-done-from-the-original-and-rolls-over ()
  (let* ((directory (make-temp-file "org-plan-test-" t))
         (+org-capture-file-gtd (expand-file-name "gtd.org" directory))
         (+org-capture-file-proj (expand-file-name "proj.org" directory))
         (+org-capture-file-diary (expand-file-name "diary.org" directory))
         (+org-capture-file-routine (expand-file-name "routine.org" directory))
         (org-agenda-files (eval org-plan-test-agenda-files-expression t))
         (org-agenda-buffer-name "*Plan Test Agenda*")
         (org-agenda-window-setup 'current-window)
         (org-agenda-span 1)
         (org-log-done nil)
         (time (encode-time 0 0 12 13 10 2026))
         source)
    (unwind-protect
        (progn
          (with-temp-file +org-capture-file-gtd
            (insert "* TODO Selected\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-13\n:FOCUS_WEEK: 2026-W42\n:FOCUS_MONTH: 2026-10\n:END:\n* TODO Old\n:PROPERTIES:\n:FOCUS_WEEK: 2026-W41\n:END:\n"))
          (with-temp-file +org-capture-file-diary)
          (with-temp-file +org-capture-file-routine)
          (with-temp-file +org-capture-file-proj
            (insert "* TODO Project action\nSCHEDULED: <2026-10-13 Tue>\n:PROPERTIES:\n:FOCUS_DAY: 2026-10-13\n:FOCUS_WEEK: 2026-W42\n:FOCUS_MONTH: 2026-10\n:END:\n"))
          (setq source (find-file-noselect +org-capture-file-gtd))
          (cl-letf (((symbol-function 'current-time) (lambda () time)))
            (org-agenda nil "p")
            (with-current-buffer org-agenda-buffer-name
              (save-excursion
                (goto-char (point-min))
                (search-forward "Day 2026-10-13:")
                (search-forward "Week 2026-W42:")
                (search-forward "Month 2026-10:"))
              (should (equal org-agenda-files
                             (list +org-capture-file-gtd +org-capture-file-diary
                                   +org-capture-file-routine)))
              (should-not (string-match-p "Project action" (buffer-string)))
              (goto-char (point-min))
              (search-forward "Selected")
              (let ((marker (org-get-at-bol 'org-hd-marker)))
                (should (eq (marker-buffer marker) source))
                (org-agenda-todo 'done))
              (should (= (how-many "DONE Selected" (point-min) (point-max)) 3))
              (should (string-match-p
                       (regexp-quote "Day 2026-10-13: DONE 1/1 (100%)")
                       (buffer-string)))
              (org-agenda-redo t)
              (let ((end (save-excursion
                           (goto-char (point-min))
                           (search-forward "Task Pool")
                           (line-beginning-position))))
                ;; The same completed source task appears in three plan blocks.
                (should (= (how-many "DONE Selected" (point-min) end) 3))
                (should-not (string-match-p "TODO Old"
                                            (buffer-substring (point-min) end))))
              (setq time (encode-time 0 0 12 20 10 2026))
              (org-agenda-redo t)
              ;; A later week/day drops old selection, while the month retains it.
              (should (= (how-many "DONE Selected" (point-min) (point-max)) 1)))))
      (dolist (buffer (buffer-list))
        (when (or (equal (buffer-name buffer) org-agenda-buffer-name)
                  (and (buffer-file-name buffer)
                       (string-prefix-p directory (buffer-file-name buffer))))
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory directory t))))

;;; org-plan-tests.el ends here
