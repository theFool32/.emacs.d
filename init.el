;;; init.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:

;;; StartUp
(defvar +my/start-time (current-time))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-percentage 0.1)))

;; Load init-custom.el if it exists
(load (expand-file-name "init-custom.el" user-emacs-directory) nil 'nomessage)

(defvar +my/first-input-hook nil)
(defun +my/first-input-hook-fun ()
  "A hook run after the first input."
  (when +my/first-input-hook
    (run-hooks '+my/first-input-hook)
    (setq +my/first-input-hook nil))
  (remove-hook 'pre-command-hook '+my/first-input-hook-fun))
(add-hook 'pre-command-hook '+my/first-input-hook-fun)


;;; Const
;; Load env
(let ((my-env-file (concat user-emacs-directory "env")))
  (when (and (or (display-graphic-p)
                 (daemonp))
             (file-exists-p my-env-file))
    (if (file-readable-p my-env-file)
        (let (envvars environment)
          (with-temp-buffer
            (save-excursion
              (insert "\n")
              (insert-file-contents my-env-file))
            (while (re-search-forward "\n *\\([^#= \n]*\\)=" nil t)
              (push (match-string 1) envvars)
              (push (buffer-substring
                     (match-beginning 1)
                     (1- (or (save-excursion
                               (when (re-search-forward "^\\([^= ]+\\)=" nil t)
                                 (line-beginning-position)))
                             (point-max))))
                    environment)))
          (when environment
            (setq process-environment
                  (append (nreverse environment) process-environment)
                  exec-path
                  (if (member "PATH" envvars)
                      (append (split-string (getenv "PATH") path-separator t)
                              (list exec-directory))
                    exec-path)
                  shell-file-name
                  (if (member "SHELL" envvars)
                      (or (getenv "SHELL") shell-file-name)
                    shell-file-name))
            envvars)))))

;; UserInfo
(setq user-full-name "theFool32")
(setq user-mail-address "saber.rl32@gmail.com")
;; -UserInfo

;; Consts
(defconst *sys/gui*
  (or (display-graphic-p) (daemonp))
  "Are we running on a GUI Emacs?")

(defconst *sys/win32*
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst *sys/linux*
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst *sys/mac*
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst *rg*
  (executable-find "rg")
  "Do we have ripgrep?")

(defconst *git*
  (executable-find "git")
  "Do we have git?")

;;; Package Manager
;;;; Use-package after-call
(defvar +use-package--deferred-pkgs '(t))
(defun use-package-handler/:after-call (name _keyword hooks rest state)
  "Add keyword `:after-call' to `use-package'.
The purpose of this keyword is to expand the lazy-loading
capabilities of `use-package'.  Consult `use-package-concat' and
`use-package-process-keywords' for documentations of NAME, HOOKS,
REST and STATE."
  (if (plist-get state :demand)
      (use-package-process-keywords name rest state)
    (let ((fn (make-symbol (format "grandview--after-call-%s-h" name))))
      (use-package-concat
       `((fset ',fn
               (lambda (&rest _)
                 (condition-case e
                     (let ((default-directory user-emacs-directory))
                       (require ',name))
                   ((debug error)
                    (message "Failed to load deferred package %s: %s" ',name e)))
                 (when-let* ((deferral-list (assq ',name +use-package--deferred-pkgs)))
                   (dolist (hook (cdr deferral-list))
                     (advice-remove hook #',fn)
                     (remove-hook hook #',fn))
                   (setq +use-package--deferred-pkgs
                         (delq deferral-list +use-package--deferred-pkgs))
                   (unintern ',fn nil)))))
       (cl-loop for hook in hooks
                collect (if (string-match-p "-\\(?:functions\\|hook\\)$" (symbol-name hook))
                            `(add-hook ',hook #',fn)
                          `(advice-add #',hook :before #',fn)))
       `((unless (assq ',name +use-package--deferred-pkgs)
           (push '(,name) +use-package--deferred-pkgs))
         (nconc (assq ',name +use-package--deferred-pkgs)
                '(,@hooks)))
       (use-package-process-keywords name rest state)))))
;; (require 'use-package-core)
(require 'use-package)
(push :after-call use-package-deferring-keywords)
(setq use-package-keywords (use-package-list-insert :after-call use-package-keywords :after))
(defalias 'use-package-normalize/:after-call #'use-package-normalize-symlist)

;;;; Straight [not-used]

;; (setq straight--process-log nil
;;       straight-vc-git-default-clone-depth 1
;;       straight-repository-branch "develop"
;;       straight-use-package-by-default t
;;       ;; straight-check-for-modifications '(check-on-save find-when-checking)
;;       straight-check-for-modifications nil)

;; (unless (featurep 'straight)
;;   (defvar bootstrap-version)

;;   (let ((bootstrap-file (concat user-emacs-directory
;;                                 "straight/repos/straight.el/bootstrap.el"))
;;         (bootstrap-version 5))
;;     (unless (file-exists-p bootstrap-file)
;;       (with-current-buffer
;;           (url-retrieve-synchronously
;;            "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;            'silent 'inhibit-cookies)
;;         (goto-char (point-max))
;;         (eval-print-last-sexp)))
;;     (load bootstrap-file nil 'nomessage)))

;; ;; (defun +set-github-mirror (oldfunc &rest args)
;; ;;   (let ((url (apply oldfunc args)))
;; ;;     (replace-regexp-in-string (rx (group "github.com"))
;; ;;                               "hub.fastgit.org" url nil nil 1)))
;; ;; (advice-add 'straight-vc-git--encode-url :around #'+set-github-mirror)

;; (add-to-list 'straight-built-in-pseudo-packages 'eglot)
;; (add-to-list 'straight-built-in-pseudo-packages 'tramp)
;; (add-to-list 'straight-built-in-pseudo-packages 'use-package)
;; (add-to-list 'straight-built-in-pseudo-packages 'project)
;; (add-to-list 'straight-built-in-pseudo-packages 'org)
;; (add-to-list 'straight-built-in-pseudo-packages 'xref)
;; ;; -Straight

;; (defun +my/check-straight-repos ()
;;   (interactive)
;;   (find-file (read-file-name "Repos: " "~/.emacs.d/straight/repos/")))


;;;; elpaca

;;  FIXME: still slow when startup (~0.5s)
(defvar elpaca-installer-version 0.9)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  (elpaca-use-package-mode)
  (setq elpaca-use-package-by-default t))

(mapc
 (lambda (p) (add-to-list 'elpaca-ignored-dependencies p))
 '(xref tramp eglot use-package project org xref recentf winner
        tramp-sh flymake simple diff-mode smerge-mode python css-mode custom
        server help elec-pair paren tab-bar hl-line pulse prog-mode
        lisp-mode treesit imenu eldoc))

;;  HACK: currently `elpaca' will check the version of dependency while some packages do not provide. skip it with a larger default version
(defun elpaca--check-version (e)
  "Ensure E's dependency versions are met."
  (cl-loop
   initially (elpaca--signal e "Checking dependency versions")
   with queued = (elpaca--queued)
   with version-regexp-alist = ;; Handle -dev, -DEV, etc. Why isn't this default?
   (append version-regexp-alist '(("\\(?:-[[:alpha:]]+\\)" . -1)))
   for (id need) in (elpaca--dependencies e)
   for min = (version-to-list need)
   for datep = (> (car min) elpaca--date-version-schema-min) ;; YYYYMMDD version.
   for dep = (elpaca-alist-get id queued)
   for core = (unless dep (elpaca-alist-get id package--builtin-versions))
   for version = (cond (core (if datep elpaca-core-date core))
                       ((memq id elpaca-ignored-dependencies) 'skip)
                       ((null dep) (cl-return (elpaca--fail e (format "Missing dependency %s" id))))
                       (datep (version-to-list (elpaca--commit-date dep)))
                       (t (version-to-list (or (elpaca--declared-version dep) "999"))))
   unless (eq version 'skip)
   when (or (and core (version-list-< version min))
            (and (version-list-< version min)
                 (let ((tag (elpaca-latest-tag dep)))
                   (or (null tag) (version-list-< (version-to-list tag) min)))))
   do (cl-return (elpaca--fail e (format "%s installed version %s lower than min required %s"
                                         id version need))))
  (elpaca--continue-build e))
;; Block until current queue processed.
(elpaca-wait)


;;;; Benchmark [not-used]
;; (use-package benchmark-init
;;   :demand t
;;   :config
;;   (add-hook '+my/first-input-hook 'benchmark-init/deactivate))

;;; Helper Functions
;;;###autoload
(defun +my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;;;###autoload
(defun +my/imenu ()
  "Consult-outline in `org-mode' unless imenu."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (consult-org-heading)
    (consult-imenu)))

;;;###autoload
(defun +my/rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory (file-name-nondirectory buffer-file-name)))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      (kill-buffer)
      (find-file file-to))))

;;;###autoload
(defun +my/delete-file ()
  "Put current buffer file to top."
  (interactive)
  (delete-file
   (read-file-name "Delete: " default-directory (file-name-nondirectory buffer-file-name)))
  (unless (file-exists-p (buffer-file-name))
    (kill-current-buffer)))


;;;###autoload
(defun +my/open-recent ()
  "Open recent directory in Dired or file otherwise."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let* ((candidates (if (derived-mode-p 'dired-mode)
                         (delete-dups
                          (append (mapcar 'file-name-directory recentf-list))
                          ;; (append (mapcar (lambda (fname) (string-join (butlast (string-split fname "/")) "/")) recentf-list))
                          )
                       (mapcar #'abbreviate-file-name
                               ;; (-filter (lambda (filename) (not (file-directory-p filename)))
                               (-filter (lambda (filename) (not (string= "/" (substring filename -1))))
                                        recentf-list)))))
    (find-file
     (consult--read
      candidates
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history
      ))))

;;;###autoload
(defun +my/project-root (&optional dir)
  "Return the project root of DIR."
  (when-let* ((default-directory (or dir default-directory))
              (project (project-current)))
    (expand-file-name (if (fboundp 'project-root)
                          (project-root project)
                        (cdr project)))))

;;;###autoload
(defun +my/save-file ()
  "Save files including org agenda."
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (org-save-all-org-buffers)
    (save-buffer)))

;;;###autoload
(defvar +my/profiler--started nil)
(defun +my/profiler-toggle ()
  "Start ro stop profiler."
  (interactive)
  (if +my/profiler--started
      (progn
        (profiler-stop)
        (profiler-report)
        (setq +my/profiler--started nil))
    (profiler-start 'cpu+mem)
    (setq +my/profiler--started t)))


;;;###autoload
(defun +my/google-it (&optional word)
  "Google WORD."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  (thing-at-point 'symbol))))
  (browse-url (concat "https://www.google.com/search?q=" word)))

;;;###autoload
(defun +my/replace (&optional word)
  "Make it eary to use `:%s' to replace WORD."
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'symbol))))
  (let* ((word (replace-regexp-in-string "\\\\" "\\\\\\\\" word))
         (word (replace-regexp-in-string "/" "\\\\/" word)))
    (evil-ex (concat "%s/" word "/" word))))

(defun +my/open-in-osx-finder (file)
  "Open FILE in `Finder'."
  (interactive "GFile: ")
  (let* ((file (expand-file-name file))
         (script (concat
	              "set thePath to POSIX file \"" file "\"\n"
	              "tell application \"Finder\"\n"
	              " set frontmost to true\n"
	              " reveal thePath \n"
	              "end tell\n")))
    (start-process "osascript-getinfo" nil "osascript" "-e" script)))

(defun +my/quick-look (&optional file)
  "Open FILE with quick look."
  (interactive
   (list
    (if (derived-mode-p 'dired-mode)
        (car (dired-get-marked-files))
      (read-file-name "File:" default-directory buffer-file-name))))
  (call-process-shell-command (concat "qlmanage -p \"" (expand-file-name file) "\"")))


(defun +my/org-datetree-find-date-create (&optional time)
  "Find or create date tree for TIME."
  (interactive)
  (let* ((time (or time (current-time)))
         (goal-text "Daily Goals")
         (archived-text "Archived")
         (arg-time (decode-time time))
         (today-time (decode-time (current-time)))
         (same-day (and (= (nth 3 arg-time) (nth 3 today-time))
                        (= (nth 4 arg-time) (nth 4 today-time))
                        (= (nth 5 arg-time) (nth 5 today-time))))
         (has-goal nil)
         (has-archived nil)
         (archived-point nil))
    (org-reverse-datetree-goto-date-in-file time)
    (org-fold-show-children)
    (if (org-goto-first-child)
        (progn
          (while (progn
                   (let ((heading (org-element-property :title (org-element-at-point))))
                     (cond
                      ((string= heading goal-text)
                       (setq has-goal t)
                       (when same-day (org-toggle-tag "ACT_TODAY" 'on)))
                      ((string= heading archived-text)
                       (setq has-archived t)
                       (setq archived-point (point)))))
                   (org-goto-sibling)))
          (unless has-goal
            (org-insert-heading nil)
            (insert goal-text)
            (when same-day (org-toggle-tag "ACT_TODAY" 'on)))
          (unless has-archived
            (org-insert-heading nil)
            (insert archived-text)
            (setq archived-point (point))))
      (org-insert-subheading nil)
      (insert goal-text)
      (when same-day (org-toggle-tag "ACT_TODAY" 'on))
      (org-insert-heading-after-current)
      (insert archived-text)
      (setq archived-point (point)))
    (goto-char archived-point)
    (end-of-line)
    (point)))

(defun +my/auto-act-today-and-current-week ()
  (let ((act-today-points '())
        (act-month-points '())
        (act-week-points '())
        (act-month-tag ":ACT_MONTH:")
        (act-today-tag ":ACT_TODAY:")
        (act-week-tag ":ACT_WEEK:"))
    (org-map-entries
     (lambda ()
       (let ((tag-string (car (last (org-heading-components)))))
         (when tag-string
           (when (string= tag-string act-today-tag)
             (push (point) act-today-points))
           (when (string= tag-string act-week-tag)
             (push (point) act-week-points))
           (when (string= tag-string act-month-tag)
             (push (point) act-month-points))))))

    (when (length> act-month-points 1)
      (dolist (p (cdr (reverse act-month-points)))
        (save-excursion
          (goto-char p)
          (org-toggle-tag (substring act-month-tag 1 -1)))))
    (when (length> act-today-points 1)
      (dolist (p (cdr (reverse act-today-points)))
        (save-excursion
          (goto-char p)
          (org-toggle-tag (substring act-today-tag 1 -1)))))
    (when (length> act-week-points 1)
      (dolist (p (cdr (reverse act-week-points)))
        (save-excursion
          (goto-char p)
          (org-toggle-tag (substring act-week-tag 1 -1)))))
    ))


(defvar project--ignore-list
  '("/opt/homebrew/"))

(defun my-project--ignored-p (path)
  (when path
    (catch 'found
      (dolist (ignore project--ignore-list)
        (when (string-prefix-p (file-truename ignore) (file-truename path))
          (throw 'found t))))))

;;; Global Configuration
;; UTF8Coding
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

(setq default-directory (concat (getenv "HOME") "/"))

;; Replace selection on insert
(delete-selection-mode 1)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; When buffer is closed, saves the cursor location
(save-place-mode 1)

;; Set history-length longer
(setq-default history-length 500)
;; -History

;; SmallConfigs
;; Turn Off Cursor Alarms
(setq ring-bell-function 'ignore)

;; Show Keystrokes in Progress Instantly
(setq echo-keystrokes 0.1)

;; Don't Lock Files
(setq create-lockfiles nil)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; Better Compilation
(setq-default compilation-always-kill t) ; kill compilation process before starting another

(setq-default compilation-ask-about-save nil) ; save all buffers on `compile'

(setq-default compilation-scroll-output t)

;; ad-handle-definition warnings are generated when functions are redefined with `defadvice',
;; they are not helpful.
(setq ad-redefinition-action 'accept)

;; Move Custom-Set-Variables to Different File
(setq custom-file (concat user-emacs-directory "custom-set-variables.el"))
(load custom-file 'noerror)

;; So Long mitigates slowness due to extremely long lines.
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; for long line
(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; Add a newline automatically at the end of the file upon save.
(setq require-final-newline t)

;; Default .args, .in, .out files to text-mode
(add-to-list 'auto-mode-alist '("\\.in\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.out\\'" . text-mode))
(add-to-list 'auto-mode-alist '("\\.args\\'" . text-mode))
;; -SmallConfigs

;; Sync my code on save
(defmacro η (fnc)
  "Return function that ignores its arguments and invokes FNC."
  `(lambda (&rest _rest)
     (funcall ,fnc)))
(when +self/use-rc-to-sync
  (advice-add #'save-buffer :after (η
                                    (lambda ()
                                      (when (or
                                             (derived-mode-p 'prog-mode)
                                             (derived-mode-p 'yaml-ts-mode))
                                        (call-process-shell-command (concat "rs " (buffer-file-name)) nil 0)
                                        ;; (call-process-shell-command "rs" nil 0)
                                        (call-process-shell-command "rc" nil 0))))))

;; _ as part of a word
(modify-syntax-entry ?_ "w")
(defalias 'forward-evil-word 'forward-evil-symbol)

;; Don't ping things that look like domain names.
(setq command-line-ns-option-alist nil)

(setq vc-follow-symlinks t)


;; Disable message for some functions
(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))
(advice-add 'save-buffer :around 'suppress-message-advice-around)

(defun filter-command-error-function (data context caller)
  "Ignore the `'buffer-read-only', `beginning-of-line', `end-of-line', `beginning-of-buffer', `end-of-buffer' signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-line
                                end-of-line
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'filter-command-error-function)

;; (setq-default which-func-modes '(emacs-lisp-mode python-mode python-ts-mode org-mode latex-mode))
;; (which-function-mode +1)

(use-package breadcrumb
  :ensure (:host github :repo "joaotavora/breadcrumb")
  ;;  FIXME: it is slow..
  :hook ((prog-mode org-mode LaTeX-mode) . breadcrumb-local-mode)
  )

(use-package server
  :ensure nil
  :defer 5
  :commands (server-running-p)
  :config
  (or (server-running-p) (server-start)))

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; Open new file in original frame
(setq ns-pop-up-frames nil)

;;; Evil
;;;; Evil configuration
(use-package evil
  :hook (elpaca-after-init . evil-mode)
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-want-keybinding 'nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  ;; (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Allows you to click buttons without initiating a selection
  (define-key evil-motion-state-map [down-mouse-1] nil)

  (with-eval-after-load 'general
    (general-define-key :keymaps 'evil-window-map
                        "C-h" 'evil-window-left
                        "C-j" 'evil-window-down
                        "C-k" 'evil-window-up
                        "C-l" 'evil-window-right))
  )

;;;; Evil related packages

(use-package evil-embrace
  :defer 1
  :after evil
  :commands (embrace-commander embrace-add-pair embrace-add-pair-regexp)
  :config
  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]"))
  (global-evil-surround-mode)
  (evil-embrace-enable-evil-surround-integration)
  )


(use-package evil-escape
  :after evil
  :hook (+my/first-input . evil-escape-mode)
  :commands (evil-escape-pre-command-hook)
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  (add-hook 'pre-command-hook 'evil-escape-pre-command-hook)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  )



(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter
             evilnc-comment-or-uncomment-lines
             evilnc-copy-and-comment-lines))

;; for visualization like substitute
(use-package evil-traces
  :after evil-ex
  :hook (+my/first-input . evil-traces-mode))

;; Allows you to use the selection for * and #
(use-package evil-visualstar
  :after evil
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


(use-package evil-collection
  :defer nil
  :after evil
  :init
  (setq evil-want-keybinding nil)
  :config
  (let ((modes '(atomic-chrome calc calendar consult debug devdocs diff-hl diff-mode dired doc-view ebib edebug ediff eglot eldoc elisp-mode eval-sexp-fu evil-mc flymake  git-timemachine grep help helpful buffer image image-dired image+ imenu imenu-list (indent "indent")  info log-view man (magit magit-repos magit-submodule) magit-section markdown-mode mu4e mu4e-conversation org (pdf pdf-view) popup proced (process-menu simple) profiler replace sh-script shortdoc so-long tab-bar tablist tabulated-list tar-mode thread timer-list vc-annotate vc-dir vc-git vdiff vertico view vterm vundo wdired wgrep which-key xref yaml-mode (ztree ztree-diff ztree-dir))))
    (evil-collection-init modes))
  )

;; indent textobj
(use-package evil-indent-plus
  :after evil
  :hook (+my/first-input . evil-indent-plus-default-bindings)
  :commands (evil-indent-plus-default-bindings))
;; in/decrease number
;; (use-package evil-numbers)

(use-package evil-anzu
  :after evil
  :after-call evil-ex-search-next
  :config
  (global-anzu-mode)
  (add-hook 'evil-insert-state-entry-hook #'evil-ex-nohighlight)
  )

;;; Search
;;;; English

;;  FIXME: hang
(use-package maple-translate
  :disabled
  :ensure (:host github :repo "honmaple/emacs-maple-translate")
  :commands (maple-translate maple-translate+ maple-translate-posframe)
  :custom
  (maple-translate-buffer " *maple-translate* ")
  :config
  ;; (setq maple-translate-engine '(google dictcn youdao))
  (setq maple-translate-engine '(google))

  ;; with google translate
  (setq maple-translate-google-url "https://translate.googleapis.com/translate_a/single")


  (defun maple-translate-posframe-tip (result)
    "Show STRING using posframe-show."
    (unless (and (require 'posframe nil t) (posframe-workable-p))
      (error "Posframe not workable"))

    (if result
        (progn
          (with-current-buffer (get-buffer-create maple-translate-buffer)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert result)
              (maple-translate-mode)
              (goto-char (point-min))))
          (posframe-show maple-translate-buffer
                         :left-fringe 8
                         :right-fringe 8
                         :internal-border-color (face-foreground 'default)
                         :internal-border-width 1)
          (unwind-protect
              (push (read-event) unread-command-events)
            (progn
              (posframe-hide maple-translate-buffer))))
      (message "Nothing to look up")))

  (defun maple-translate-posframe(word)
    "Translate WORD and display result in posframe."
    (interactive (list (maple-translate-word)))
    (maple-translate-show word 'maple-translate-posframe-tip)))

(use-package go-translate
  ;; :bind (("C-c g"   . gt-do-translate)
  ;;        ("C-c G"   . gt-do-translate-prompt)
  ;;        ("C-c u"   . gt-do-text-utility)
  ;;        ("C-c d g" . gt-do-translate)
  ;;        ("C-c d G" . gt-do-translate-prompt)
  ;;        ("C-c d p" . gt-do-speak)
  ;;        ("C-c d s" . gt-do-setup)
  ;;        ("C-c d u" . gt-do-text-utility))
  :commands (gt-do-translate-prompt gt-do-text-utility)
  :init
  (setq gt-langs '(en zh)
        gt-buffer-render-follow-p t
        gt-buffer-render-window-config
        '((display-buffer-reuse-window display-buffer-in-direction)
          (direction . bottom)
          (window-height . 0.4)))

  (setq gt-pop-posframe-forecolor (face-foreground 'tooltip nil t)
        gt-pop-posframe-backcolor (face-background 'tooltip nil t))
  (when (facep 'posframe-border)
    (setq gt-pin-posframe-bdcolor (face-background 'posframe-border nil t)))
  :config
  (with-no-warnings
    (setq gt-preset-translators
          `((default . ,(gt-translator
                         :taker   (list (gt-taker :pick nil :if 'selection)
                                        (gt-taker :text 'paragraph :if '(Info-mode help-mode helpful-mode devdocs-mode))
                                        (gt-taker :text 'buffer :pick 'fresh-word
                                                  :if (lambda (translatror)
                                                        (and (not (derived-mode-p 'fanyi-mode)) buffer-read-only)))
                                        (gt-taker :text 'word))
                         :engines (if (display-graphic-p)
                                      (list (gt-bing-engine :if 'not-word)
                                            (gt-youdao-dict-engine :if 'word))
                                    (list (gt-bing-engine :if 'not-word)
                                          (gt-youdao-dict-engine :if 'word)
                                          (gt-youdao-suggest-engine :if 'word)
                                          (gt-google-engine :if 'word)))
                         :render  (list (gt-posframe-pop-render
                                         :if (lambda (translator)
                                               (and (display-graphic-p)
                                                    (not (derived-mode-p 'Info-mode 'help-mode 'helpful-mode 'devdocs-mode))
                                                    (not (member (buffer-name) '("COMMIT_EDITMSG")))))
                                         :frame-params (list :accept-focus nil
                                                             :width 70
                                                             :height 15
                                                             :left-fringe 16
                                                             :right-fringe 16
                                                             :border-width 1
                                                             :border-color gt-pin-posframe-bdcolor))
                                        (gt-overlay-render :if 'read-only)
                                        (gt-insert-render :if (lambda (translator) (member (buffer-name) '("COMMIT_EDITMSG"))))
                                        (gt-buffer-render))))
            (multi-dict . ,(gt-translator :taker (gt-taker :prompt t)
                                          :engines (list (gt-bing-engine)
                                                         (gt-youdao-dict-engine)
                                                         (gt-youdao-suggest-engine :if 'word)
                                                         (gt-google-engine))
                                          :render (gt-buffer-render)))
            (Text-Utility . ,(gt-text-utility :taker (gt-taker :pick nil)
                                              :render (gt-buffer-render)))))

    (defun gt--do-translate (dict)
      "Translate using DICT from the preset tranlators."
      (gt-start (alist-get dict gt-preset-translators)))

    (defun gt-do-translate-prompt ()
      "Translate with prompt using the multiple dictionaries."
      (interactive)
      (gt--do-translate 'multi-dict))

    (defun gt-do-text-utility ()
      "Handle the texts with the utilities."
      (interactive)
      (gt--do-translate 'Text-Utility))))

;;;; Look up

(defun +lookup--run-handler (handler identifier)
  (if (commandp handler)
      (call-interactively handler)
    (funcall handler identifier)))

(defun +lookup--run-handlers (handler identifier origin)
  (message "Looking up '%s' with '%s'" identifier handler)
  (condition-case-unless-debug e
      (let ((wconf (current-window-configuration))
            (result (condition-case-unless-debug e
                        (+lookup--run-handler handler identifier)
                      (error
                       (message "Lookup handler %S threw an error: %s" handler e)
                       'fail))))
        (cond ((eq result 'fail)
               (set-window-configuration wconf)
               nil)
              ((or (get handler '+lookup-async)
                   (eq result 'deferred)))
              ((or result
                   (null origin)
                   (/= (point-marker) origin))
               (prog1 (point-marker)
                 (set-window-configuration wconf)))))
    ((error user-error)
     (message "Lookup handler %S: %s" handler e)
     nil)))

(defun +lookup--jump-to (prop identifier &optional display-fn arg)
  (let* ((origin (point-marker))
         (handlers
          (plist-get (list :definition '+lookup-definition-functions
                           :implementations '+lookup-implementations-functions
                           :type-definition '+lookup-type-definition-functions
                           :references '+lookup-references-functions
                           :documentation '+lookup-documentation-functions
                           :file '+lookup-file-functions)
                     prop))
         (result
          (if arg
              (if-let*
                  ((handler
                    (intern-soft
                     (completing-read "Select lookup handler: "
                                      (delete-dups
                                       (remq t (append (symbol-value handlers)
                                                       (default-value handlers))))
                                      nil t))))
                  (+lookup--run-handlers handler identifier origin)
                (user-error "No lookup handler selected"))
            (run-hook-wrapped handlers #'+lookup--run-handlers identifier origin))))
    (unwind-protect
        (when (cond ((null result)
                     (message "No lookup handler could find %S" identifier)
                     nil)
                    ((markerp result)
                     (funcall (or display-fn #'switch-to-buffer)
                              (marker-buffer result))
                     (goto-char result)
                     result)
                    (result))
          (with-current-buffer (marker-buffer origin)
            (better-jumper-set-jump (marker-position origin)))
          result)
      (set-marker origin nil))))


;;
;; Lookup backends

(autoload 'xref--show-defs "xref")
(defun +lookup--xref-show (fn identifier &optional show-fn)
  (let ((xrefs (funcall fn
                        (xref-find-backend)
                        identifier)))
    (when xrefs
      (let* ((jumped nil)
             (xref-after-jump-hook
              (cons (lambda () (setq jumped t))
                    xref-after-jump-hook)))
        (funcall (or show-fn #'xref--show-defs)
                 (lambda () xrefs)
                 nil)
        (if (cdr xrefs)
            'deferred
          jumped)))))

(defun +lookup-xref-definitions-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-definitions'"
  (condition-case _
      (+lookup--xref-show 'xref-backend-definitions identifier #'xref--show-defs)
    (cl-no-applicable-method nil)))

(defun +lookup-xref-references-backend-fn (identifier)
  "Non-interactive wrapper for `xref-find-references'"
  (condition-case _
      (+lookup--xref-show 'xref-backend-references identifier #'xref--show-xrefs)
    (cl-no-applicable-method nil)))

(defun +lookup-dumb-jump-backend-fn (_identifier)
  "Look up the symbol at point (or selection) with `dumb-jump', which conducts a
project search with ag, rg, pt, or git-grep, combined with extra heuristics to
reduce false positives.
This backend prefers \"just working\" over accuracy."
  (and (require 'dumb-jump nil t)
       (dumb-jump-go)))

(defun +lookup-project-search-backend-fn (identifier)
  (when identifier
    (+consult-ripgrep-at-point (+my/project-root) identifier)
    t))

(defun +lookup-ffap-backend-fn (identifier)
  (require 'ffap)
  (let ((guess
         (cond ((doom-region-active-p)
                (buffer-substring-no-properties
                 (doom-region-beginning)
                 (doom-region-end)))
               ((ffap-guesser))
               ((thing-at-point 'filename t))
               (identifier))))
    (when (and (stringp guess)
               (or (file-exists-p guess)
                   (ffap-url-p guess)))
      (find-file-at-point guess))))

;;
;; Main commands

;;;###autoload
(defun +lookup/definition (identifier &optional arg)
  "Jump to the definition of IDENTIFIER (defaults to the symbol at point).
Each function in `+lookup-definition-functions' is tried until one changes the
point or current buffer. Falls back to dumb-jump, naive
ripgrep/the_silver_searcher text search, then `evil-goto-definition' if
evil-mode is active."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :definition identifier nil arg))
        ((error "Couldn't find the definition of %S" identifier))))

;;;###autoload
(defun +lookup/references (identifier &optional arg)
  "Show a list of usages of IDENTIFIER (defaults to the symbol at point)
Tries each function in `+lookup-references-functions' until one changes the
point and/or current buffer. Falls back to a naive ripgrep/the_silver_searcher
search otherwise."
  (interactive (list (doom-thing-at-point-or-region)
                     current-prefix-arg))
  (cond ((null identifier) (user-error "Nothing under point"))
        ((+lookup--jump-to :references identifier nil arg))
        ((error "Couldn't find references of %S" identifier))))


;;;###autoload
(defun doom-region-active-p ()
  "Return non-nil if selection is active.
Detects evil visual mode as well."
  (declare (side-effect-free t))
  (or (use-region-p)
      (and (bound-and-true-p evil-local-mode)
           (evil-visual-state-p))))


;;;###autoload
(defun doom-region-beginning ()
  "Return beginning position of selection.
Uses `evil-visual-beginning' if available."
  (declare (side-effect-free t))
  (or (and (bound-and-true-p evil-local-mode)
           (markerp evil-visual-beginning)
           (marker-position evil-visual-beginning))
      (region-beginning)))

;;;###autoload
(defun doom-region-end ()
  "Return end position of selection.
Uses `evil-visual-end' if available."
  (declare (side-effect-free t))
  (if (bound-and-true-p evil-local-mode)
      evil-visual-end
    (region-end)))

;;;###autoload
(defun doom-thing-at-point-or-region (&optional thing prompt)
  "Grab the current selection, THING at point, or xref identifier at point.
Returns THING if it is a string. Otherwise, if nothing is found at point and
PROMPT is non-nil, prompt for a string (if PROMPT is a string it'll be used as
the prompting string). Returns nil if all else fails.
NOTE: Don't use THING for grabbing symbol-at-point. The xref fallback is smarter
in some cases."
  (declare (side-effect-free t))
  (cond ((stringp thing)
         thing)
        ((doom-region-active-p)
         (buffer-substring-no-properties
          (doom-region-beginning)
          (doom-region-end)))
        (thing
         (thing-at-point thing t))
        ((require 'xref nil t)
         ;; Eglot, nox (a fork of eglot), and elpy implementations for
         ;; `xref-backend-identifier-at-point' betray the documented purpose of
         ;; the interface. Eglot/nox return a hardcoded string and elpy prepends
         ;; the line number to the symbol.
         (if (memq (xref-find-backend) '(eglot elpy nox))
             (thing-at-point 'symbol t)
           ;; A little smarter than using `symbol-at-point', though in most
           ;; cases, xref ends up using `symbol-at-point' anyway.
           (xref-backend-identifier-at-point (xref-find-backend))))
        (prompt
         (read-string (if (stringp prompt) prompt "")))))


(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-ffap-backend-fn
    +lookup-project-search-backend-fn))

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn))

;;
;; dumb-jump

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project "~/.emacs.d/"
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-quiet t
        dumb-jump-selector 'completing-read)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

;;
;; xref
(use-package xref
  :ensure nil
  :init
  (setq xref-search-program 'ripgrep)
  (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :hook ((xref-after-return xref-after-jump) . recenter))


;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(use-package better-jumper
  :hook (+my/first-input . better-jumper-mode)
  :commands doom-set-jump-a
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (mapcar
   (lambda (fn)
     (advice-add fn :around #'doom-set-jump-a))
   (list #'kill-current-buffer #'+my/imenu #'+my/consult-line
         #'find-file #'+my/consult-line-symbol-at-point #'consult-fd #'consult-ripgrep
         #'+consult-ripgrep-at-point))
  )

(with-eval-after-load 'xref
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; This integration is already built into evil
  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)
  )

(use-package avy

  :commands (avy-goto-char avy-goto-line))

(use-package wgrep
  :demand t
  :after (evil embark)
  :config
  (evil-set-initial-state 'grep-mode 'normal))

;;; Completion
;;;; Vertico

(use-package pinyinlib
  :after orderless
  :after-call +my/first-input-hook-fun
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  )


(use-package embark
  :ensure (embark :files (:defaults "*.el"))
  :after-call +my/first-input-hook-fun
  :after general
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)
   ("C-/" . embark-export)
   ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
   :map embark-file-map
   ("r" . +my/rename-file)
   ("d" . +my/delete-file)
   ("X" . +my/open-in-osx-finder)
   ("SPC" . +my/quick-look)
   :map embark-identifier-map
   (";" . embrace-commander)
   ("D" . xref-find-definitions-other-window)
   :map embark-region-map
   (";" . embrace-commander)
   ("y" . evilnc-copy-and-comment-lines)
   ("V" . diff-hl-show-hunk)
   ("/" . evilnc-comment-or-uncomment-lines)
   ("=" . er/expand-region)
   )
  :custom
  (embark-cycle-key ".")
  (embark-help-key "?")
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;;  HACK: bind will be override by evil
  (general-define-key :states '(normal insert visual emacs)
                      "C-." 'embark-act
                      "M-." 'embark-dwim
                      "C-h B" 'embark-bindings)

  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (which-key--hide-popup-ignore-command)
        (which-key--show-keymap
         (if (eq (plist-get (car targets) :type) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (plist-get (car targets) :type)
                   (embark--truncate-target (plist-get (car targets) :target))
                   (if (cdr targets) "…" "")))
         (if prefix
             (pcase (lookup-key keymap prefix 'accept-default)
               ((and (pred keymapp) km) km)
               (_ (key-binding prefix 'accept-default)))
           keymap)
         nil nil t (lambda (binding)
                     (not (string-suffix-p "-argument" (cdr binding))))))))

  (setq embark-indicators
        '(embark-which-key-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator))

  (defun embark-hide-which-key-indicator (fn &rest args)
    "Hide the which-key indicator immediately when using the completing-read prompter."
    (which-key--hide-popup-ignore-command)
    (let ((embark-indicators
           (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

  (advice-add #'embark-completing-read-prompter
              :around #'embark-hide-which-key-indicator)
  )


(use-package embark-consult
  :demand t
  :after consult)

(use-package vertico
  :ensure (vertico :includes (vertico-quick vertico-repeat vertico-directory) :files (:defaults "extensions/vertico-*.el"))
  :hook (window-setup . vertico-mode)
  :config
  (setq vertico-cycle nil
        vertico-preselect 'first)

  (defun +vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  (define-key vertico-map (kbd "S-SPC") #'+vertico-restrict-to-matches)

  (defun +vertico/jump-list (jump)
    "Go to an entry in evil's (or better-jumper's) jumplist."
    (interactive
     (let (buffers)
       (unwind-protect
           (list
            (consult--read
             ;; REVIEW Refactor me
             (nreverse
              (delete-dups
               (delq
                nil (mapcar
                     (lambda (mark)
                       (when mark
                         (cl-destructuring-bind (path pt _id) mark
                           (let* ((visiting (find-buffer-visiting path))
                                  (buf (or visiting (find-file-noselect path t)))
                                  (dir default-directory))
                             (unless visiting
                               (push buf buffers))
                             (with-current-buffer buf
                               (goto-char pt)
                               (font-lock-fontify-region
                                (line-beginning-position) (line-end-position))
                               (format "%s:%d: %s"
                                       (car (cl-sort (list (abbreviate-file-name (buffer-file-name buf))
                                                           (file-relative-name (buffer-file-name buf) dir))
                                                     #'< :key #'length))
                                       (line-number-at-pos)
                                       (string-trim-right (or (thing-at-point 'line) ""))))))))
                     (cddr (better-jumper-jump-list-struct-ring
                            (better-jumper-get-jumps (better-jumper--get-current-context))))))))
             :prompt "jumplist: "
             :sort nil
             :require-match t
             :category 'jump-list))
         (mapc #'kill-buffer buffers))))
    (if (not (string-match "^\\([^:]+\\):\\([0-9]+\\): " jump))
        (user-error "No match")
      (let ((file (match-string-no-properties 1 jump))
            (line (match-string-no-properties 2 jump)))
        (find-file file)
        (goto-char (point-min))
        (forward-line (string-to-number line)))))

  (use-package vertico-quick
    :after vertico
    :ensure nil
    :ensure nil
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))
  (use-package vertico-repeat
    :after vertico
    :ensure nil
    :ensure nil
    :bind ("C-c r" . vertico-repeat)
    :hook (minibuffer-setup . vertico-repeat-save)
    )
  (use-package vertico-directory
    :ensure nil
    :after vertico
    :after-call +my/first-input-hook-fun
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ;; ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-w" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
  )

;; A few more useful configurations...
(use-package emacs
  :ensure nil
  :init
  (global-auto-revert-mode -1)
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t)

  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'completion))


(use-package consult
  :after-call +my/first-input-hook-fun
  :ensure (:host github :repo "minad/consult")
  :bind (
         ([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         ("<help> a" . consult-apropos)
         ("M-s m" . consult-multi-occur)
         )
  :init
  :config
  (setq consult-preview-key "M-.")
  (setq consult-narrow-key "<")

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; consult-imenu
  (with-eval-after-load 'consult-imenu
    (setq consult-imenu-config '((python-ts-mode :types
                                                 ((?c "Class"    font-lock-type-face)
                                                  (?C "Constant"    font-lock-constant-face)
                                                  (?f "Function"  font-lock-function-name-face)
                                                  (?m "Method"  font-lock-function-name-face)
                                                  (?v "Variable"  font-lock-variable-name-face)))
                                 (latex-mode :types
                                             ((?c "Class"    font-lock-type-face)
                                              (?C "Constant"    font-lock-constant-face)
                                              (?f "Function"  font-lock-function-name-face)
                                              (?m "Method"  font-lock-function-name-face)
                                              (?M "Module"  font-lock-type-face)
                                              (?v "Variable"  font-lock-variable-name-face)))
                                 (emacs-lisp-mode :toplevel "Functions"
                                                  :types ((?f "Functions" font-lock-function-name-face)
                                                          (?m "Macros"    font-lock-function-name-face)
                                                          (?p "Packages"  font-lock-constant-face)
                                                          (?t "Types"     font-lock-type-face)
                                                          (?h "Headings"  font-lock-doc-face)
                                                          (?v "Variables" font-lock-variable-name-face))))))

  (defun +my/consult-set-evil-search-pattern (&optional condition)
    (let ((re
           (cond
            ((eq condition 'rg) (substring (car consult--grep-history) 1)) ;; HACK: assume the history begins with `#'
            ((or t (eq condition 'line)) (car consult--line-history)))))
      (add-to-history 'evil-ex-search-history re)
      (setq evil-ex-search-pattern (list re t t))
      (setq evil-ex-search-direction 'forward)
      (anzu-mode t)))

  (defun +my/consult-line-symbol-at-point ()
    (interactive)
    (evil-without-repeat ;; I use evil always
      (consult-line (thing-at-point 'symbol))
      (+my/consult-set-evil-search-pattern)))

  (defun +my/consult-line ()
    (interactive)
    (evil-without-repeat ;; I use evil always
      (consult-line)
      (+my/consult-set-evil-search-pattern)))

  (setq consult-ripgrep-args
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /   --smart-case --no-heading --with-filename --line-number --search-zip")

  (defun +consult-ripgrep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let* ((s (symbol-at-point)))
                                    (symbol-name s))))
    (consult-ripgrep dir initial))

  ;; HACK add `ignore' according to upstream, wihout meaning
  (defun consult--orderless-regexp-compiler (input type &rest _config)
    (setq input (cdr (orderless-compile input)))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input t str))))
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  ;; Shorten candidates in consult-buffer:
  ;; See: https://emacs-china.org/t/21-emacs-vertico-orderless-marginalia-embark-consult/19683/50
  (defun vmacs-consult--source-recentf-items ()
    (let ((ht (consult--buffer-file-hash))
          file-name-handler-alist ;; No Tramp slowdown please.
          items)
      (dolist (file recentf-list (nreverse items))
        ;; Emacs 29 abbreviates file paths by default, see
        ;; `recentf-filename-handlers'.
        (unless (eq (aref file 0) ?/)
          (setq file (expand-file-name file)))
        (unless (gethash file ht)
          (push (propertize
                 (vmacs-short-filename file)
                 'multi-category `(file . ,file))
                items)))))

  (defun vmacs-short-filename(file)
    "return filename with one parent directory.
/a/b/c/d-> c/d"
    (let* ((file (directory-file-name file))
           (filename (file-name-nondirectory file))
           (dir (file-name-directory file))
           short-name)
      (setq short-name
            (if dir
                (format "%s/%s" (file-name-nondirectory (directory-file-name dir)) filename)
              filename))
      (propertize short-name 'multi-category `(file . ,file))))

  (plist-put consult--source-recent-file
             :items #'vmacs-consult--source-recentf-items)


  (advice-add 'marginalia--annotate-local-file :override
              (defun marginalia--annotate-local-file-advice (cand)
                (marginalia--fields
                 ((marginalia--full-candidate cand)
                  :face 'marginalia-size ))))


  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))

  (setq consult-buffer-sources '(consult--source-buffer consult--source-hidden-buffer consult--source-recent-file))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

  (defun consult-imenu--create-key-name-eglot (prefix item types)
    "Create a key-name with optional prefix and type annotations."
    (let* ((name (copy-sequence (car item)))
           (name-type (get-text-property 0 'breadcrumb-kind name))
           (type (assoc name-type types))
           (pos (consult-imenu--normalize (or (car (get-text-property 0 'breadcrumb-region name)) (cdr item))))
           (key-name (concat (when prefix (concat prefix " ")) name)))

      (when type
        (add-face-text-property (if prefix (1+ (length prefix)) 0) (length key-name)
                                (nth 2 type) 'append key-name)

        (setq key-name (concat (car type) " " key-name))
        (put-text-property 0 (length (car type)) 'consult--type (nth 1 type) key-name))

      (list (cons key-name pos))))


  (defun consult-imenu--flatten-eglot (prefix face list types)
    "Flatten imenu LIST.
PREFIX is prepended in front of all items.
FACE is the item face.
TYPES is the mode-specific types configuration."
    (mapcan
     (lambda (item)
       (if (and (consp item) (stringp (car item)) (integer-or-marker-p (cdr item)))
           (consult-imenu--create-key-name-eglot prefix item types)

         (progn
           (append
            (consult-imenu--create-key-name-eglot prefix item types)

            (let* ((name (concat (car item)))
                   (next-prefix (if prefix (concat prefix "/" name) name)))
              (add-face-text-property 0 (length name)
                                      'consult-imenu-prefix 'append name)

              (consult-imenu--flatten-eglot next-prefix face (cdr item) types))))))
     list))

  (defun my-consult-imenu--compute ()
    "Compute imenu candidates."
    (consult--forbid-minibuffer)
    (let* ((imenu-use-markers t)
           ;; Generate imenu, see `imenu--make-index-alist'.
           (items (imenu--truncate-items
                   (save-excursion
                     (without-restriction
                       (funcall imenu-create-index-function)))))
           (config (cdr (seq-find (lambda (x) (derived-mode-p (car x))) consult-imenu-config))))
      ;; Fix toplevel items, e.g., emacs-lisp-mode toplevel items are functions
      (when-let (toplevel (plist-get config :toplevel))
        (let ((tops (seq-remove (lambda (x) (listp (cdr x))) items))
              (rest (seq-filter (lambda (x) (listp (cdr x))) items)))
          (setq items (nconc rest (and tops (list (cons toplevel tops)))))))
      ;; Apply our flattening in order to ease searching the imenu.
      (let ((fn (if (and (boundp 'eglot--managed-mode) eglot--managed-mode) #'consult-imenu--flatten-eglot #'consult-imenu--flatten)))
        (funcall fn
                 nil nil items
                 (mapcar (pcase-lambda (`(,x ,y ,z)) (list y x z))
                         (plist-get config :types)))
        )
      ))
  (advice-add 'consult-imenu--compute :override #'my-consult-imenu--compute)
  )

(use-package consult-jump-project
  :ensure (consult-jump-project :type git :host github :repo "jdtsmith/consult-jump-project")
  :custom (consult-jump-direct-jump-modes '(dired-mode)))

(use-package consult-dir
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (defun consult-dir--zlua-dirs ()
    "Return list of fasd dirs."
    (reverse
     (mapcar
      (lambda (str) (format "%s/" (car (last (split-string str " ")))))
      (split-string (shell-command-to-string "z -l | tail -n 50") "\n" t))))
  (defvar consult-dir--source-zlua
    `(:name     "z.lua dirs"
                :narrow   ?z
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () (getenv "ZLUA_SCRIPT"))
                :items    ,#'consult-dir--zlua-dirs)
    "Fasd directory source for `consult-dir'.")
  ;; (add-to-list 'consult-dir-sources 'consult-dir--source-zlua t)
  (setq consult-dir-sources '(consult-dir--source-recentf consult-dir--source-zlua consult-dir--source-project))
  )

(use-package consult-git-log-grep
  :after consult
  :commands consult-git-log-grep
  :ensure (:host github :repo "ghosty141/consult-git-log-grep")
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))

;;  TODO: consult-todo-project accept git files
(use-package consult-todo
  :ensure (:host github :repo "theFool32/consult-todo" :branch "dev")
  :demand t
  :config
  (when *rg*
    ;;  HACK: speed up with `ripgrep'
    ;; (grep-apply-setting 'grep-find-template "find <D> <X> -type f <F> -exec rg <C> --no-heading -H  <R> /dev/null {} +")
    (grep-apply-setting 'grep-template "rg --no-heading -H <R> <D>")

    (defun consult-todo-dir (&optional directory files)
      "Jump to hl-todo keywords in FILES in DIRECTORY.
If optinal arg FILES is nil, search in all files.
If optional arg DIRECTORY is nil, rgrep in default directory."
      (interactive)
      (let* ((files (or files "* .*"))
             (directory (or directory default-directory)))
        (add-hook 'compilation-finish-functions #'consult-todo--candidates-rgrep)
        (cl-letf ((compilation-buffer-name-function
                   (lambda (&rest _) (format "*consult-todo-%s*" directory))))
          (save-window-excursion
            ;;  HACK: use `lgrep' instead `find'
            (lgrep (concat  "\\b" (replace-regexp-in-string "\\\\[<>]*" "" (hl-todo--regexp)) "\\b") files directory)
            ))))
    )
  )


(use-package orderless
  :after-call elpaca-after-init-hook
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (flex styles basic partial-completion)))))

(use-package marginalia
  :hook (+my/first-input . marginalia-mode)
  :config
  (add-to-list 'marginalia-prompt-categories '("Open org files:" . project-file))
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :init
  (setq vertico-posframe-parameters
        '((min-width . 80)
          (min-height . 15)
          (left-fringe . 8)
          (right-fringe . 8)))
  ;;  HACK: To fix https://github.com/tumashu/vertico-posframe/issues/12
  (add-hook 'minibuffer-exit-hook #'vertico-posframe-cleanup)
  )

;;;; Code Completion
(use-package corfu
  :ensure (corfu :includes (corfu-indexed corfu-quick corfu-popupinfo corfu-history) :files (:defaults "extensions/corfu-*.el"))
  ;; :hook (+my/first-input . global-corfu-mode)
  :custom
  (global-corfu-modes '((not dape-repl-mode) t))
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.01)
  (corfu-echo-documentation 0.3)
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  (corfu-on-exact-match 'quit)
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  :init
  (global-corfu-mode)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("C-n" . corfu-next)
        ("C-j" . corfu-insert)
        ("S-SPC" . corfu-insert-separator)
        ("S-TAB" . corfu-previous)
        ("C-p" . corfu-previous)
        ([?\r] . newline)
        ([backtab] . corfu-previous))
  :config
  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)


  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after (lambda (&rest _) (evil-normalize-keymaps)))
  (advice-add 'corfu--teardown :after (lambda (&rest _) (evil-normalize-keymaps)))
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (use-package corfu-quick
    :ensure nil
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)))

  (use-package corfu-popupinfo
    :ensure nil
    :config
    (setq corfu-popupinfo-delay '(0.2 . 0.1))
    (set-face-attribute 'corfu-popupinfo nil :height 140)
    :hook (corfu-mode . corfu-popupinfo-mode))

  (use-package corfu-history
    :ensure nil
    :hook (corfu-mode . corfu-history-mode))

  ;; allow evil-repeat
  ;; https://github.com/minad/corfu/pull/225
  (defun corfu--unread-this-command-keys ()
    (when (> (length (this-command-keys)) 0)
      (setq unread-command-events (nconc
                                   (listify-key-sequence (this-command-keys))
                                   unread-command-events))
      (clear-this-command-keys t)))

  (cl-defmethod corfu--insert :around (status)
    (if (or (eq this-command 'corfu-insert-exact)
            (not (eq status 'exact)))
        (cl-call-next-method)
      (corfu--unread-this-command-keys)
      (setq this-command 'corfu-insert-exact)))

  (defun corfu-insert-exact ()
    "Insert current candidate with the `exact' status.
Quit if no candidate is selected."
    (interactive)
    (if (>= corfu--index 0)
        (corfu--insert 'exact)
      (corfu-quit)))

  (mapc #'evil-declare-ignore-repeat
        '(corfu-next
          corfu-previous
          corfu-first
          corfu-last))

  (mapc #'evil-declare-change-repeat
        '(corfu-insert
          corfu-insert-exact
          corfu-complete))
  )

(use-package tempel
  :after corfu
  :after-call +my/first-input-hook-fun
  :ensure (:host github :repo "minad/tempel")
  :config
  (defun my/tempel-expand-or-next ()
    "Try tempel expand, if failed, try copilot expand."
    (interactive)
    (if tempel--active
        (tempel-next 1)
      (call-interactively #'tempel-expand)))
  (with-eval-after-load 'general
    (general-define-key
     :keymaps '(evil-insert-state-map)
     "C-k" 'my/tempel-expand-or-next)))

(use-package cape
  :after (corfu tempel)
  :bind (("C-x C-f" . cape-file)
         ("C-x C-l" . cape-line))
  :hook (((org-mode emacs-lisp-mode) . my/set-basic-capf)
         ((lsp-completion-mode eglot-managed-mode lsp-bridge-mode lspce-mode). my/set-lsp-capf))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     ;; (cape-capf-buster
     (if +self/use-tabnine
         (cape-capf-super
          arg-capf
          #'tabnine-capf)
       (cape-capf-super
        arg-capf)
       )
     ;; 'equal)
     #'tmux-capf
     ;; #'cape-dabbrev
     ;; #'eng-capf
     ;; #'cape-dict
     ))

  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (let ((f (car (last completion-at-point-functions 2))))
      (when (functionp f)
        (setq-local completion-at-point-functions (my/convert-super-capf f)))))

  (defun my/set-lsp-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf
                                               'lsp-capf)))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  )

(use-package corfu-english-helper
  :after cape
  :bind (("C-x C-e" . eng-capf))
  :commands (corfu-english-helper-search)
  :defer t
  :ensure (:host github :repo "manateelazycat/corfu-english-helper")
  :config
  (fset 'eng-capf (cape-capf-interactive #'corfu-english-helper-search))
  )

(use-package tabnine-capf
  :if +self/use-tabnine
  :after cape
  :commands (tabnine-capf tabnine-capf-start-process)
  :ensure (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh" "*.py"))
  :hook ((kill-emacs . tabnine-capf-kill-process))
  :config
  (defalias 'tabnine-capf 'tabnine-completion-at-point))

(use-package tmux-capf
  :ensure (:host github :repo "theFool32/tmux-capf" :files ("*.el" "*.sh"))
  :bind (("C-x C-t" . tmux-capf))
  :after cape
  :commands tmux-capf)


;;; Utils
;;;; misc

(use-package recentf
  :ensure nil
  :hook (elpaca-after-init . recentf-mode)
  :custom
  ;; (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 2000)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,+self/org-base-dir
                     ,(expand-file-name "~\/.mail\/*")
                     ;; "^/\\(?:ssh\\|scp\\|su\\|sudo\\)?:"
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "COMMIT_EDITMSG\\'"))
  :config
  (defun recentd-track-opened-file ()
    "Insert the name of the directory just opened into the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-add-file default-directory))
    ;; Must return nil because it is run from `write-file-functions'.
    nil)

  (defun recentd-track-closed-file ()
    "Update the recent list when a dired buffer is killed.
That is, remove a non kept dired from the recent list."
    (and (derived-mode-p 'dired-mode) default-directory
         (recentf-remove-if-non-kept default-directory)))

  (add-hook 'dired-after-readin-hook 'recentd-track-opened-file)
  (add-hook 'kill-buffer-hook 'recentd-track-closed-file)

  (defun recentf-keep-tramp-predicate (file)
    "Return non-nil if FILE should be kept in the recent list.
It handles the case of remote files as well."
    (cond
     ((file-remote-p file))
     ((file-readable-p file))))
  (custom-set-variables '(recentf-keep '(recentf-keep-tramp-predicate)))

  (defun do-recentf-cleanup ()
    "Clean up not existed files for recentf"
    (interactive)
    (let ((recentf-keep '(recentf-keep-default-predicate)))
      (recentf-cleanup)))
  )

(use-package sudo-edit
  :commands (sudo-edit))

(use-package gcmh
  :hook (elpaca-after-init . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

(use-package restart-emacs
  :commands restart-emacs)

(use-package atomic-chrome
  :defer
  :commands (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist
	    '(("overleaf\\.com" . LaTeX-mode))))

(use-package tramp
  :defer 60
  :ensure nil
  :config
  (setq remote-file-name-inhibit-locks t
      tramp-use-scp-direct-remote-copying t
      remote-file-name-inhibit-auto-save-visited t)

  (setq tramp-completion-use-auth-sources nil
        tramp-copy-size-limit (* 1024 1024)
        tramp-verbose 0
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil)
  (setq remote-file-name-inhibit-cache nil
        vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

    (connection-local-set-profile-variables
    'remote-direct-async-process
    '((tramp-direct-async-process . t)))

    (connection-local-set-profiles
    '(:application tramp :protocol "scp")
    'remote-direct-async-process)

    (setq magit-tramp-pipe-stty-settings 'pty)
  )

(use-package vundo
  :ensure (:host github :repo "casouri/vundo")
  :commands vundo
  :defer t
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O))

(use-package super-save
  :ensure (:host github :repo "theFool32/super-save")
  :hook (window-setup . super-save-mode)
  :init
  (setq auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (add-to-list 'super-save-triggers 'switch-to-buffer)
  (add-to-list 'super-save-triggers 'eglot-rename)
  (add-to-list 'super-save-triggers 'consult-buffer)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 0.4)
  (setq save-silently t)
  (setq super-save-auto-save-when-idle t)
  (add-to-list 'super-save-predicates (lambda () (not (and (featurep 'tempel) tempel--active))))
  (add-to-list 'super-save-predicates (lambda () (not (and (boundp 'corfu--frame) (frame-live-p corfu--frame) (frame-visible-p corfu--frame)))))
  (add-to-list 'super-save-predicates (lambda () (not (and (boundp 'rime--preedit-overlay) rime--preedit-overlay))))

  (defun +super-save-without-format (&optional idle-trigger)
    (let ((before-save-hook (remove 'format-all--buffer-from-hook before-save-hook)))
      (when (super-save-p)
        (save-all-buffers))))
  (advice-add 'super-save-command :override '+super-save-without-format))


(use-package ztree
  :commands ztree-diff)

(use-package winner
  :ensure nil
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (elpaca-after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package tab-bar
  :ensure nil
  :ensure nil
  :commands (tab-new tab-bar-rename-tab tab-bar-close-tab tab-bar-select-tab-by-name)
  ;; :hook (elpaca-after-init . tab-bar-mode)
  :config
  (setq tab-bar-show nil))

(use-package ace-window
  :commands ace-window
  :config
  (set-face-attribute 'aw-leading-char-face nil :height 400)
  )

(use-package imenu-list
  :commands (imenu-list imenu-list-smart-toggle))

;;; Tools
;;;; Dired
;; DiredPackage
(use-package dired
  ;; :after-call +my/first-input-hook-fun
  :commands (dired dirvish)
  :ensure nil
  :bind
  (:map dired-mode-map
        ("C-q" . evil-avy-goto-line))
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  :config
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first")
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "'" '+my/quick-look
                        "l" 'dired-find-alternate-file
                        "h" 'dired-up-directory)
    )
  )

;; Colourful dired
(use-package diredfl
  :hook (dired-mode . diredfl-mode))

;; Extra Dired functionality
(use-package dired-x
  :ensure nil
  :after dired
  :config
  (setq dired-omit-files
        (concat dired-omit-files
                "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*"))
  )

(use-package dirvish  ;; `(' for details.
  :ensure (dirvish :type git :host github :repo "alexluigit/dirvish")
  :hook ((+my/first-input . dirvish-override-dired-mode)
         (evil-collection-setup . (lambda (&rest a)
                                    (evil-define-key '(normal) dired-mode-map
                                      (kbd "C-c f") 'dirvish-fd
                                      "i" 'wdired-change-to-wdired-mode
                                      "q" 'dirvish-quit
                                      "." 'dired-omit-mode
                                      "c" 'dirvish-yank-menu
                                      "?" 'dirvish-dispatch
                                      "s" 'dirvish-narrow ;;use `revert-buffer' (gr) to restore
                                      (kbd "TAB") 'dirvish-subtree-toggle
                                      (kbd "M-m") 'dirvish-setup-menu
                                      (kbd "M-t") 'dirvish-layout-toggle
                                      "*"   'dirvish-mark-menu
                                      "f"   'dirvish-file-info-menu
                                      [remap dired-sort-toggle-or-edit] 'dirvish-quicksort
                                      [remap dired-do-redisplay] 'dirvish-ls-switches-menu
                                      [remap dired-summary] 'dirvish-dispatch
                                      ;; [remap dired-do-copy] 'dirvish-yank-menu
                                      [remap mode-line-other-buffer] 'dirvish-history-last))))
  :after dired

  :custom
  (dirvish-mode-line-format ; it's ok to place string inside
   '(:left (sort file-time " " file-size symlink) :right (omit yank index)))
  (dirvish-attributes '(nerd-icons file-size vc-state git-msg))
  (dirvish-subtree-state-style 'nerd)
  (dirvish-side-follow-buffer-file t)
  ;; (dirvish-enabled-features-on-remote '(extras vc))
  :config
  (setq dirvish-open-with-programs
        '((("doc" "docx" "odt" "ods" "xls" "rtf" "xlsx" "odp" "ppt" "pptx" "pdf") . ("open" "%f"))))
  (setq dired-recursive-deletes 'always)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-listing-switches
        "-l --almost-all --human-readable --time-style=long-iso --group-directories-first --no-group")
  )

(use-package dirvish-extras
  :ensure nil
  :after dirvish
  )


(use-package casual
  :ensure (:host github :repo "kickingvegas/casual" :files ("lisp/*.el"))
  :commands (casual-dired-tmenu casual-dired-sort-by-tmenu)
  :init
  (require 'casual-dired)
  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "C-o" #'casual-dired-tmenu)))

;; SaveAllBuffers
;;;###autoload
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-bffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(with-eval-after-load 'general
  (general-def "C-x C-s" nil)
  (general-def "C-x C-s" 'save-all-buffers))
;; -SaveAllBuffers

;;;; Magit

(use-package transient)

;; MagitPac
(use-package magit
  :defer t
  :commands (magit magit-open-repo aborn/simple-git-commit-push)
  :if *git*
  :config
  ;; (global-auto-revert-mode -1)
  ;; (magit-auto-revert-mode -1)
  (defvar +magit-open-windows-in-direction 'right
    "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")
  (defun +magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...
- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                '(display-buffer-same-window))

               ('(+magit--display-buffer-in-direction))))))

  (defun +magit--display-buffer-in-direction (buffer alist)
    "`display-buffer-alist' handler that opens BUFFER in a direction.
This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
    (let ((direction (or (alist-get 'direction alist)
                         +magit-open-windows-in-direction))
          (origin-window (selected-window)))
      (if-let* ((window (window-in-direction direction)))
          (unless magit-display-buffer-noselect
            (select-window window))
        (if-let* ((window (and (not (one-window-p))
                               (window-in-direction
                                (pcase direction
                                  (`right 'left)
                                  (`left 'right)
                                  ((or `up `above) 'down)
                                  ((or `down `below) 'up))))))
            (unless magit-display-buffer-noselect
              (select-window window))
          (let ((window (split-window nil nil direction)))
            (when (and (not magit-display-buffer-noselect)
                       (memq direction '(right down below)))
              (select-window window))
            (display-buffer-record-window 'reuse window buffer)
            (set-window-buffer window buffer)
            (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
            (set-window-prev-buffers window nil))))
      (unless magit-display-buffer-noselect
        (switch-to-buffer buffer t t)
        (selected-window))))

;;;###autoload
  (defun +magit/quit (&optional kill-buffer)
    "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
    (interactive "P")
    (let ((topdir (magit-toplevel)))
      (funcall magit-bury-buffer-function kill-buffer)
      (or (cl-find-if (lambda (win)
                        (with-selected-window win
                          (and (derived-mode-p 'magit-mode)
                               (equal magit--default-directory topdir))))
                      (window-list))
          (+magit/quit-all))))

;;;###autoload
  (defun +magit/quit-all ()
    "Kill all magit buffers for the current repository."
    (interactive)
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers)))

  (defun +magit--kill-buffer (buf)
    "TODO"
    (when (and (bufferp buf) (buffer-live-p buf))
      (let ((process (get-buffer-process buf)))
        (if (not (processp process))
            (kill-buffer buf)
          (with-current-buffer buf
            (if (process-live-p process)
                (run-with-timer 5 nil #'+magit--kill-buffer buf)
              (kill-process process)
              (kill-buffer buf)))))))
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)
  (setq magit-diff-refine-hunk (quote all))

  (general-define-key :states '(normal)
                      :keymaps 'magit-mode-map
                      "q" #'+magit/quit
                      "Q" #'+magit/quit-all)


  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (if (string-match "^http" url)
                        url
                      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                                "https://\\2/\\3"
                                                url)))
        (message "opening repo %s" url))))

  (defun aborn/simple-git-commit-push ()
    "Simple commit current git project and push to its upstream."
    (interactive)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (magit-stage-modified)
    (magit-diff-staged)
    (setq msg (read-string "Commit Message: "))
    (when (length= msg 0)
      (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S"
                                    (current-time))))
    (magit-call-git "commit" "-m" msg)
    (when (magit-get "remote" "origin" "url")
      (magit-push-current-to-upstream nil)
      (message "now do async push to %s" (magit-get "remote" "origin" "url")))
    (magit-mode-bury-buffer))
  )
;; -MagitPac

(use-package magit-delta
  :if (executable-find "delta")
  :hook (magit-mode . magit-delta-mode))

;; Walk through git revisions of a file
(use-package git-timemachine
  :ensure (:host codeberg :repo "pidu/git-timemachine")
  :after magit
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer"))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow)
  :bind (:map magit-status-mode-map
              ("%" . magit-gitflow-popup)))

(use-package smerge-mode
  :ensure nil
  :commands smerge-mode
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (smerge-mode . evil-normalize-keymaps))
  :config
  (local-leader-def
    :keymaps 'smerge-mode-map
    "n" '(smerge-next :wk "Next conflict")
    "p" '(smerge-prev :wk "Previous conflict")
    "RET" '(smerge-keep-current :wk "Accept current")
    "l" '(smerge-keep-lower :wk "Keep lower")
    "u" '(smerge-keep-upper :wk "Keep upper")
    "m" '(smerge-keep-mine :wk "Keep mine")
    "A" '(smerge-keep-all :wk "Keep all")))

(defvar gitmoji--all-emoji
  '(("增加新特性" . "feat:")
    ("bug 修复" . "fix:")
    ("文档改动" . "docs:")
    ("功能、交互优化" . "improve:")
    ("格式改动（不影响代码运行的变动，例如加空格、换行、分号等）" . "style:")
    ("重构代码" . "refactor:")
    ("性能相关优化" . "perf:")
    ("测试代码" . "test:")
    ("构建过程或辅助工具变动" . "chore:")
    ("回滚" . "revert:")
    ("合并" . "merge:")
    ("上传资源文件" . "resource:")))

(defun gitmoji-picker ()
  "Choose a gitmoji."
  (interactive)
  (let* ((choices gitmoji--all-emoji)
         (candidates (mapcar (lambda (cell)
                               (cons (format "%s — %s" (cdr cell) (car cell)) (concat (cdr cell) " ")))
                             choices)))
    (insert (cdr (assoc (completing-read "Choose a gitmoji " candidates) candidates)))
    (evil-insert-state)))

(use-package blamer
  :ensure (:host github :repo "artawower/blamer.el")
  :bind (("s-i" . blamer-show-posframe-commit-info))
  :commands (blamer-show-posframe-commit-info)
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                   :background unspecified
                   :height 140
                   :italic t))))
;;;; Checker
;; flymake
(use-package flymake
  :ensure nil
  ;; :hook ((prog-mode LaTeX-mode) . flymake-mode)
  :hook ((python-mode python-ts-mode LaTeX-mode) . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil)
  (setq flymake-show-diagnostics-at-end-of-line nil)
  ;; (setq-local flymake-diagnostic-functions nil)
  (setq flymake-fringe-indicator-position 'right-fringe)

  ;; (setq python-flymake-command '("ruff" "--quiet" "--stdin-filename=stdin" "-"))
  )

(use-package flymake-ruff
  :ensure t
  :hook ((python-mode python-ts-mode) . flymake-ruff-load))

(use-package flymake-popon
  :hook (flymake-mode . flymake-popon-mode))

(use-package jinx
  :ensure (:host github :repo "minad/jinx" :files (:defaults "jinx-mod.c" "emacs-module.h"))
  :bind (([remap ispell-word] . #'jinx-correct))
  :hook (LaTeX-mode . jinx-mode)
  :config
  (add-to-list 'jinx-exclude-regexps '(t "\\cc"))
  )

;;;; Ebib
(use-package ebib
  :after (evil-collection org)
  :commands ebib
  :ensure (ebib :includes (org-ebib) :host github :repo "theFool32/ebib")
  :hook (ebib-index-mode . hl-line-mode)
  :custom
  (ebib-file-associations '(("pdf" . "open")))
  (ebib-index-window-size 30)
  (ebib-citation-description-function 'ebib-create-org-title)
  (ebib-preload-bib-files (list (concat +self/ebib-base-dir "ref.bib")))
  (ebib-file-search-dirs (list (concat +self/ebib-base-dir "pdfs/")))
  (ebib-notes-directory (concat +self/ebib-base-dir "notes/"))
  (ebib-keywords (concat +self/ebib-base-dir "ebib-keywords.txt"))
  (ebib-keywords-field-keep-sorted t)
  (ebib-keywords-save-on-exit 'always)
  (ebib-filters-default-file (concat +self/ebib-base-dir "ebib-filters"))
  (ebib-timestamp-format "%Y-%m-%d,%T")
  (ebib-index-default-sort '("timestamp" . descend))
  (ebib-use-timestamp t "recording the time that entries are added")
  (ebib-extra-fields
   '((BibTeX "keywords" "abstract" "timestamp" "read"
             "file"  "url" "crossref" "annote" "doi")))
  (ebib-hidden-fields
   '("volume" "number" "series" "editor" "pages" "address" "month" "organization" "publisher" "crossref"))
  (ebib-index-columns '(("Title" 70 t)
                        ("Author/Editor" 33 t)
                        ("Year" 4 t)
                        ("timestamp" 19 t)
                        ("read" 1 t)
                        ("readtime" 19 t)))
  :bind
  (:map ebib-index-mode-map
        ("/" . ebib-jump-to-entry)
        ("?" . ebib-swiper)
        ;; ("?" . ebib-search)
        ("x" . ebib-delete-entry-with-file)
        ("s"   . ebib-save-all-databases)
        ("S"   . +my/search-pdf)
        ("B"   . ebib-import-ref)
        :map ebib-multiline-mode-map
        ("C-c C-c" . ebib-quit-multiline-buffer-and-save)
        ("C-c C-q" . ebib-cancel-multiline-buffer)
        ("C-c C-s" . ebib-save-from-multiline-buffer)
        :map bibtex-mode-map
        ("C-c C-i" . insert-to-bib))
  :init
  (defun ebib-swiper ()
    (interactive)
    (progn (consult-line) (ebib--update-entry-buffer)))

  ;;  TODO: search only the current pdf
  ;;  TODO: split the files and the contents
  (defun +my/search-pdf ()
    (interactive)
    (consult-ripgrep (concat +self/ebib-base-dir "/pdfs") ""))

  (defun insert-to-bib ()
    (interactive)
    (save-excursion
      (call-interactively 'ebib-jump-to-entry)
      (ebib--execute-when
        (entries
         (let ((key (ebib--get-key-at-point)))
           (with-temp-buffer
             (ebib--format-entry key ebib--cur-db nil nil '("author" "booktitle" "year" "title" "journal"))
             (kill-new (buffer-substring-no-properties (point-min) (point-max))))
           (message (format "Entry `%s' copied to kill ring.  Use `y' to yank (or `C-y' outside Ebib)." key))))
        (default
         (beep))))
    (yank))

  :config
  (require 'org-ebib)
  (local-leader-def
    :keymaps 'org-mode-map
    "lb" 'org-ebib-insert-link)
  )

;;  HACK: lexical-let not work here
(defun ebib-import-ref (url)
  (interactive "sUrl:")
  (let ((buffername (concat "*ref-" (org-id-uuid) "*"))
        (pdf-url url))
    (run-with-idle-timer
     0.1
     nil
     (lambda ()
       (let ((tempbuff (get-buffer-create buffername)))
         (make-process
          :name ""
          :buffer tempbuff
          :command (list "ref_down.py" pdf-url (concat +self/ebib-base-dir "/pdfs"))
          :sentinel (lambda (process event)
                      ;; Render result to content buffer when subprocess finish.
                      (when (string= (substring event 0 -1) "finished")
                        (let ((buffer (process-buffer process)))
                          ;; Do nothing if process buffer has killed.
                          (when (get-buffer buffer)
                            (with-current-buffer buffer
                              (ebib-import-entries)
                              (kill-buffer buffer)
                              (ebib--update-buffers))))))))))))
;;;; Rime
;; karabiner: shift for emacs-rime
;; #+begin_src json
;; {
;;     "description": "Shift for Emacs-Rime",
;;     "manipulators": [
;;         {
;;             "type": "basic",
;;             "from": {
;;                 "key_code": "left_shift",
;;                 "modifiers": {
;;                     "optional": [
;;                         "any"
;;                     ]
;;                 }
;;             },
;;             "to": [
;;                 {
;;                     "key_code": "left_shift",
;;                     "lazy": true
;;                 }
;;             ],
;;             "to_if_alone": [
;;                 {
;;                     "key_code": "backslash",
;;                     "modifiers": ["left_control"]
;;                 }
;;             ],
;;             "conditions": [
;;                 {
;;                 "type": "frontmost_application_if",
;;                 "bundle_identifiers": ["org\\.gnu\\.Emacs"]
;;                 }
;;             ]
;;         }
;;     ]
;; },
;; #+end_src

(use-package rime
  :after-call toggle-input-method
  :if +self/use-rime
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist/"
        rime-user-data-dir "~/.emacs.d/Rime/"
        default-input-method "rime")

  :custom
  (rime-show-candidate 'posframe)
  (rime-posframe-properties (list :font "sarasa ui sc"
                                  :internal-border-width 2))
  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/Applications/Emacs.app/Contents/Resources/include/"))

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)

  (setq rime-disable-predicates '(rime-predicate-evil-mode-p
                                  (lambda ()
                                    (and (rime-predicate-prog-in-code-p)
                                         (not (rime-predicate-in-code-string-p))
                                         ))
                                  (lambda ()
                                    (and (> (point) (save-excursion (back-to-indentation) (point)))
                                         (let ((string (buffer-substring (point) (max (line-beginning-position) (- (point) 80)))))
                                           (string-match-p "[a-zA-Z0-9\./\\<>;:\[\]{}\|!@#\$&\*\(\)\-=\+_,\?\^]$" string))))
                                  ))



  )

;;;; Shell
(when (and module-file-suffix
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands (vterm--internal vterm-posframe-toggle +my/smart-switch-to-vterm-tab)
    :init
    (setq vterm-always-compile-module t)
    (setq vterm-shell "tmux")
    (setq vterm-timer-delay 0.001
          process-adaptive-read-buffering nil)
    :config
    (evil-define-key 'insert vterm-mode-map (kbd "C-c") 'vterm-send-C-c)
    (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm-send-escape)

    (defun +my/smart-switch-to-vterm-tab ()
      "Switch to vterm tab if exists, otherwise create a new vterm tab."
      (interactive)
      (let ((vterm-buffer-name "vterm-tab"))
        (if (get-buffer vterm-buffer-name)
            (progn
              (tab-bar-select-tab-by-name "vterm")
              (switch-to-buffer vterm-buffer-name))
          (tab-new)
          (tab-bar-rename-tab "vterm")
          (call-interactively #'vterm)
          (delete-other-windows))))

    (with-no-warnings
      (defvar vterm-posframe--frame nil)

      (defun vterm-posframe-hidehandler (_)
        "Hidehandler used by `vterm-posframe-toggle'."
        (not (eq (selected-frame) posframe--frame)))

      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let* ((vterm-posframe-buffer-name "vterm-posframe")
               (buffer (or (get-buffer vterm-posframe-buffer-name) (vterm--internal #'ignore vterm-posframe-buffer-name)))
               (width  (max 80 (/ (frame-width) 2)))
               (height (/ (frame-height) 2)))
          (if (and vterm-posframe--frame
                   (frame-live-p vterm-posframe--frame)
                   (frame-visible-p vterm-posframe--frame))
              (progn
                (posframe-hide buffer)
                ;; Focus the parent frame
                (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'vterm-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . 't))
                   :accept-focus t))
            ;; Focus the child frame
            (select-frame-set-input-focus vterm-posframe--frame))))
      )
    (add-hook 'vterm-mode-hook
              (lambda ()
                (set (make-local-variable 'buffer-face-mode-face) '(:family "JetBrains Mono"))
                (buffer-face-mode t))))
  )

(use-package eat
  :ensure (:host codeberg
                 :repo "akib/emacs-eat"
                 :files ("*.el" ("term" "term/*.el") "*.texi"
                         "*.ti" ("terminfo/e" "terminfo/e/*")
                         ("terminfo/65" "terminfo/65/*")
                         ("integration" "integration/*")
                         (:exclude ".dir-locals.el" "*-tests.el")))
  :commands (eat-project eat my-eat-dwim)
  :custom
  (eat-shell "fish")
  :bind
  (("C-0" . #'eat-project)
   ("C-9" . #'my-eat-dwim))
  :init
  (with-eval-after-load 'meow
    (add-to-list 'meow-mode-state-list '(eat-mode . insert)))
  :config
  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (defun my-eat-dwim ()
    (interactive)
    (let ((eat-buffer-name "eat-standalone"))
      (my-create-term-cmd #'eat eat-buffer-name)))

  (defun my-eat-find-file-dwim (filename)
    (interactive)
    (let ((dir default-directory))
      (if (string= (buffer-name) "eat-standalone")
          (progn
            (tab-bar-switch-to-recent-tab)
            (find-file (expand-file-name filename dir)))
        (find-file-other-window filename))))

  (defun my-eat-dired-dwim ()
    (interactive)
    (if (string= (buffer-name) "eat-standalone")
        (progn
          (tab-bar-switch-to-recent-tab)
          (dirvish default-directory))
      (dirvish default-directory)))

  (add-to-list 'eat-message-handler-alist '("find-file" . my-eat-find-file-dwim))
  (add-to-list 'eat-message-handler-alist '("dired" . my-eat-dired-dwim)))


;;;; Mail
(defvar mu-path (format "%s%s" (getenv "MU_PATH") "/share/emacs/site-lisp/mu/mu4e"))
(use-package mu4e
  :ensure nil
  :load-path mu-path
  :if (executable-find "mu")
  :commands mu4e
  :hook ((mu4e-headers-mode . hl-line-mode)
         (mu4e-compose-mode . (lambda () (electric-indent-local-mode -1) (setq-local evil-auto-indent nil))))
  :init
  (provide 'html2text)
  :config
  (setenv "XAPIAN_CJK_NGRAM" "true")

  (setq
   mu4e-change-filenames-when-moving t
   mu4e-hide-index-messages t
   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy nil
   mail-user-agent 'mu4e-user-agent
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-smtp-service 587
   smtpmail-starttls-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-stream-type  'starttls
   ;; mu4e-get-mail-command "offlineimap"
   mu4e-get-mail-command "true"
   mu4e-update-interval nil
   ;; 回复邮件插入邮件引用信息
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
   mu4e-view-show-images t
   mu4e-view-image-max-width 800
   mu4e-compose-signature-auto-include t
   mu4e-compose-dont-reply-to-self t
   mu4e-use-fancy-chars nil
   mu4e-headers-include-related t
   mu4e-headers-skip-duplicates t
   mu4e-completing-read-function 'completing-read
   message-kill-buffer-on-exit t
   mu4e-confirm-quit nil
   mu4e-compose-format-flowed t
   mu4e-view-show-addresses t

   ;; 根据 from 邮件头使用正确的账户上下文发送 Email.
   message-sendmail-envelope-from 'header

   mu4e-maildir "~/.mail"

   mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
   mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
   mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
   mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
   mu4e-headers-thread-child-prefix         '("├>" . "├▶")
   mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
   )

  (setq mu4e-bookmarks
        '( ("flag:unread AND NOT flag:trashed and not (m:/Gmail/Trash OR m:/Gmail/[Gmail]/Trash OR m:/Outlook/Deleted OR m:/QQ/Deleted Messages OR m:/XMU/Trash)"      "Unread messages"   ?u)
           ("m:/Gmail/Inbox or m:/Outlook/Inbox or m:/XMU/Inbox" "Inbox" ?i)
           ("date:today..now"  "Today's messages"   ?t)
           ("date:7d..now"  "Last 7 days"           ?w)
           ("mime:image/*"  "Messages with images"  ?p)
           ("NOT (m:/Gmail/Trash OR m:/Gmail/[Gmail]/Trash OR m:/Outlook/Deleted OR m:/QQ/Deleted Messages OR m:/XMU/Trash)"  "All"  ?a)))

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 4)
          (:maildir . 25)
          (:from . 22)
          (:subject)))


  ;; 该函数基于当前所在的 maildir 来判定所账户上下文。
  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; If rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; Not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  ;; 中文搜索
  (defun mu4e-goodies~break-cjk-word (word)
    "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
    (if (or (<= (length word) 2)
            (equal (length word) (string-bytes word))) ; only ascii chars
        word
      (let ((pos nil)
            (char-list nil)
            (br-word nil))
        (if (setq pos (string-match ":" word))     ; like: "s:abc"
            (concat (substring word 0 (+ 1 pos))
                    (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
          (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abc你好
              word
            (progn
              (setq char-list (split-string word "" t))
              (while (cdr char-list)
                (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
                (setq char-list (cdr char-list)))
              br-word))))))
  (defun mu4e-goodies~break-cjk-query (expr)
    "Break CJK strings into bi-grams in query."
    (let ((word-list (split-string expr " " t))
          (new ""))
      (dolist (word word-list new)
        (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))
  (setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query)

  (defun open-mail-in-browser (&optional mail)
    (interactive)
    (let ((mails '(("Gmail". "https://www.gmail.com")
                   ("Outlook" . "https://www.outlook.com")
                   ("XMU" . "https://stu.xmu.edu.cn"))))
      (browse-url
       (cdr (assoc (completing-read "Mail:" (mapcar 'car mails)) mails)))))
  (general-define-key :states '(normal)
                      :keymaps 'mu4e-main-mode-map
                      "o" #'open-mail-in-browser)
  )



(use-package smtpmail
  :ensure nil
  :config
  (defun fetch-access-token ()
    (with-temp-buffer
      (call-process "mutt_oauth2.py" nil t nil (expand-file-name "~/.mstoken"))
	  (buffer-string)))

   ;;; Add new authentication method for xoauth2
  (cl-defmethod smtpmail-try-auth-method
    (process (_mech (eql xoauth2)) user password)
    (let* ((access-token (fetch-access-token)))
      (smtpmail-command-or-throw
       process
       (concat "AUTH XOAUTH2 "
               (base64-encode-string
                (concat "user=" user "\1auth=Bearer " access-token "\1\1") t)))))

  (add-to-list 'smtpmail-auth-supported 'xoauth2)
  )

;;;; PDF-Tools
(use-package pdf-tools
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :hook (dired-mode . pdf-tools-install)
  :config
  ;; (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t) ; automatically annotate highlights
  (setq pdf-view-resize-factor 1.1) ; more fine-grained zooming
  )


;;;; GPT
(use-package gptel
  :ensure t)

;;  TODO: a better workflow
(use-package aidermacs
  :ensure (:host github :repo "MatthewZMD/aidermacs" :files ("*.el"))
  :config
  (when (boundp 'deepseek-key)
    (setenv "DEEPSEEK_API_KEY" deepseek-key))
  (global-set-key (kbd "C-c a") 'aidermacs-transient-menu))

(use-package claude-code
  :disabled
  :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
                   :files ("*.el" (:exclude "images/*")))
  :bind-keymap
  ("C-c c" . claude-code-command-map)
  :config
  (setq claude-code-terminal-backend 'eat)
  ;; (setq claude-code-program "ccr")
  ;; (setq claude-code-program-switches '("code"))

  (claude-code-mode))


(use-package claude-code-ide
  :ensure (:type git :host github :repo "manzaltu/claude-code-ide.el")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setq claude-code-ide-terminal-backend 'eat)
  (claude-code-ide-emacs-tools-setup))

(use-package minuet
    :bind
    (("M-y" . #'minuet-complete-with-minibuffer) ;; use minibuffer for completion
     ("M-i" . #'minuet-show-suggestion) ;; use overlay for completion
     ("C-c m" . #'minuet-configure-provider)
     :map minuet-active-mode-map
     ;; These keymaps activate only when a minuet suggestion is displayed in the current buffer
     ("M-p" . #'minuet-previous-suggestion) ;; invoke completion or cycle to next completion
     ("M-n" . #'minuet-next-suggestion) ;; invoke completion or cycle to previous completion
     ("M-d" . #'minuet-accept-suggestion) ;; accept
     ;; Accept the first line of completion, or N lines with a numeric-prefix:
     ;; e.g. C-u 2 M-a will accepts 2 lines of completion.
     ("M-a" . #'minuet-accept-suggestion-line)
     ("M-e" . #'minuet-dismiss-suggestion))

    :init
    ;; if you want to enable auto suggestion.
    ;; Note that you can manually invoke completions without enable minuet-auto-suggestion-mode
    ;; (add-hook 'prog-mode-hook #'minuet-auto-suggestion-mode)
    
    )

;;; UI
;;;; UI-config
;; SmoothScroll
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll

;; TitleBar
(setq-default frame-title-format '("EMACS" " - %b"))
;; -TitleBar

(menu-bar-mode 0)

;; DisLineNum
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'(lambda () (display-line-numbers-mode 0)))
;; Display column numbers in modeline
(column-number-mode 1)
(setq display-line-numbers-type 'relative)
(setq display-line-numbers-grow-only t)
;; -DisLineNum

;; DisTimeBat
(setq display-time-format "%m-%d %I:%M"
      display-time-mail-string ""
      display-time-default-load-average nil)
(display-time-mode t)
;; (display-battery-mode 1)
;; -DisTimeBat

;;Font

(when (display-graphic-p)
  ;; (add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-14"))
  ;; (set-face-attribute 'default nil
  ;;                     :font "CaskaydiaCove Nerd Font"
  ;;                     :height 140)
  ;; (set-fontset-font t '(#x4e00 . #x9fff) "Sarasa Mono SC")

  (defun max/set-font (FONT-NAME CN-FONT-NAME &optional INITIAL-SIZE CN-FONT-RESCALE-RATIO)
    "Set different font-family for Latin and Chinese charactors."
    (let* ((size (or INITIAL-SIZE 14))
	       (ratio (or CN-FONT-RESCALE-RATIO 0.0))
	       (main (font-spec :name FONT-NAME :size size))
	       (cn (font-spec :name CN-FONT-NAME)))
      (set-face-attribute 'default nil :font main)
      (dolist (charset '(kana han symbol cjk-misc bopomofo))
        (set-fontset-font t charset cn))
      (setq face-font-rescale-alist (if (/= ratio 0.0) `((,CN-FONT-NAME . ,ratio)) nil))))

  (max/set-font "CaskaydiaCove Nerd Font" "Sarasa Term SC" 14 1.1)
  )

(setq split-width-threshold 0
      split-height-threshold nil)

;; ATIPac
(use-package nerd-icons
  :init
  (setq nerd-icons-scale-factor 1.1))
(use-package nerd-icons-completion
  :ensure (nerd-icons-completion :type git :host github :repo "rainstormstudio/nerd-icons-completion")
  :commands (nerd-icons-completion-marginalia-setup)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup))
(use-package nerd-icons-corfu
  :after (corfu nerd-icons)
  :after-call +my/first-input-hook-fun
  :config
  (setq nerd-icons-corfu-mapping
        `((array :style "cod" :icon "symbol_array" :face font-lock-type-face)
          (boolean :style "cod" :icon "symbol_boolean" :face font-lock-builtin-face)
          (class :style "cod" :icon "symbol_class" :face font-lock-type-face)
          (color :style "cod" :icon "symbol_color" :face success)
          (command :style "cod" :icon "terminal" :face default)
          (constant :style "cod" :icon "symbol_constant" :face font-lock-constant-face)
          (constructor :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (enummember :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum-member :style "cod" :icon "symbol_enum_member" :face font-lock-builtin-face)
          (enum :style "cod" :icon "symbol_enum" :face font-lock-builtin-face)
          (event :style "cod" :icon "symbol_event" :face font-lock-warning-face)
          (field :style "cod" :icon "symbol_field" :face font-lock-variable-name-face)
          (file :style "cod" :icon "symbol_file" :face font-lock-string-face)
          (folder :style "cod" :icon "folder" :face font-lock-doc-face)
          (interface :style "cod" :icon "symbol_interface" :face font-lock-type-face)
          (keyword :style "cod" :icon "symbol_keyword" :face font-lock-keyword-face)
          (macro :style "cod" :icon "symbol_misc" :face font-lock-keyword-face)
          (magic :style "cod" :icon "wand" :face font-lock-builtin-face)
          (method :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (function :style "cod" :icon "symbol_method" :face font-lock-function-name-face)
          (module :style "cod" :icon "file_submodule" :face font-lock-preprocessor-face)
          (numeric :style "cod" :icon "symbol_numeric" :face font-lock-builtin-face)
          (operator :style "cod" :icon "symbol_operator" :face font-lock-comment-delimiter-face)
          (param :style "cod" :icon "symbol_parameter" :face default)
          (property :style "cod" :icon "symbol_property" :face font-lock-variable-name-face)
          (reference :style "cod" :icon "references" :face font-lock-variable-name-face)
          (snippet :style "cod" :icon "symbol_snippet" :face font-lock-string-face)
          (string :style "cod" :icon "symbol_string" :face font-lock-string-face)
          (struct :style "cod" :icon "symbol_structure" :face font-lock-variable-name-face)
          (text :style "cod" :icon "text_size" :face font-lock-doc-face)
          (typeparameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (type-parameter :style "cod" :icon "list_unordered" :face font-lock-type-face)
          (unit :style "cod" :icon "symbol_ruler" :face font-lock-constant-face)
          (value :style "cod" :icon "symbol_field" :face font-lock-builtin-face)
          (variable :style "cod" :icon "symbol_variable" :face font-lock-variable-name-face)
          (tabnine :style "cod" :icon "hubot" :face font-lock-warning-face)
          (unknown :style "cod" :icon "code" :face font-lock-warning-face)
          (t :style "cod" :icon "code" :face font-lock-warning-face)))
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package olivetti
  :ensure (:host github :repo "rnkn/olivetti")
  :commands (olivetti-mode olivetti-shrink olivetti-expand olivetti-set-width)
  :custom
  (olivetti-body-width 120))
;; -ATIPac

(use-package popper
  :defines popper-echo-dispatch-actions
  :hook (window-setup . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Agenda Commands\\*"
          "\\*eldoc\\*"
          "\\*Calendar\\*"
          "\\*Org Links\\*"
          "\\*gt-result\\*"

          bookmark-bmenu-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          devdocs-mode
          ;; occur-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode
          youdao-dictionary-mode multi-translate-mode maple-translate-mode

          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*[Wo]*Man.*\\*$"
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*vc-.*\\*$"
          "^\\*macro expansion\\**"
          "\\*Flymake diagnostics .*\\*$"

          ;; "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          ))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)
  (with-no-warnings
    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


(use-package ef-themes
  :ensure (:host github :repo "protesilaos/ef-themes")
  :init
  ;;  HACK: do not load unused themes
  (dolist (theme '(ef-winter ef-tritanopia-dark ef-trio-dark ef-night ef-duo-dark ef-deuteranopia-dark ef-dark ef-cherie ef-bio ef-autumn ef-tritanopia-light ef-summer ef-spring ef-light ef-frost ef-duo-light ef-deuteranopia-light ef-day ef-cyprus ef-trio-light))
    (add-to-list 'custom-known-themes theme))
  (ef-themes-select 'ef-trio-light)
  :config
  (with-eval-after-load 'org
    ;; (custom-set-faces '(org-done ((t (:foreground "gray")))))
    (setq org-todo-keyword-faces
          `(("TODO" . (:background ,(ef-themes-with-colors red-cooler) :weight bold :foreground "white"))
            ("WIP"  . ,(ef-themes-with-colors yellow-warmer))
            ("PROJ" . (:background ,(ef-themes-with-colors bg-cyan-subtle) :weight bold :foreground ,(ef-themes-with-colors cyan-cooler)))
            ("WAITING" . ,(ef-themes-with-colors fg-dim))
            ;; ("DONE" . (:foreground ,(ef-themes-with-colors fg-alt) :strike-through t))
            ("DONE" . (:foreground "gray" :strike-through t))
            ("UNDONE" . (:foreground ,(ef-themes-with-colors fg-dim) :weight bold :strike-through t))
            ("CANCELED" . (:foreground ,(ef-themes-with-colors fg-dim) :weight bold :strike-through t))))

    (custom-set-faces
     `(org-scheduled-today ((t (:foreground ,(ef-themes-with-colors blue))))))
    )
  )

;; DoomModeline
(use-package doom-modeline
  :hook (window-setup . doom-modeline-mode)
  :custom-face
  (doom-modeline-buffer-modified ((t (:inherit (error bold) :background unspecified))))
  :custom
  ;; (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-major-mode-color-icon t)
  ;; (doom-modeline-height 15)
  (doom-modeline-bar-width 1)
  (doom-modeline-time-icon nil)
  )
;; -DoomModeline

(use-package colorful-mode
  :custom
  (colorful-use-prefix t)
  :hook ((prog-mode . colorful-mode)))

;;;; Pretty Code

;;;###autoload
(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")


(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.
  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.
This function accepts one special property:
  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.
For example, the rule for emacs-lisp-mode is very simple:
  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")
This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.
Pretty symbols can be unset for emacs-lisp-mode with:
  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (doom-enlist modes))
        (assq-delete-all mode +pretty-code-symbols-alist))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let* ((char (plist-get +pretty-code-symbols key)))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (doom-enlist modes))
        (setf (alist-get mode +pretty-code-symbols-alist)
              (if-let* ((old-results (alist-get mode +pretty-code-symbols-alist)))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :<=  8804
    :>=  8805
    :tuple         "⨂"
    :dot           "•"
    :checkbox      "□"
    :pending       "■"
    :checkedbox    "☑"
    :list_property "∷"
    :ellipses      "…"
    :arrow_right   "→"
    :arrow_left    "←"
    :title         "❤"
    :subtitle      "𝙩"
    :author        "✍"
    :date          "⚓"
    :property      "☸"
    :options       "⌥"
    :latex_class   "🄲"
    :latex_header  "⇥"
    :beamer_header "↠"
    :attr_latex    "🄛"
    :attr_html     "🄗"
    :begin_quote   "❮"
    :end_quote     "❯"
    :caption       "☰"
    :header        "›"
    :results       "🍌"
    :begin_export  "⏩"
    :end_export    "⏪"
    :properties    "⚙"
    :end           "∎"
    :priority_a   "🄰"
    :priority_b   "🄱"
    :priority_c   "🄲"
    :priority_d   "🄳"
    ;; :priority_c   "🅲"
    )
  "Options plist for `set-pretty-symbols!'.
This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defvar +pretty-code-enabled-modes '(org-mode)
  "List of major modes in which `prettify-symbols-mode' should be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun +pretty-code-init-pretty-symbols-h ()
  "Enable `prettify-symbols-mode'.
If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.
Otherwise it builds `prettify-code-symbols-alist' according to
`+pretty-code-symbols-alist' for the current major-mode."
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (when (or (eq +pretty-code-enabled-modes t)
              (if (eq (car +pretty-code-enabled-modes) 'not)
                  (not (memq major-mode (cdr +pretty-code-enabled-modes)))
                (memq major-mode +pretty-code-enabled-modes)))
      (setq prettify-symbols-alist
            (append (cdr (assq major-mode +pretty-code-symbols-alist))
                    (default-value 'prettify-symbols-alist)))
      (when prettify-symbols-mode
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))

(add-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)

;; for Python-mode
;; (set-pretty-symbols! 'python-mode
;;   :>= ">="
;;   :<= "<="
;;   ;; Functional
;;   :def "def"
;;   :lambda "lambda"
;;   ;; Types
;;   :null "None"
;;   :true "True"
;;   :false "False"
;;   :int "int"
;;   :str "str"
;;   :float "float"
;;   :bool "bool"
;;   :tuple "tuple"
;;   ;; Flow
;;   :not "not"
;;   :in "in" :not-in "not in"
;;   :and "and" :or "or"
;;   :for "for"
;;   :return "return" :yield "yield")

(set-pretty-symbols! 'emacs-lisp-mode
  :lambda "lambda")

(set-pretty-symbols! 'org-mode
  :name "#+NAME:"
  :src_block "#+begin_src"
  :src_block_end "#+end_src"
  :src_block "#+begin_latex"
  :src_block_end "#+end_latex"
  ;; :checkbox      "[ ]"
  ;; :pending       "[-]"
  ;; :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_latex:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  ;; :priority_a    "[#A]"
  ;; :priority_b    "[#B]"
  ;; :priority_c    "[#C]"
  ;; :priority_d    "[#D]"
  )

;;;;

;;;; Highlight

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook ((elpaca-after-init . global-hl-line-mode)
         ((term-mode vterm-mode) . hl-line-unload-function)))

;; Highlight symbols
(use-package symbol-overlay

  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit (region bold)))))
  (symbol-overlay-default-face ((t (:inherit (region bold)))))
  (symbol-overlay-face-1 ((t (:inherit nerd-icons-blue :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-2 ((t (:inherit nerd-icons-pink :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-3 ((t (:inherit nerd-icons-yellow :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-4 ((t (:inherit nerd-icons-orange :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-5 ((t (:inherit nerd-icons-red :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-6 ((t (:inherit nerd-icons-purple :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-7 ((t (:inherit nerd-icons-green :background unspecified :foreground unspecified :inverse-video t))))
  (symbol-overlay-face-8 ((t (:inherit nerd-icons-cyan :background unspecified :foreground unspecified :inverse-video t))))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook ((prog-mode LaTeX-mode) . hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background unspecified))))
  (diff-hl-insert ((t (:background unspecified))))
  (diff-hl-delete ((t (:background unspecified))))
  :hook ((find-file . diff-hl-mode)
         (vc-dir-mode . diff-hl-dir-mode)
         (dired-mode . diff-hl-dired-mode-unless-remote))
  :init (setq diff-hl-draw-borders nil)
  :config
  (add-hook 'after-change-major-mode-hook 'diff-hl-update-once)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if *sys/mac* #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Pulse current line
(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my-pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 ace-window aw--select-window
                 pager-page-down pager-page-up
                 treemacs-select-window
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))

;;; Program
;;;; Paren
(use-package paren
  :ensure nil
  :hook (elpaca-after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t
              show-paren-style 'parenthesis
              show-paren-context-when-offscreen 'child-frame))

;; Automatic parenthesis pairing
(use-package elec-pair
  :ensure nil
  :hook (+my/first-input . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            #'(lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t
                                 (,electric-pair-inhibit-predicate c)))))))

(with-eval-after-load 'evil
  (defun my/evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
    COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
    (let* ((cur-point (point))
           (parens '("()" "[]" "{}" "<>"))
           (quotes '("\"" "'"))
           (pqs (append parens quotes))
           range
           found-range)
      ;; HACK: ' is widely used in lisp
      (when (derived-mode-p 'emacs-lisp-mode)
        (setq pqs (butlast pqs)))
      (dolist (p pqs)
        (save-excursion
          (ignore-errors
            (if (member p parens)
                (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
              (setq range (evil-select-quote (aref p 0) beg end type count)))))
        (when (and range
                   (<= (car range) cur-point)
                   (>= (cadr range) cur-point))
          (if found-range
              (when (< (- (cadr range) (car range))
                       (- (cadr found-range) (car found-range)))
                (setf (car found-range) (car range))
                (setf (cadr found-range) (cadr range)))
            (setq found-range range))))
      found-range))
  (evil-define-text-object my/evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (my/evil-paren-range count beg end type t))

  (evil-define-text-object my/evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (my/evil-paren-range count beg end type nil))
  (define-key evil-inner-text-objects-map "g" #'my/evil-inner-paren)
  (define-key evil-outer-text-objects-map "g" #'my/evil-a-paren)

  (defun my/edit-kill ()
    (interactive)
    (let* ((parens '("(" ")" "[" "]" "{" "}" "<" ">" "\""))
           (char (string (char-after))))
      ;;  HACK: ' is widely used in lisp
      (when (not (derived-mode-p 'emacs-lisp-mode))
        (push "'" parens))
      (setq unread-command-events
            (append (apply 'vconcat (mapcar 'kbd
                                            (if (and (not (nth 3 (syntax-ppss)))
                                                     (member char parens))
                                                `("d" "a" ,char)
                                              ;; '("d" "i" "g")
                                              '("v" "i" "g" "x")
                                              ))) nil))))
  (with-eval-after-load 'general
    (general-define-key
     :keymaps '(evil-normal-state-map)
     "C-k" 'my/edit-kill)))

;;;; Indent
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)

(defun toggle-indent (&optional indent)
  "Toggle indent based on `evil-auto-indent'"
  (interactive (list evil-auto-indent))
  (if indent
      (progn
        (electric-indent-local-mode -1)
        (setq-local evil-auto-indent nil))
    (progn
      (electric-indent-local-mode 1)
      (setq-local evil-auto-indent t))))

(dolist (hook '(text-mode-hook conf-mode-hook conf-space-mode-hook))
  (add-hook hook (lambda ()
                   (toggle-indent t))))

(use-package indent-bars
  :disabled
  :ensure (indent-bars :type git :host github :repo "jdtsmith/indent-bars")
  :hook (prog-mode . indent-bars-mode)
  :custom-face
  (indent-bars-face ((t (:height 1.2))))
  :custom
  (indent-bars-treesit-support t)
  (indent-bars-no-descend-string t)
  (indent-bars-treesit-ignore-blank-lines-types '("module"))
  (indent-bars-prefer-character t)
  (indent-bars-treesit-wrap
   '((python
	  argument_list
	  parameters ; for python, as an example
	  list
	  list_comprehension
	  dictionary
	  dictionary_comprehension
	  parenthesized_expression
	  subscript)))
  (indent-bars-no-stipple-char ?\⎸)
  :init
  (require 'indent-bars-ts)
  )


;;;; Tree sitter
(use-package treesit
  :if (treesit-available-p)
  :ensure nil
  :init
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
  :config
  (setq treesit-language-source-alist
        '((bash "https://github.com/tree-sitter/tree-sitter-bash")
          (cmake "https://github.com/uyha/tree-sitter-cmake")
          (css "https://github.com/tree-sitter/tree-sitter-css")
          (elisp "https://github.com/Wilfred/tree-sitter-elisp")
          (go "https://github.com/tree-sitter/tree-sitter-go")
          (html "https://github.com/tree-sitter/tree-sitter-html")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json "https://github.com/tree-sitter/tree-sitter-json")
          (make "https://github.com/alemuller/tree-sitter-make")
          (markdown "https://github.com/ikatyang/tree-sitter-markdown")
          (python "https://github.com/tree-sitter/tree-sitter-python")
          (toml "https://github.com/tree-sitter/tree-sitter-toml")
          (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

  (setq major-mode-remap-alist
        '((latex-mode . LaTeX-mode)
          (yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (js-json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
  (setq major-mode-remap-defaults nil)
  )

(use-package evil-textobj-tree-sitter
  :defer nil
  :ensure (evil-textobj-tree-sitter :type git
                                    :host github
                                    :repo "meain/evil-textobj-tree-sitter"
                                    :files (:defaults "queries" "treesit-queries"))
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
  (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))

  ;; You can also bind multiple items and we will match the first one we can find
  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj ("conditional.outer" "loop.outer")))

  ;; function
  ;; Goto start of next function
  (define-key evil-normal-state-map (kbd "]f") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer")))
  ;; Goto start of previous function
  (define-key evil-normal-state-map (kbd "[f") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t)))
  ;; Goto end of next function
  (define-key evil-normal-state-map (kbd "]F") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" nil t)))
  ;; Goto end of previous function
  (define-key evil-normal-state-map (kbd "[F") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "function.outer" t t)))
  ;; class
  ;; Goto start of next class
  (define-key evil-normal-state-map (kbd "]c") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "class.outer")))
  ;; Goto start of previous class
  (define-key evil-normal-state-map (kbd "[c") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))
  ;; Goto end of next class
  (define-key evil-normal-state-map (kbd "]C") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "class.outer" nil t)))
  ;; Goto end of previous class
  (define-key evil-normal-state-map (kbd "[C") (lambda ()
                                                 (interactive)
                                                 (evil-textobj-tree-sitter-goto-textobj "class.outer" t t)))
  )

;;;; LSP
(use-package eglot
  :ensure nil
  :commands (eglot-booster)
  :hook ((eglot-managed-mode . (lambda ()
                                 (leader-def :keymaps 'override
                                   "ca" '(eglot-code-actions :wk "Code Actions")
                                   "cr" '(eglot-rename :wk "Rename symbol")
                                   "cI" '(eglot-code-action-organize-imports :wk "Organize import")
                                   "cJ" '(consult-eglot-symbols :wk "Symbols in project")
                                   "cd" '(eglot-find-declaration :wk "Jump to definition")
                                   "cF" '(eglot-find-implementation :wk "Find implementation")
                                   "cD" '(eglot-find-typeDefinition :wk "Find type definition"))
                                 ;; (eglot-inlay-hints-mode -1)
                                 ))
         ((python-mode python-ts-mode c-mode c++-mode LaTeX-mode) .
          (lambda ()
            (unless (my-project--ignored-p (buffer-file-name (current-buffer)))
              (eglot-ensure)))))
  :config
  (fset 'lsp-capf 'eglot-completion-at-point)
  (setq eglot-stay-out-of '(flymake))
  (setq eglot-sync-connect 0
        eglot-connect-timeout 10
        eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5
        eglot-events-buffer-config '(:size 0 :format full)
        ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
        ;;      its popup rule causes eglot to steal focus too often.
        eglot-auto-display-help-buffer nil)
  (setq eldoc-echo-area-use-multiline-p nil)
  (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
  (add-to-list 'eglot-server-programs '((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) "texlab"))

  (eglot-booster-mode +1)
  )

(use-package consult-eglot)

(use-package eglot-booster
  :ensure (:host github :repo "jdtsmith/eglot-booster")
  :if (executable-find "emacs-lsp-booster")
  :commands (eglot-booster-mode)
  :after eglot
  :config
  (eglot-booster-mode))

;;;; Utils
(use-package devdocs
  :ensure (:host github :repo "astoff/devdocs.el")
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :config
  (add-hook 'python-ts-mode-hook
            (lambda() (setq-local devdocs-current-docs '("python~3.9" "PyTorch" "NumPy~1.20"))))
  (defun devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))
  (defun devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol))))

(use-package project
  :ensure nil
  :after-call +my/first-input-hook-fun
  :config
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))

  (cl-defmethod project-root ((project (head local)))
    (nth 1 project))
  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project" ".projectile" ".rc_config" ".rs_config")
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let* ((root (locate-dominating-file dir f)))
              (throw 'ret (list 'local root))))))))

  (setq project-find-functions '(project-try-vc my/project-try-local))

  ;;  auto remember project
  (add-hook 'change-major-mode-hook (lambda ()
                                      (when (and (buffer-file-name)
                                                 (not (string-match-p "^/\\(?:ssh\\|scp\\|su\\|sudo\\)?:" (buffer-file-name)))
                                                 (not (string-match-p "straight/repos" (buffer-file-name)))
                                                 (fboundp 'project-current))
                                        (when-let* ((root (+my/project-root)))
                                          (project-remember-project (project-current)))))))


(use-package format-all

  :commands format-all-buffer
  :hook ((prog-mode . format-all-ensure-formatter)
         (prog-mode . format-all-mode))
  )

;;;; Language
;;;;; Python
(use-package python
  :ensure nil
  :defer t
  :mode ("\\.py\\'" . python-ts-mode)
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset-verbose nil)

  :config
  (add-hook 'python-ts-mode-hook (lambda ()
                                   (setq-local tab-width 4)))

  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        importmagic-python-interpreter "python")

  )

(use-package py-isort
  :after python
  :hook (python-ts-mode . (lambda ()
                            (add-hook 'before-save-hook
                                      (lambda ()
                                        (when (evil-normal-state-p)
                                          (call-interactively #'py-isort-buffer)))))))


;;;;; Latex
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

(use-package bibtex
  :ensure nil
  :after (org tex)
  :custom
  (bibtex-dialect 'biblatex)
  (bibtex-align-at-equal-sign t)
  (bibtex-text-indentation 20)
  )


(use-package latex
  :ensure  (auctex :pre-build (("./autogen.sh")
                               ("./configure"
                                "--without-texmf-dir"
                                "--with-lispdir=./"
                                "--with-datadir=./")
                               ("make"))
                   :build (:not elpaca--compile-info) ;; Make will take care of this step
                   :files ("*.el" "doc/*.info*" "etc" "images" "latex" "style")
                   :version (lambda (_) (require 'tex-site) AUCTeX-version))
  ;;  FIXME: not work
  :mode ("\\.tex\\'" . LaTeX-mode)
  :hook (LaTeX-mode . outline-minor-mode)
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
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (push
                                '("latexmk" "latexmk -pdf -pvc -view=none %s" TeX-run-TeX nil t :help "Run latexmk on file")
                                TeX-command-list)
                               (push
                                '("xelatex" "xelatex %s" TeX-run-TeX nil t :help "Run xelatex for CJK")
                                TeX-command-list)
                               (setq TeX-command-list (delete-dups TeX-command-list))
                               ))
  ;; use Skim as default pdf viewer
  ;; Skim's displayline is used for forward search (from .tex to .pdf)
  ;; option -b highlights the current line; option -g opens Skim in the background
  (setq TeX-view-program-list
        '(("PDF Viewer" "/Applications/Skim.app/Contents/SharedSupport/displayline -g %n %o %b")))
  (assq-delete-all 'output-pdf TeX-view-program-selection)
  (add-to-list 'TeX-view-program-selection '(output-pdf "PDF Viewer"))


  (defun latex/compile-commands-until-done (clean-first)
    (interactive "P")
    (save-buffer)
    (when clean-first (TeX-clean t))
    (message "Compilation started.")
    (let* ((initial-buffer (buffer-name))
           (TeX-process-asynchronous nil)
           (master-file (TeX-master-file))
           (next-command (TeX-command-default master-file))
           (counter 0))
      (while (and
              (> counter -1)
              (not (equal next-command TeX-command-Show)))
        (message "%d Doing: %s" (cl-incf counter) next-command)
        (set-buffer initial-buffer)
        (TeX-command next-command 'TeX-master-file)
        ;; `TeX-command' occasionally changes current buffer.
        (set-buffer initial-buffer)
        (if (null (plist-get TeX-error-report-switches (intern master-file)))
            ;; (if (string= next-command "BibTeX")
            ;;     (setq next-command "LaTeX")
            ;; (setq next-command (TeX-command-default master-file)))
            (setq next-command (TeX-command-default master-file))
          (setq counter -1)
          (when (y-or-n-p "Error found. Visit it? ")
            ;; `TeX-next-error' number of arguments changed at some
            ;; point.
            (call-interactively #'TeX-next-error))))
      (when (>= counter 0) ;;
        (set-buffer initial-buffer)
        (TeX-view))))

  (setcar (cdr (assoc "Check" TeX-command-list)) "chktex -v6 -H %s")

  (local-leader-def
    :keymaps 'LaTeX-mode-map
    "m" '(TeX-master-file-ask :wk "Master file")
    "c" '(latex/compile-commands-until-done :wk "Compile")
    "v" '(TeX-view :wk "View"))
  )

(use-package cdlatex
  :after tex
  :hook (LaTeX-mode . cdlatex-mode)
  :custom
  (cdlatex-sub-super-scripts-outside-math-mode nil)
  :config
  (add-to-list 'cdlatex-math-modify-alist-default '(?b "\\bm" "\\textbf" t nil nil))
  (add-to-list 'cdlatex-math-modify-alist-default '(?B "\\mathbb" nil t t nil))
  )

(use-package asymbol
  :ensure (:host github :repo "dwuggh/asymbol" :depth 1)
  :hook (LaTeX-mode . asymbol-mode)
  :init
  (setq asymbol-help-symbol-linewidth 110
	    asymbol-help-tag-linewidth 110)

  :config
  (add-hook 'org-cdlatex-mode-hook
            (lambda () (define-key org-cdlatex-mode-map "`" 'asymbol-insert-text-or-symbol)))
  )


;;;;; Org
;;  TODO: remove unused codes

;; Row/Column traversal

;;;###autoload
(defun +org/table-previous-row ()
  "Go to the previous row (same column) in the current table. Before doing so,
re-align the table if necessary. (Necessary because org-mode has a
`org-table-next-row', but not `org-table-previous-row')"
  (interactive)
  (org-table-maybe-eval-formula)
  (org-table-maybe-recalculate-line)
  (if (and org-table-automatic-realign
           org-table-may-need-update)
      (org-table-align))
  (let ((col (org-table-current-column)))
    (beginning-of-line 0)
    (when (or (not (org-at-table-p)) (org-at-table-hline-p))
      (beginning-of-line))
    (org-table-goto-column col)
    (skip-chars-backward "^|\n\r")
    (when (org-looking-at-p " ")
      (forward-char))))

;;
;; Hooks

;;;###autoload
(defun +org-realign-table-maybe-h ()
  "Auto-align table under cursor."
  (when (and (org-at-table-p) org-table-may-need-update)
    (let ((pt (point))
          (inhibit-message t))
      ;; (org-table-recalculate)
      (if org-table-may-need-update (org-table-align))
      (goto-char pt))))

;;
;; Advice

;;;###autoload
(defun +org-realign-table-maybe-a (&rest _)
  "Auto-align table under cursor and re-calculate formulas."
  (when (eq major-mode 'org-mode)
    (+org-realign-table-maybe-h)))

;;;###autoload
(defun +org--refresh-inline-images-in-subtree ()
  "Refresh image previews in the current heading/tree."
  (if (> (length org-inline-image-overlays) 0)
      (org-remove-inline-images)
    (org-display-inline-images
     t t
     (if (org-before-first-heading-p)
         (line-beginning-position)
       (save-excursion (org-back-to-heading) (point)))
     (if (org-before-first-heading-p)
         (line-end-position)
       (save-excursion (org-end-of-subtree) (point))))))

(defun +org--insert-item (direction)
  (let* ((context
          (save-excursion
            (when (bolp)
              (back-to-indentation)
              (forward-char))
            (org-element-lineage
             (org-element-context)
             '(table table-row headline inlinetask item plain-list)
             t)))
         (type (org-element-type context)))
    (cond ((memq type '(item plain-list))
           (let ((marker (org-element-property :bullet context))
                 (pad (save-excursion
                        (org-beginning-of-item)
                        (back-to-indentation)
                        (- (point) (line-beginning-position)))))
             (save-match-data
               (pcase direction
                 (`below
                  (org-end-of-item)
                  (backward-char)
                  (end-of-line)
                  (if (and marker (string-match "\\([0-9]+\\)\\([).] *\\)" marker))
                      (let ((l (line-number-at-pos)))
                        (org-insert-item)
                        (when (= l (line-number-at-pos))
                          (org-next-item)
                          (org-end-of-line)))
                    (insert "\n" (make-string pad 32) (or marker ""))))
                 (`above
                  (org-beginning-of-item)
                  (if (and marker (string-match-p "[0-9]+[).]" marker))
                      (org-insert-item)
                    (insert (make-string pad 32) (or marker ""))
                    (save-excursion (insert "\n")))))))
           (when (org-element-property :checkbox context)
             (insert "[ ] ")))

          ((memq type '(table table-row))
           (pcase direction
             ('below (save-excursion (org-table-insert-row t))
                     (org-table-next-row))
             ('above (save-excursion (org-shiftmetadown))
                     (+org/table-previous-row))))

          ((let ((level (or (org-current-level) 1)))
             (pcase direction
               (`below
                (let (org-insert-heading-respect-content)
                  (goto-char (line-end-position))
                  (org-end-of-subtree)
                  (insert "\n" (make-string level ?*) " ")))
               (`above
                (org-back-to-heading)
                (insert (make-string level ?*) " ")
                (save-excursion (insert "\n"))))
             (when-let* ((todo-keyword (org-element-property :todo-keyword context))
                         (todo-type (org-element-property :todo-type context)))
               (org-todo (cond ((eq todo-type 'done)
                                (car (+org-get-todo-keywords-for todo-keyword)))
                               (todo-keyword)
                               ('todo)))))))

    (when (org-invisible-p)
      (org-show-hidden-entry))
    (when (and (bound-and-true-p evil-local-mode)
               (not (evil-emacs-state-p)))
      (evil-insert 1))))

;;;###autoload
(defun +org-get-todo-keywords-for (&optional keyword)
  "Returns the list of todo keywords that KEYWORD belongs to."
  (when keyword
    (cl-loop for (type . keyword-spec)
             in (cl-remove-if-not #'listp org-todo-keywords)
             for keywords =
             (mapcar (lambda (x) (if (string-match "^\\([^(]+\\)(" x)
                                     (match-string 1 x)
                                   x))
                     keyword-spec)
             if (eq type 'sequence)
             if (member keyword keywords)
             return keywords)))

;;
;; Commands

;;;###autoload
(defun +org/dwim-at-point ()
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- headline: toggle latex fragments and inline images underneath.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: execute the source block
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    ;; skip over unimportant contexts
    (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
      (setq context (org-element-property :parent context)
            type (org-element-type context)))
    (pcase type
      (`headline
       (cond ((and (fboundp 'toc-org-insert-toc)
                   (member "TOC" (org-get-tags)))
              (toc-org-insert-toc)
              (message "Updating table of contents"))
             ((string= "ARCHIVE" (car-safe (org-get-tags)))
              (org-force-cycle-archived))
             ((or (org-element-property :todo-type context)
                  (org-element-property :scheduled context))
              (org-todo
               (if (eq (org-element-property :todo-type context) 'done)
                   (or (car (+org-get-todo-keywords-for (org-element-property :todo-keyword context)))
                       'todo)
                 'done)))
             (t
              (+org--refresh-inline-images-in-subtree)
              (org-clear-latex-preview)
              (org-latex-preview '(4)))))

      (`clock (org-clock-update-time-maybe))

      (`footnote-reference
       (org-footnote-goto-definition (org-element-property :label context)))

      (`footnote-definition
       (org-footnote-goto-previous-reference (org-element-property :label context)))

      ((or `planning `timestamp)
       (org-follow-timestamp-link))

      ((or `table `table-row)
       (if (org-at-TBLFM-p)
           (org-table-calc-current-TBLFM)
         (ignore-errors
           (save-excursion
             (goto-char (org-element-property :contents-begin context))
             (org-call-with-arg 'org-table-recalculate (or arg t))))))

      (`table-cell
       (org-table-blank-field)
       (org-table-recalculate)
       (when (and (string-empty-p (string-trim (org-table-get-field)))
                  (bound-and-true-p evil-local-mode))
         (evil-change-state 'insert)))

      (`src-block
       (org-ctrl-c-ctrl-c))

      (`babel-call
       (org-babel-lob-execute-maybe))

      (`statistics-cookie
       (save-excursion (org-update-statistics-cookies nil)))

      ((or `src-block `inline-src-block)
       (org-babel-execute-src-block))

      ((or `latex-fragment `latex-environment)
       (org-latex-preview))

      (`link
       (let* ((lineage (org-element-lineage context '(link) t))
              (path (org-element-property :path lineage)))
         (if (or (equal (org-element-property :type lineage) "img")
                 (and path (image-type-from-file-name path)))
             (+org--refresh-inline-images-in-subtree)
           (org-open-at-point))))

      ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
       (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
         (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

      (_ (+org--refresh-inline-images-in-subtree)))))


;; I use this instead of `org-insert-item' or `org-insert-heading' which are too
;; opinionated and perform this simple task incorrectly (e.g. whitespace in the
;; wrong places).
;;;###autoload
(defun +org/insert-item-below (count)
  "Inserts a new heading, table cell or item below the current one."
  (interactive "p")
  (dotimes (_ count) (+org--insert-item 'below)))

;;
;; Hooks

;;;###autoload
(defun +org-update-cookies-h ()
  "Update counts in headlines (aka \"cookies\")."
  (when (and buffer-file-name (file-exists-p buffer-file-name))
    (let (org-hierarchical-todo-statistics)
      ;; (org-update-parent-todo-statistics) ;; HACK: it does not work to update statistics while the below one works
      (call-interactively 'org-update-statistics-cookies))))

;;;###autoload
(defun +org-enable-auto-update-cookies-h ()
  "Update statistics cookies when saving or exiting insert mode (`evil-mode')."
  (when (bound-and-true-p evil-local-mode)
    (add-hook 'evil-insert-state-exit-hook #'+org-update-cookies-h nil t))
  (add-hook 'before-save-hook #'+org-update-cookies-h nil t))

;;;###autoload
(defun +org/remove-link ()
  "Unlink the text at point."
  (interactive)
  (unless (org-in-regexp org-link-bracket-re 1)
    (user-error "No link at point"))
  (save-excursion
    (let ((label (if (match-end 2)
                     (match-string-no-properties 2)
                   (org-link-unescape (match-string-no-properties 1)))))
      (delete-region (match-beginning 0) (match-end 0))
      (insert label))))



;; REVIEW These are all proof-of-concept. Refactor me!

;;;###autoload
(defun +org/refile-to-current-file (arg &optional file)
  "Refile current heading to elsewhere in the current buffer.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-targets `((,file :maxlevel . 10)))
        (org-refile-use-outline-path nil)
        (org-refile-keep arg)
        current-prefix-arg)
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-file (arg file)
  "Refile current heading to a particular org file.
If prefix ARG, copy instead of move."
  (interactive
   (list current-prefix-arg
         (read-file-name "Select file to refile to: "
                         default-directory
                         (buffer-file-name (buffer-base-buffer))
                         t nil
                         (lambda (f) (string-match-p "\\.org$" f)))))
  (+org/refile-to-current-file arg file))

;;;###autoload
(defun +org/refile-to-other-window (arg)
  "Refile current heading to an org buffer visible in another window.
If prefix ARG, copy instead of move."
  (interactive "P")
  (let ((org-refile-keep arg)
        org-refile-targets
        current-prefix-arg)
    (dolist (win (delq (selected-window) (window-list)))
      (with-selected-window win
        (let ((file (buffer-file-name (buffer-base-buffer))))
          (and (eq major-mode 'org-mode)
               file
               (cl-pushnew (cons file (cons :maxlevel 10))
                           org-refile-targets)))))
    (call-interactively #'org-refile)))

;;;###autoload
(defun +org/refile-to-running-clock (arg)
  "Refile current heading to the currently clocked in task.
If prefix ARG, copy instead of move."
  (interactive "P")
  (unless (bound-and-true-p org-clock-current-task)
    (user-error "No active clock to refile to"))
  (let ((org-refile-keep arg))
    (org-refile 2)))

;;;###autoload
(defun +org/refile-to-last-location (arg)
  "Refile current heading to the last node you refiled to.
If prefix ARG, copy instead of move."
  (interactive "P")
  (or (assoc (plist-get org-bookmark-names-plist :last-refile)
             bookmark-alist)
      (user-error "No saved location to refile to"))
  (let ((org-refile-keep arg)
        (completing-read-function
         (lambda (_p _coll _pred _rm _ii _h default &rest _)
           default)))
    (org-refile)))

;;;###autoload
(defun +org/archive-done-tasks ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward
            (concat "\\* " (regexp-opt org-done-keywords) " ") nil t)
      (goto-char (line-beginning-position))
      (org-archive-subtree))))

(with-eval-after-load 'consult
  (defun +my/retrieve-todo-entries ()
    (require 'consult-org)
    (consult--read
     (consult--with-increased-gc
      (-filter (lambda (item)
                 (not (member
                       (car (cdr (get-text-property 0 'consult-org--heading item)))
                       '("DONE" "CANCELED" "UNDONE"))))
               (consult-org--headings nil nil (list +org-capture-file-gtd +org-capture-file-routine)))) ;; Only retrieve entries from `gtd' and `routine'
     :prompt "Go to heading: "
     :category 'consult-org-heading
     :sort nil
     :require-match t
     :history '(:input consult-org--history)
     :narrow (consult-org--narrow)
     :state (consult--jump-state)
     :group
     (lambda (cand transform)
       (let ((name (buffer-name
                    (marker-buffer
                     (get-text-property 0 'org-marker cand)))))
         (if transform cand name)))
     :lookup (apply-partially #'consult--lookup-prop 'org-marker)))

  (defun consult-clock-in ()
    "Clock into an Org agenda heading."
    (interactive)
    (save-window-excursion
      (+my/retrieve-todo-entries)
      (org-clock-in)
      (save-all-buffers)))
  (consult-customize consult-clock-in :prompt "Clock in: ")

  (defun consult-mark-done ()
    "Clock into an Org agenda heading."
    (interactive)
    (save-window-excursion
      (+my/retrieve-todo-entries)
      (org-todo 'done)
      (save-all-buffers)))
  (consult-customize consult-mark-done :prompt "Mark done: ")

  )
;; OrgPac
(defvar +org-capture-file-gtd (concat +self/org-base-dir "gtd.org"))
(defvar +org-capture-file-idea (concat +self/org-base-dir "ideas.org"))
(defvar +org-capture-file-note (concat +self/org-base-dir "notes.org"))
(defvar +org-capture-file-someday (concat +self/org-base-dir "someday.org"))
(defvar +org-capture-file-diary (concat +self/org-base-dir "diary.org"))
(defvar +org-capture-file-routine (concat +self/org-base-dir "routine.org"))
(defvar +org-capture-file-proj (concat +self/org-base-dir "proj.org"))

(defvar +org-files (list +org-capture-file-gtd
                         +org-capture-file-someday
                         +org-capture-file-note
                         +org-capture-file-idea
                         +org-capture-file-diary
                         +org-capture-file-routine
                         +org-capture-file-proj))

(use-package org
  :ensure nil
  :commands (+my/open-org-agenda)
  :hook ((org-mode . org-indent-mode)
         (org-mode . +org-enable-auto-update-cookies-h)
         (org-mode . (lambda () (show-paren-local-mode -1) (eldoc-mode -1))))
  :bind (:map org-mode-map
              ([tab] . org-cycle)
              ("C-c s" . my-org-insert-sub-task))
  :init
  (setq
   org-src-window-setup 'current-window
   org-element--cache-self-verify nil
   org-element-use-cache nil
   org-src-preserve-indentation nil
   org-edit-src-content-indentation 0
   org-capture-bookmark nil
   org-log-done 'time
   org-hide-emphasis-markers t
   org-deadline-warning-days 90
   org-export-backends (quote (html icalendar latex md))
   org-use-speed-commands t
   org-confirm-babel-evaluate 'nil
   org-directory (expand-file-name +self/org-base-dir)
   org-ellipsis " ▼ "
   org-babel-python-command "python3"
   org-bullets-bullet-list '("#")

   org-indirect-buffer-display 'current-window
   ;; org-eldoc-breadcrumb-separator " → "
   org-enforce-todo-dependencies t
   org-entities-user
   '(("flat"  "\\flat" nil "" "" "266D" "♭")
     ("sharp" "\\sharp" nil "" "" "266F" "♯"))
   org-fontify-done-headline t
   org-fontify-quote-and-verse-blocks t
   org-fontify-whole-heading-line t
   org-footnote-auto-label 'plain
   org-hide-leading-stars t
   org-hide-leading-stars-before-indent-mode t
   org-image-actual-width nil
   org-list-description-max-indent 4
   org-priority-faces
   '((?A . error)
     (?B . warning)
     (?C . success))
   org-startup-indented t
   org-use-sub-superscripts '{}
   )

  :config
  (setq org-modules '(org-habit))
  (setq org-habit-graph-column 60)

  ;; babel
  (defun my/org-babel-execute-src-block (&optional _arg info _params)
    "Load language if needed"
    (let* ((lang (nth 0 info))
           (sym (cond
                 ((member (downcase lang) '("c" "cpp" "c++"))
                  'C)
                 ((string-prefix-p "jupyter" (downcase lang))
                  (intern "jupyter"))
                 (t
                  (intern lang))))
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

  ;; ui
  ;; (set-face-attribute 'org-table nil :family "Sarasa Mono SC" :weight 'semi-bold)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0))
  (add-hook 'org-capture-mode-hook #'evil-insert-state)

  (custom-set-faces
   '(org-agenda-done ((t (:strike-through nil))))
   '(org-done ((t (:strike-through t :foreground "gray"))))
   '(org-headline-done ((t (:strike-through t :foreground "gray")))))

  (defface org-checkbox-done-text
    '((t (:strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)

  ;; log
  (setq org-log-into-drawer "LOGBOOK")
  (setq org-log-into-drawer t)

  ;; misc
  (setq org-refile-use-outline-path 'file)
  (setq org-refile-targets '((+org-capture-file-gtd :level . 3)
                             (+org-capture-file-someday :level . 3)))
  (setq org-tag-alist '(("academic" . ?a) ("personal" . ?p) ("emacs" . ?e) ("work" . ?w) ("company" . ?c) ("habit" . ?h) ("joy" . ?j) ("computer" . ?C) ("program" . ?P) ("home" . ?H) ("ACT_MONTH" . ?m) ("ACT_WEEK" . ?W) ("ACT_TODAY" . ?d)))
  (setq org-capture-templates
        '(("t" "Todo" entry
           (file +org-capture-file-gtd)
           "* TODO %i%? \n:LOGBOOK: \n:CREATED: %U \n:END:" :prepend t :kill-buffer t)
          ("w" "Watting for" entry
           (file +org-capture-file-gtd)
           "* WAITING %?\n%i" :prepend t :kill-buffer t)
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
           "TODO(t)"  ; A task that needs doing & is ready to do
           "PROJ(p)"  ; An ongoing project that cannot be completed in one step
           "WIP(i)"  ; A task that is in progress
           "WAITING(w)"  ; Something is holding up this task; or it is paused
           "|"
           "DONE(d)"  ; Task successfully completed
           "UNDONE(u)"  ; Task is not completed
           "CANCELED(c)") ; Task was cancelled, aborted or is no longer applicable
          ))

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
                        "C-S-<return>" 'org-insert-subheading)
    (define-key org-src-mode-map (kbd "C-c C-c") 'org-edit-src-exit)
    (general-define-key :states '(normal)
                        :keymaps 'org-src-mode-map
                        ", '" 'org-edit-src-exit)
    (general-define-key :states '(normal)
                        :keymaps 'org-mode-map
                        "<return>" #'+org/dwim-at-point
                        )
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
      "f" 'org-set-property
      "I" 'org-toggle-inline-images
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
      ;; "dt" 'org-time-stamp
      "dt" '(lambda () (interactive) (when (eq evil-state 'normal) (evil-append 1)) (call-interactively 'org-time-stamp))
      "dT" 'org-time-stamp-inactive
      "dp" 'org-timestamp-up
      "dn" 'org-timestamp-down

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
      "sw" '(lambda () (interactive) (+my/org-datetree-find-date-create (current-time)))

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

    )

  (require 'dash)
  (defun todo-to-int (todo)
    (cl-first (-non-nil
               (mapcar (lambda (keywords)
                         (let ((todo-seq
                                (-map (lambda (x) (cl-first (split-string  x "(")))
                                      (cl-rest keywords))))
                           (cl-position-if (lambda (x) (string= x todo)) todo-seq)))
                       org-todo-keywords))))


  (defun my/org-sort-key ()
    (let* ((todo-max (apply #'max (mapcar #'length org-todo-keywords)))
           (todo (org-entry-get (point) "TODO"))
           (todo-int (if todo (todo-to-int todo) todo-max))
           (priority (org-entry-get (point) "PRIORITY"))
           (priority-int (if priority (string-to-char priority) org-default-priority))
           (timestamp (or (org-entry-get (point) "SCHEDULED") (org-entry-get (point) "DEADLINE") "<2100-12-31>")))
      (format "%03d %03d %s" priority-int todo-int timestamp)))

  (defun my/org-sort-entries ()
    (interactive)
    (org-sort-entries nil ?f #'my/org-sort-key))


  ;; functions
;;;###autoload
  (defun +my/open-org-agenda ()
    "open org agenda in left window"
    (interactive)
    (org-agenda nil "n")
    ;;  HACK: open `org-agenda' will clean `org-todo-keywords-for-agenda'
    (setq org-todo-keywords-for-agenda '("CANCELED" "DONE" "WAITING" "WIP" "PROJ" "TODO" "UNDONE")))

  (defun +my-org/mark-done ()
    (interactive)
    (when (derived-mode-p 'org-mode)
      (org-back-to-heading)
      (when-let* ((close-time (org-entry-get (point) "CLOSED"))
                  (close-time (org-time-string-to-time close-time)))
        (org-cut-subtree)
        (with-current-buffer (find-file-noselect +org-capture-file-diary)
          (+my/org-datetree-find-date-create close-time)
          (save-restriction
            (widen)
            (org-fold-show-all)
            ;;  HACK: assuming level of: year, month, week, day, Achieved, items
            (org-paste-subtree 6))
          (save-buffer)))))


  ;; inherit schedule time from parent
  (defun my-org-insert-sub-task ()
    (interactive)
    (let ((parent-schedule (org-get-scheduled-time nil)))
      (org-goto-sibling)
      (org-insert-todo-subheading t)
      (when parent-schedule
        (org-schedule nil parent-schedule))))


;;;###autoload
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


  (defun my/org-add-id-if-scheduled-or-deadline ()
    "Add ID to the current entry if it has SCHEDULED or DEADLINE."
    (when (or (org-entry-get nil "SCHEDULED")
              (org-entry-get nil "DEADLINE"))
      (org-id-get-create)))

  (add-hook 'org-capture-prepare-finalize-hook 'my/org-add-id-if-scheduled-or-deadline)

  (defun my/org-add-ids-to-headlines-in-file ()
    "Add ID properties to all headlines in the current file which
        have scheduled or deadline and do not already have an ID."
    (interactive)
    (org-map-entries
     (lambda ()
       (when (or (org-entry-get nil "SCHEDULED")
                 (org-entry-get nil "DEADLINE"))
         (org-id-get-create)))))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'my/org-add-ids-to-headlines-in-file nil 'local)))


  (defun my/org-remove-ids-without-scheduled-or-deadline ()
    "Remove ID properties from all headlines in the current file
that do not have SCHEDULED or DEADLINE."
    (interactive)
    (org-map-entries
     (lambda ()
       (unless (or (org-entry-get nil "SCHEDULED")
                   (org-entry-get nil "DEADLINE"))
         (org-entry-delete nil "ID")))))

  )
;; ;; -OrgPac

(use-package org-agenda
  :ensure nil
  :ensure nil
  :after org
  ;; :bind
  ;; (:map org-agenda-mode-map
  ;;       ("q" . winner-undo))
  :init
  (setq org-agenda-files (list +org-capture-file-gtd
                               +org-capture-file-diary
                               +org-capture-file-routine)

        ;; org-agenda-window-setup 'only-window
        org-agenda-window-setup 'current-window
        org-agenda-span 3
        org-agenda-start-with-log-mode t
        org-agenda-start-with-clockreport-mode nil
        org-agenda-start-on-weekday 1
        org-agenda-todo-ignore-scheduled 'future
        org-deadline-warning-days 60
        org-agenda-sorting-strategy
        '((agenda habit-down time-up todo-state-down timestamp-up priority-down category-keep)
          (todo priority-down category-keep) (tags priority-down category-keep)
          (search category-keep))
        )
  :config
(defvar my/org-habit-show-graphs-everywhere t
  "If non-nil, show habit graphs in all types of agenda buffers.

Normally, habits display consistency graphs only in
\"agenda\"-type agenda buffers, not in other types of agenda
buffers.  Set this variable to any non-nil variable to show
consistency graphs in all Org mode agendas.")

(defun my/org-agenda-mark-habits ()
  "Mark all habits in current agenda for graph display.

This function enforces `my/org-habit-show-graphs-everywhere' by
marking all habits in the current agenda as such.  When run just
before `org-agenda-finalize' (such as by advice; unfortunately,
`org-agenda-finalize-hook' is run too late), this has the effect
of displaying consistency graphs for these habits.

When `my/org-habit-show-graphs-everywhere' is nil, this function
has no effect."
  (when (and my/org-habit-show-graphs-everywhere
         (not (get-text-property (point) 'org-series)))
    (let ((cursor (point))
          item data)
      (while (setq cursor (next-single-property-change cursor 'org-marker))
        (setq item (get-text-property cursor 'org-marker))
        (when (and item (org-is-habit-p item))
          (with-current-buffer (marker-buffer item)
            (setq data (org-habit-parse-todo item)))
          (put-text-property cursor
                             (next-single-property-change cursor 'org-marker)
                             'org-habit-p data))))))

(advice-add #'org-agenda-finalize :before #'my/org-agenda-mark-habits)

  (plist-put org-agenda-clockreport-parameter-plist :maxlevel 3)
  (defun org/skip-daily-goals (&optional time)
    (let* ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (subtree-end (save-excursion (org-end-of-subtree t)))
           (heading (nth 4 (org-heading-components)))
           (subtree-valid (not (string= "Daily Goals" heading))))
      (unless subtree-valid
        next-headline)))
  (setq org-agenda-custom-commands
        '(("n" "Agenda"
           (
            (tags
             "ACT_MONTH"
             ((org-agenda-files (list +org-capture-file-diary))
              (org-agenda-overriding-header "Month Goals:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("*")))))
            (tags
             "ACT_WEEK"
             ((org-agenda-files (list +org-capture-file-diary))
              (org-agenda-overriding-header "Week Goals:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("*")))))
            (tags
             "ACT_TODAY"
             ((org-agenda-files (list +org-capture-file-diary))
              (org-agenda-overriding-header "Today Goals:")
              (org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo '("*")))))
            (tags
             "+SCHEDULED>=\"<today>\"&SCHEDULED<\"<+1d>\"|DEADLINE>=\"<today>\"&DEADLINE<\"<+1d>\""
             ((org-agenda-overriding-header "Today's Tasks")))
            (agenda "" ((org-agenda-skip-function 'org/skip-daily-goals)))
            (tags-todo "STYLE=\"habit\"" ((org-agenda-overriding-header "Habits")))
            (alltodo "" ((org-agenda-files (list +org-capture-file-gtd))))))))
  (evil-set-initial-state 'org-agenda-mode 'motion)
  (setq-default
   org-agenda-inhibit-startup nil
   org-agenda-skip-unavailable-files t)
  (with-eval-after-load 'general
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

  (defun my/org-save-after-command (&rest _)
    "Save all org buffers after running a command."
    (org-save-all-org-buffers))

  (advice-add 'org-agenda-todo :after #'my/org-save-after-command)
  (advice-add 'org-agenda-clock-in :after #'my/org-save-after-command)
  (advice-add 'org-agenda-clock-out :after #'my/org-save-after-command)
  (advice-add 'org-agenda-clock-cancel :after #'my/org-save-after-command)
  (advice-add 'org-agenda-schedule :after #'my/org-save-after-command)
  (advice-add 'org-agenda-deadline :after #'my/org-save-after-command)

  (run-with-timer 3 nil
                  (lambda ()
                    (with-eval-after-load 'org
                      (require 'appt)

                      (setq appt-display-interval '5) ;; warn every 5 minutes from t - appt-message-warning-time
                      (setq
                       appt-message-warning-time '15 ;; send first warning 15 minutes before appointment
                       appt-display-mode-line nil ;; don't show in the modeline
                       appt-display-format 'window) ;; pass warnings to the designated window function
                      (setq appt-disp-window-function (function ct/appt-display-native))

                      (appt-activate 1) ;; activate appointment notification

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
                      (run-at-time nil 900
                                   (lambda ()
                                     (setq appt-time-msg-list nil)
                                     (org-agenda-to-appt)))
                      (add-hook 'org-finalize-agenda-hook 'org-agenda-to-appt) ;; update appt list on agenda view
                      )))
  )
(use-package org-download
  :commands (org-download-clipboard org-download-delete org-download-image org-download-yank org-download-edit org-download-rename-at-point org-download-rename-last-file org-download-screenshot)
  :after org
  :custom
  (org-download-image-dir "img/")
  (org-download-heading-lvl nil)
  :config
  (cond (*sys/mac*
         (setq org-download-screenshot-method "screencapture -i %s"))))

(use-package org-habit
  :ensure nil
  :custom
  (org-habit-graph-column 1)
  (org-habit-show-all-today nil))

(use-package org-heatmap
  :ensure (:host github :repo "Elilif/org-heatmap")
  :after (org)
  :custom
  (org-heatmap-db-location "~/.emacs.d/.cache/org-heatmap.db")
  :config
  (org-heatmap-mode))

(use-package calfw
  :commands (cfw:open-org-calendar cfw:open-org-week-calendar)
  :ensure (calfw :includes (calfw-org calfw-cal) :host github :repo "theFool32/emacs-calfw" :files ("*.el"))
  :bind (:map cfw:calendar-mode-map
              ("s" . cfw:show-details-command))
  :custom
  (cfw:display-calendar-holidays nil)
  (cfw:display-item-separators nil)
  (cfw:org-calendar-default-view 'two-weeks)
  ;; (cfw:org-calendar-default-view 'month)
  :config
  (require 'calfw-org)

  ;;  HACK: open `org-agenda' will clean `org-todo-keywords-for-agenda'
  (advice-add #'cfw:open-org-calendar :before
              (lambda (&optional _)
                (setq org-todo-keywords-for-agenda '("CANCELED" "DONE" "WAITING" "WIP" "PROJ" "TODO" "UNDONE"))))
  (advice-add #'cfw:refresh-calendar-buffer :before
              (lambda (&optional _)
                (setq org-todo-keywords-for-agenda '("CANCELED" "DONE" "WAITING" "WIP" "PROJ" "TODO" "UNDONE"))))
  (custom-set-faces
   '(cfw:face-today ((((class color) (background light))
                      :background "#C3DEEF") ;;  HACK: background conflicts with face of `TODO' keyword
                     (((class color) (background dark))
                      :foreground "Cyan" :weight bold))))
  )

(use-package org-pomodoro
  :after org
  :commands org-pomodoro
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

(use-package org-reverse-datetree
  :commands (my-org-reverse-datetree-cleanup-empty-dates)
  :ensure (:host github :repo "theFool32/org-reverse-datetree")
  :after org
  :init

  (setq org-reverse-datetree-ignored-heading-text '("Weekly Goals" "Monthly Goals"))
  (setq-default org-reverse-datetree-level-formats
                '("%Y"                    ; year
                  (lambda (time) (format-time-string "%Y-%m %B" (org-reverse-datetree-sunday time))) ; month
                  (lambda (time)
                    (format "W%s (%s - %s)"
                            (format-time-string "%U" time)
                            (format-time-string "%Y-%m-%d" (org-reverse-datetree-sunday time))
                            (format-time-string "%Y-%m-%d" (org-reverse-datetree-last-dow 13 time))
                            )) ; month
                  "%Y-%m-%d %A"))
  :config
  (cl-defun my-org-reverse-datetree-cleanup-empty-dates (&key noconfirm
                                                              ancestors)
    (interactive)
    (unless (derived-mode-p 'org-mode)
      (user-error "Not in org-mode"))
    (when (and (or noninteractive
                   (not (called-interactively-p 'any)))
               (not noconfirm))
      (error "Please set NOCONFIRM when called non-interactively"))
    (let ((levels (length (org-reverse-datetree--get-level-formats t)))
          count)
      (when (> levels 0)
        (org-save-outline-visibility t
          (outline-hide-sublevels (1+ levels))
          (when (or noconfirm
                    (and (not (org-before-first-heading-p))
                         (yes-or-no-p "Start from the beginning?")))
            (goto-char (point-min)))
          (catch 'abort
            (while (> levels 0)
              (setq count 0)

              (while (re-search-forward
                      (rx-to-string `(and bol
                                          (group (= ,levels "*")
                                                 (+ " ")
                                                 (*? nonl)
                                                 (+ "\n")
                                                 (opt
                                                  (and "***** Daily Goals"
                                                       (*? nonl)
                                                       (+ "\n")
                                                       "***** Archived"
                                                       (*? nonl)
                                                       (+ "\n"))))
                                          (or string-end
                                              (and (** 1 ,levels "*")
                                                   " "))))
                      nil t)
                (let ((begin (match-beginning 1))
                      (end (match-end 1)))
                  (cond
                   (noconfirm
                    (delete-region begin end)
                    (cl-incf count)
                    (goto-char begin))
                   ((not noninteractive)
                    (goto-char begin)
                    (push-mark end)
                    (setq mark-active t)
                    (if (yes-or-no-p "Delete this empty entry?")
                        (progn
                          (call-interactively #'delete-region)
                          (cl-incf count)
                          (goto-char begin))
                      (goto-char end))))))
              (when (= count 0)
                (message "No trees were deleted. Aborting")
                (throw 'abort t))
              (if (and (> levels 1)
                       (or (and ancestors
                                noconfirm)
                           (and (not noninteractive)
                                (called-interactively-p 'any)
                                (yes-or-no-p "Clean up the upper level as well?"))))
                  (progn
                    (cl-decf levels)
                    (goto-char (point-min)))
                (throw 'abort t))))))))
  )

(use-package org-tidy
  :ensure t
  :hook
  (org-mode . org-tidy-mode)
  :config
  (setq org-tidy-properties-style 'invisible)
  )

;;;;; Markdown
(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . markdown-mode)
  :init
  (setq markdown-fontify-code-blocks-natively t)
  :config
  (add-to-list 'markdown-code-lang-modes '("python" . python-ts-mode))
  )



;;;;; Web
(use-package web-mode
  :mode
  ("\\.phtml\\'" "\\.tpl\\.php\\'" "\\.[agj]sp\\'" "\\.as[cp]x\\'" "\\.vue\\'"
   "\\.erb\\'" "\\.mustache\\'" "\\.djhtml\\'" "\\.[t]?html?\\'" "\\.wxml\\'")
  :custom
  (web-mode-style-padding 0)
  (web-mode-script-padding 0)
  (web-mode-block-padding 0)
  (web-mode-part-padding 0)
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-comment-annotation t)
  (setq web-mode-enable-comment-interpolation t)
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-comment-formats '(("java" . "//") ("javascript" . "//") ("php" . "//")))
  (modify-syntax-entry ?' "\"" web-mode-syntax-table)
  (modify-syntax-entry ?` "\"" web-mode-syntax-table)
  ;; "-" as word so company completes kabeb-case
  (modify-syntax-entry ?_ "w" web-mode-syntax-table)
  (modify-syntax-entry ?- "w" web-mode-syntax-table)
  (modify-syntax-entry ?# "_" web-mode-syntax-table)
  )

(use-package css-mode
  :ensure nil
  :mode ("\\.css\\'" "\\.wxss\\'")
  :init
  (add-hook 'css-mode-hook #'rainbow-mode))

;; EmmetPac
(use-package emmet-mode
  :hook (web-mode css-mode scss-mode sgml-mode rjsx-mode)
  ;; :bind (:map web-mode-map
  ;;             ("C-j" . emmet-expand-yas))
  :config
  (add-hook 'emmet-mode-hook (lambda()
                               (setq emmet-indent-after-insert t))))
;; -EmmetPac


;;;; Debug
(use-package dape
  :ensure (:host github :repo "svaante/dape")
  :hook (dape-active-mode . dape-breakpoint-global-mode)
  :config

  (transient-define-prefix dape-transient ()
    "Transient for dape."
    [["Stepping"
      ("n" "Next" dape-next :transient t)
      ("i" "Step in" dape-step-in :transient t)
      ("o" "Step out" dape-step-out :transient t)
      ("c" "Continue" dape-continue :transient t)
      ("r" "restart" dape-restart :transient t)]
     ["Breakpoints"
      ("bb" "Toggle" dape-breakpoint-toggle :transient t)
      ("bd" "Remove all" dape-breakpoint-remove-all :transient t)
      ("bl" "Add log breakpoing" dape-breakpoint-log :transient t)
      ("be" "Add expression breakpoint" dape-breakpoint-expression :transient t)
      ]
     ["Switch"
      ("i" "Info" dape-info)
      ("R" "Repl" dape-repl)
      ("m" "Memory" dape-read-memory)
      ("t" "Thread" dape-select-thread)
      ("w" "Watch" dape-watch-dwim)
      ("S" "Stack" dape-select-stack)]
     ["Quit"
      ("qq" "Quit" dape-quit :transient nil)
      ("qk" "Kill" dape-kill :transient nil)]])

  (add-to-list 'dape-configs
               `(debugpy-remote-attach
                 modes (python-mode python-ts-mode)
                 host (lambda () (read-string "Host: " "localhost"))
                 port (lambda () (read-number "Port: "))
                 :request "attach"
                 :type "python"
                 :pathMappings [(:localRoot (lambda ()
                                              (read-directory-name "Local source directory: "
                                                                   (funcall dape-cwd-fn)))
                                            :remoteRoot (lambda ()
                                                          (read-string "Remote source directory: ")))]
                 :justMyCode nil
                 :showReturnValue t))
  )

;;; Edit
;;;; Fold
;; Used for fold
;; Copy from doom-emacs
;;
;; Helpers

(defun +fold--ensure-hideshow-mode ()
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (+fold--ensure-hideshow-mode)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (unless (eolp)
            (end-of-line)
            (+fold--hideshow-fold-p))))))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))


;;
;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-toggle))
          ((+fold--outline-fold-p) (outline-cycle))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-unfold))
          ((+fold--outline-fold-p)
           (outline-show-children)
           (outline-show-entry))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-refold))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block)))
          ((+fold--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (cond ((featurep 'vimish-fold)
         (vimish-fold-unfold-all))
        ((save-excursion
           (+fold--ensure-hideshow-mode)
           (if (integerp level)
               (progn
                 (outline-hide-sublevels (max 1 (1- level)))
                 (hs-life-goes-on
                  (hs-hide-level-recursive (1- level) (point-min) (point-max))))
             (hs-show-all)
             (when (fboundp 'outline-show-all)
               (outline-show-all)))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (progn
      (when (featurep 'vimish-fold)
        (vimish-fold-refold-all))
      (+fold--ensure-hideshow-mode)
      (hs-life-goes-on
       (if (integerp level)
           (hs-hide-level-recursive (1- level) (point-min) (point-max))
         (hs-hide-all))))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (evil-vimish-fold/next-fold count)
                          (evil-vimish-fold/previous-fold (- count))))
                      (if (/= (point) orig-pt) (point))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))

(defface +fold-hideshow-folded-face
  `((t (:inherit font-lock-comment-face :weight light)))
  "Face to hightlight `hideshow' overlays."
  :group 'doom-themes)

;;;###autoload
(defun +fold-hideshow-forward-block-by-indent-fn (_arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (let ((range (+fold-hideshow-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))

;;
;; Indentation detection

(defun +fold--hideshow-empty-line-p (_)
  (string= "" (string-trim (thing-at-point 'line 'no-props))))

(defun +fold--hideshow-geq-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (>= (current-indentation) base-indent)))

(defun +fold--hideshow-g-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (> (current-indentation) base-indent)))

(defun +fold--hideshow-seek (start direction before skip predicate base-indent)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun +fold-hideshow-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base-indent (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
            end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
      (list begin end base-indent))))


(use-package hideshow
  :ensure nil
  :config
  (setq hs-special-modes-alist
        (append
         '((yaml-ts-mode "\\s-*\\_<\\(?:[^:]+\\)\\_>"
                         ""
                         "#"
                         +fold-hideshow-forward-block-by-indent-fn nil)
           (latex-mode
            ;; LaTeX-find-matching-end needs to be inside the env
            ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
            "\\\\end{[a-zA-Z*]+}"
            "%"
            (lambda (_arg)
              ;; Don't fold whole document, that's useless
              (unless (save-excursion
                        (search-backward "\\begin{document}"
                                         (line-beginning-position) t))
                (LaTeX-find-matching-end)))
            nil))
         hs-special-modes-alist
         '((t)))))
(use-package vimish-fold)
(use-package evil-vimish-fold
  :after evil
  :bind
  (([remap evil-toggle-fold]   . #'+fold/toggle)
   ([remap evil-close-fold]    . #'+fold/close)
   ([remap evil-open-fold]     . #'+fold/open)
   ([remap evil-open-fold-rec] . #'+fold/open)
   ([remap evil-close-folds]   . #'+fold/close-all)
   ([remap evil-open-folds]    . #'+fold/open-all))
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
                                        evil-vimish-fold/delete evil-vimish-fold/delete-all
                                        evil-vimish-fold/create evil-vimish-fold/create-line)
  :config
  (vimish-fold-global-mode +1))
;; end of fold



(use-package outli
  :ensure (:host github :repo "jdtsmith/outli")
  :hook ((emacs-lisp-mode . outli-mode)
         ;; (outli-mode . (lambda () (call-interactively #'outline-hide-sublevels)))
         ))


;;; Binding

(use-package which-key
  :hook (+my/first-input . which-key-mode)

  :custom
  (which-key-separator " ")
  (which-key-prefix-prefix "+"))

;; Leader def
(use-package general
  :after evil
  :demand t
  :config
  (general-create-definer tab-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "C-s"
    )

  (tab-def
    "" nil
    "c" '(tab-new :wk "New")
    "r" '(tab-bar-rename-tab :wk "Rename")
    "d" '(tab-bar-close-tab :wk "Close")
    "s" '(tab-bar-select-tab-by-name :wk "Select")
    "t" '(+my/smart-switch-to-vterm-tab :wk "Vterm")
    "1" '((lambda () (interactive) (tab-bar-select-tab 1)) :wk "Select 1")
    "2" '((lambda () (interactive) (tab-bar-select-tab 2)) :wk "Select 2")
    "3" '((lambda () (interactive) (tab-bar-select-tab 3)) :wk "Select 3")
    "4" '((lambda () (interactive) (tab-bar-select-tab 4)) :wk "Select 4")
    "5" '((lambda () (interactive) (tab-bar-select-tab 5)) :wk "Select 5")
    )

  (general-create-definer leader-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix "SPC"
    )
  (general-create-definer local-leader-def
    :states '(normal visual emacs motion)
    :keymaps 'override
    :prefix ",")

  (local-leader-def
    "w" 'evil-avy-goto-word-1
    "/" 'evilnc-comment-or-uncomment-lines)

  (general-evil-define-key 'normal
      '(python-mode-map python-ts-mode-map LaTeX-mode-map emacs-lisp-mode-map yaml-ts-mode-map)
    "<tab>" '+fold/toggle)

  ;; evil mode
  (general-def 'normal
    "/" '+my/consult-line
    "?" '+my/consult-line-symbol-at-point
    "gd" 'xref-find-definitions
    "gr" 'xref-find-references)

  ;; Navigation
  (general-def 'insert
    "C-a" 'beginning-of-line
    "C-e" 'end-of-line)

  ;; (general-def 'insert
  ;;   "C-o" 'evil-normal-state)

  (general-def "<escape>" 'keyboard-quit)
  (general-def "C-<tab>" 'tab-next)

  (leader-def
    "" nil
    ;; "<SPC>" '(execute-extended-command :wk "M-x")
    "<SPC>" '(switch-to-buffer :wk "Switch buffer") ;; maybe `switch-to-buffer' is used more frequently
    "/" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "?" '(+consult-ripgrep-at-point :wk "Search symbol here")
    "." '(find-file :wk "Find file")
    ":" '(pp-eval-expression :wk "Evil expression")
    ";" '(+my/open-org-agenda :wk "Agenda")
    "x" '(org-capture :wk "Org capture")
    "r" '(er/expand-region :wk "expand-region")
    "k" '(+my/replace :wk "Replace")
    "y" '(consult-yank-from-kill-ring :wk "Kill ring")

    "b" '(:wk "Buffer")
    "b[" '(previous-buffer :wk "Previous buffer")
    "b]" '(next-buffer :wk "Next buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-current-buffer :wk "Kill buffer")
    "bd" '((lambda () (interactive) (kill-current-buffer) (evil-quit)) :wk "Kill and close")
    "bq" '(evil-quit :wk "evil-quit")
    "bK" '(+my/kill-other-buffers :wk "Kill other buffers")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(evil-write-all :wk "Save all buffer")

    "f" '(:wk "Files")
    "ff" '(find-file :wk "Find file")
    "fr" '(+my/open-recent :wk "Recent file")
    "fs" '(+my/save-file :wk "Save file")
    "fd" '(dired-jump :wk "Current directory")
    "fe" '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :wk "init.el")
    "fo" '((lambda() (interactive) (find-file (format "%s/%s.org" +self/org-base-dir (completing-read "Open org files:" (mapcar (lambda (f) (file-name-base f)) +org-files))))) :wk "Org files")
    "fh" '((lambda() (interactive)(consult-fd default-directory)) :wk "Find file here")
    "fH" '((lambda() (interactive)(find-file (read-file-name "Remote: " "/scp:"))) :wk "Remote")

    "z" '(consult-dir :wk "z.lua")

    "fE" '(:wk "File Encoding")
    "fEr" '(revert-buffer-with-coding-system :wk "Revert encoding")
    "fEs" '(set-buffer-file-coding-system :wk "Set encoding")

    "j" '(:wk "Jump")
    "jj" '(evil-avy-goto-char :wk "Jump to character")
    "jl" '(evil-avy-goto-line :wk "Jump to line")
    "je" '(+vertico/jump-list :wk "Jump-list")

    "s" '(:wk "Search")
    "sb" '(+my/consult-line-symbol-at-point :wk "Search buffer")
    "si" '(+my/imenu :wk "Jump to symbol")
    "sp" '(consult-ripgrep :wk "Search project")
    "sT" '(load-theme :wk "Load theme")
    "sh" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "sd" '(devdocs-lookup-at-point :wk "Search devdocs")
    "sD" '(devdocs-search-at-point :wk "Search devdocs")
    "sg" '(+my/google-it :wk "Google")

    "c" '(:wk "Code")
    "cf" '(format-all-buffer :wk "Format buffer")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "cm" '(symbol-overlay-put :wk "Mark")

    "e" '(:wk "Error")
    "es" '(flymake-start :wk "Check current buffer")
    "el" '(consult-flymake :wk "List errors")
    "eP" '(flymake-show-project-diagnostics :wk "Show project errors")
    "eb" '(flymake-show-buffer-diagnostics :wk "Show buffer errors")
    "en" '(flymake-goto-next-error :wk "Next error")
    "ep" '(flymake-goto-prev-error :wk "Previous error")


    "g" '(:wk "Git")
    "gs" '(magit-status :wk "status")
    "ga" '(magit-stage-file :wk "stage file")
    "gp" '(magit-push :wk "push")
    "gc" '(magit-commit :wk "commit")
    ;; "gu" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "stage and commit")
    "gu" '(aborn/simple-git-commit-push :wk "stage and commit")
    "gb" '(magit-branch-checkout :wk "checkout")
    "gB" '(magit-blame :wk "blame")
    "gf" '(magit-fetch :wk "fetch")
    "gF" '(magit-pull :wk "pull")
    "gl" '(magit-log :wk "log")
    "go" '(magit-open-repo :wk "open repo")
    "gm" '(gitmoji-picker :wk "open repo")

    "w" '(:wk "Window")
    ;; :sp
    "-" '(evil-window-split :wk "Split window vertically")
    ;; :vsp
    "\\" '(evil-window-vsplit :wk "Split window horizontally")
    "wj" '(evil-window-down :wk "Focus window down")
    "wk" '(evil-window-up :wk "Focus window up")
    "wh" '(evil-window-left :wk "Focus window left")
    "wl" '(evil-window-right :wk "Focus window right")
    "w=" '(balance-windows :wk "balance windows")
    "wu" '(winner-undo :wk "Undo window")
    "wr" '(winner-redo :wk "Redo window")
    "ws" '(ace-window :wk "Select window")
    "wo" '(delete-other-windows :wk "Maximize window")
    "wR" '(desktop-read :wk "Restore session")
    "wS" '(desktop-save-in-desktop-dir :wk "Save session")

    "t" '(:wk "Toggle")
    "tl" '(toggle-truncate-lines :wk "Line wrap")
    "td" '(toggle-debug-on-error :wk "Debug on error")
    "tt" '(dirvish :wk "Dirvish")
    "ts" '(dirvish-side :wk "Dirvish side")
    "te" '(vterm-posframe-toggle :wk "Shell")
    "tc" '(olivetti-mode :wk "Center")
    "ti" '(toggle-indent :wk "Indent")
    "tp" '(+my/profiler-toggle :wk "Profiler")
    "tb" '(blamer-mode :wk "Blame")

    "o" '(:wk "Open")
    "om" '((lambda () (interactive) (mu4e)) :wk "Mail")
    "oy" '(gt-do-translate :wk "Translate at point")
    "oY" '(gt-do-translate-prompt :wk "Translate")
    ;; "oY" '((lambda () (interactive) (maple-translate+ (read-from-minibuffer "Translate word: "))):wk "Translate")
    "oe" '((lambda() (interactive)(if (get-buffer "vterm") (switch-to-buffer "vterm") (call-interactively #'vterm))) :wk "Shell")
    "ov" '(vterm-other-window :wk "Shell in window")
    "ot" '(org-todo-list :wk "Org Todo")
    "ox" '(org-agenda :wk "Org agenda")
    "ob" '(ebib :wk "Ebib")
    "oB" '(ebib-import-ref :wk "Ebib import")
    "oc" '(cfw:open-org-calendar :wk "Calendar")
    "od" '(consult-mark-done :wk "Mark done")
    "oi" '(consult-clock-in :wk "Clock in")
    "oo" '((lambda () (interactive)(org-clock-out) (org-save-all-org-buffers)) :wk "Clock out")
    "op" '(org-pomodoro :wk "Pomodoro")
    "ou" '(browse-url :wk "Url")
    "og" '(gptel :wk "GPT")

    "a" '(:wk "AI")
    "aa" '(gptel-add :wk "Add")
    "as" '(gptel-send :wk "Send")
    "ar" '(gptel-rewrite :wk "Rewrite")
    "ai" '(aidermacs-transient-menu :wk "Aider")

    "p" '(:wk "Project")
    "pp" '(project-switch-project :wk "Switch project")
    "pf" '(project-find-file :wk "Find file in project")
    "pt" '(consult-todo-project :wk "List project tasks")
    "pk" '(project-kill-buffers :wk "Kill project buffers")

    "q" '(:wk "Quit")
    ;; "qq" '(kill-emacs :wk "Quit")
    "qq" '(save-buffers-kill-terminal :wk "Quit")
    "qr" '(restart-emacs :wk "Restart")

    "u" '((lambda() (interactive)
            (call-process-shell-command "rc" nil 0)
            ;; (call-process-shell-command "rs" nil 0)
            (call-process-shell-command (concat "rs " (buffer-file-name)) nil 0)
            ) :wk "Sync code")
    )
  )

;;; End

(add-hook 'elpaca-after-init-hook
          #'(lambda ()
              (+my/open-org-agenda)
              (message "Start in %s s with %d gc" (float-time (time-subtract (current-time) +my/start-time)) gcs-done)))

(provide 'init)

;;; init.el ends here
