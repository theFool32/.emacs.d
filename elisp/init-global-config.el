;;; init-global-config.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-custom)
  (require 'init-const))

;; SudoEditPac
(use-package sudo-edit
  :commands (sudo-edit))
;; -SudoEditPac

;; UTF8Coding
(unless *sys/win32*
  (set-selection-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)
  (set-language-environment "UTF-8")
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8))
;; Treat clipboard input as UTF-8 string first; compound text next, etc.
(when *sys/gui*
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))
;; -UTF8Coding

;; EditExp
;; Remove useless whitespace before saving a file
(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

;; Replace selection on insert
(delete-selection-mode 1)

;; Map Alt key to Meta
(setq x-alt-keysym 'meta)
(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

;; -EditExp

;; History
(use-package recentf
  :hook (after-init . recentf-mode)
  :custom
  ;; (recentf-auto-cleanup "05:00am")
  (recentf-max-saved-items 200)
  (recentf-exclude `(,(expand-file-name package-user-dir)
                     ,+self/org-base-dir
                     ,(expand-file-name "~\/.mail\/*")
                     "^/\\(?:ssh\\|scp\\|su\\|sudo\\)?:"
                     ".cache"
                     ".cask"
                     ".elfeed"
                     "bookmarks"
                     "cache"
                     "ido.*"
                     "persp-confs"
                     "recentf"
                     "undo-tree-hist"
                     "url"
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
  )

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
;; Currently available in Emacs master branch *only*!
(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

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
                                      (when (derived-mode-p 'prog-mode)
                                        (call-process-shell-command "rc" nil 0))))))

;; _ as part of a word
(modify-syntax-entry ?_ "w")
(defalias 'forward-evil-word 'forward-evil-symbol)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)
(setq command-line-ns-option-alist nil)


(use-package gcmh
  :hook (+my/first-input . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 64 1024 1024)))

(setq vc-follow-symlinks t)


;; Disable message for some functions
(defun suppress-message-advice-around (fun &rest args)
  (let (message-log-max)
    (with-temp-message (or (current-message) "")
      (apply fun args))))
(advice-add 'save-buffer :around 'suppress-message-advice-around)

(defun filter-command-error-function (data context caller)
  "Ignore the buffer-read-only, beginning-of-line, end-of-line, beginning-of-buffer, end-of-buffer signals; pass the rest to the default handler."
  (when (not (memq (car data) '(buffer-read-only
                                beginning-of-line
                                end-of-line
                                beginning-of-buffer
                                end-of-buffer)))
    (command-error-default-function data context caller)))

(setq command-error-function #'filter-command-error-function)


(add-hook 'after-change-major-mode-hook #'hexcolour-add-to-font-lock)

(provide 'init-global-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-global-config.el ends here
