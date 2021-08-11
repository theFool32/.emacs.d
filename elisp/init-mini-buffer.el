;; -*- lexical-binding: t -*-

;;; init-mini-buffer.el ---
;;
;; Filename: init-mini-buffer.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Sun May  2 14:40:03 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 422
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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
  (require 'init-custom)
  (require 'init-const))

(autoload 'ffap-file-at-point "ffap")
;; Copy from selectrum
(defun +complete--metadata ()
  "Get completion metadata.
Demotes any errors to messages."
  (condition-case-unless-debug err
      (completion-metadata (minibuffer-contents)
                           minibuffer-completion-table
                           minibuffer-completion-predicate)
    (error (message (error-message-string err)) nil)))

(defun +complete--get-meta (setting)
  "Get metadata SETTING from completion table."
  (completion-metadata-get (+complete--metadata) setting))

(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn
                             (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table)))) 'append)
(defun +complete-fido-backward-updir ()
  "Delete char before or go up directory, like `ido-mode'."
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (+complete--get-meta 'category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))
    (call-interactively 'backward-delete-char)))

(defun +complete-fido-delete-char ()
  "Delete char or maybe call `dired', like `ido-mode'."
  (interactive)
  (let ((end (point-max)))
    (if (or (< (point) end) (not (eq (+complete--get-meta 'category) 'file)))
        (call-interactively 'delete-char)
      (dired (file-name-directory (minibuffer-contents)))
      (exit-minibuffer))))

(defun +complete-get-current-candidate ()
  (selectrum-get-current-candidate))

(defun +complete-insert-current-candidate ()
  (selectrum-insert-current-candidate))

(defun +complete-fido-enter-dir ()
  (interactive)
  (let ((candidate (+complete-get-current-candidate))
        (current-input (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond
     ((and (eq (+complete--get-meta 'category) 'file)
           (string= (car (last (s-split "/" current-input))) ".."))
      (progn
        (insert "/")
        (+complete-fido-do-backward-updir)
        (+complete-fido-do-backward-updir)))

     ((and (eq (+complete--get-meta 'category) 'file)
           (file-directory-p candidate)
           (not (string= candidate "~/")))
      (+complete-insert-current-candidate))

     (t (insert "/")))))

(defun +complete-fido-do-backward-updir ()
  (interactive)
  (if (and (eq (char-before) ?/)
           (eq (+complete--get-meta 'category) 'file))
      (save-excursion
        (goto-char (1- (point)))
        (when (search-backward "/" (point-min) t)
          (delete-region (1+ (point)) (point-max))))))


(use-package selectrum
  :hook (+self/first-input . selectrum-mode)
  :config

  (with-eval-after-load 'general
    (general-def "C-c C-r" 'selectrum-repeat)
    )
  ;; (setq selectrum-should-sort nil)
  (with-eval-after-load 'orderless
    (setq orderless-skip-highlighting (lambda () selectrum-is-active))
    (setq selectrum-refine-candidates-function #'orderless-filter)
    (setq selectrum-highlight-candidates-function #'orderless-highlight-matches))

  (define-key selectrum-minibuffer-map (kbd "DEL") '+complete-fido-backward-updir)
  (define-key selectrum-minibuffer-map (kbd "/") '+complete-fido-enter-dir)
  (define-key selectrum-minibuffer-map (kbd "C-d") '+complete-fido-delete-char)
  (define-key selectrum-minibuffer-map (kbd "C-w") '+complete-fido-do-backward-updir)

  )


(use-package consult
  :after orderless
  :straight (:host github :repo "minad/consult")
  :bind (
         ([remap recentf-open-files] . consult-recent-file)
         ([remap imenu] . consult-imenu)
         ([remap switch-to-buffer] . consult-buffer)
         ("M-g o" . consult-outline)
         ("M-g h" . consult-org-heading)
         ("M-g a" . consult-org-agenda)
         )
  :init
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)
  :config
  (setq consult-preview-key nil)
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-narrow-key "<")
  (setq consult-async-input-debounce 0)
  (setq consult-async-input-throttle 0)
  (setq consult-buffer-sources '(consult--source-buffer consult--source-hidden-buffer))

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  (setq consult-preview-key nil)
  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-command "fd --color=never --full-path ARG OPTS")

  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)


  (with-eval-after-load 'projectile
    (projectile-load-known-projects))

  (defun my-consult-set-evil-search-pattern (&optional condition)
    (let ((re
           (cond
            ((eq condition 'rg) (substring (car consult--grep-history) 1)) ;; HACK: assume the history begins with `#'
            ((or t (eq condition 'line)) (car consult--line-history))
            )))
      (add-to-history 'evil-ex-search-history re)
      (setq evil-ex-search-pattern (list re t t))
      (setq evil-ex-search-direction 'forward)))

  (defun my-consult-line-symbol-at-point ()
    (interactive)
    (evil-without-repeat ;; I use evil always
      (consult-line (thing-at-point 'symbol))
      (my-consult-set-evil-search-pattern)))

  (defcustom noct-consult-ripgrep-or-line-limit 300000
    "Buffer size threshold for `noct-consult-ripgrep-or-line'.
When the number of characters in a buffer exceeds this threshold,
`consult-ripgrep' will be used instead of `consult-line'."
    :type 'integer)

  (defun noct-consult-ripgrep-or-line ()
    "Call `consult-line' for small buffers or `consult-ripgrep' for large files."
    (interactive)
    (evil-without-repeat ;; I use evil always
      (if (or (not buffer-file-name)
              (buffer-narrowed-p)
              (ignore-errors
                (file-remote-p buffer-file-name))
              (jka-compr-get-compression-info buffer-file-name)
              (<= (buffer-size)
                  (/ noct-consult-ripgrep-or-line-limit
                     (if (eq major-mode 'org-mode) 4 1))))
          (progn (consult-line)
                 (my-consult-set-evil-search-pattern))
        (when (file-writable-p buffer-file-name)
          (save-buffer))
        (let ((consult-ripgrep-command
               (concat "rg "
                       "--null "
                       "--line-buffered "
                       "--color=ansi "
                       "--max-columns=250 "
                       "--no-heading "
                       "--line-number "
                       ;; adding these to default
                       "--smart-case "
                       "--hidden "
                       "--max-columns-preview "
                       ;; add back filename to get parsing to work
                       "--with-filename "
                       ;; defaults
                       "-e ARG OPTS "
                       (shell-quote-argument buffer-file-name))))
          (consult-ripgrep)
          (my-consult-set-evil-search-pattern 'rg)))))

  (defun consult--orderless-regexp-compiler (input type)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
  )

(use-package consult-projectile
  :after projectile
  :straight (consult-projectile :type git :host gitlab :repo "OlMon/consult-projectile" :branch "master")
  )


;; Completion styles
(setq completion-styles '(basic partial-completion substring initials flex))
(use-package orderless
  :after marginalia
  :demand t
  :config
  (savehist-mode)
  (setq orderless-matching-styles '(orderless-regexp orderless-literal orderless-initialism ))
  (defun without-if-$! (pattern _index _total)
    (when (or (string-prefix-p "$" pattern) ;如果以! 或$ 开头，则表示否定，即不包含此关键字
              (string-prefix-p "!" pattern))
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun flex-if-comma (pattern _index _total) ;如果以逗号结尾，则以flex 算法匹配此组件
    (when (string-suffix-p "," pattern)
      `(orderless-flex . ,(substring pattern 0 -1))))
  (defun literal-if-= (pattern _index _total) ;如果以=结尾，则以literal  算法匹配此关键字
    (when (or (string-suffix-p "=" pattern)
              (string-suffix-p "-" pattern)
              (string-suffix-p ";" pattern))
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(literal-if-= flex-if-comma without-if-$!))

  (setq completion-styles (cons 'orderless completion-styles)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package marginalia
  :hook (+self/first-input . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light))
  (advice-add #'marginalia-cycle :after
              (lambda () (when (bound-and-true-p selectrum-mode) (selectrum-exhibit)))))

(use-package mini-frame
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :config
  (setq resize-mini-frames t)
  (setq mini-frame-create-lazy nil)
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . ,(/ (frame-pixel-height) 2))
                                     (background-mode 'dark)
                                     (foreground-color . "#bbc2cf")
                                     (background-color . "#242730")
                                     ;; (internal-border-width . 1)
                                     ;; (child-frame-border-width . 1)
                                     (min-width . 80)
                                     (width . 0.8)))

  ;; (setq mini-frame-internal-border-color "gray50")  ;; FIXME: very slow for mini-frame border
  ;; (set-face-background 'child-frame-border "gray50" mini-frame-frame)

  (when (and (not noninteractive) (require 'mini-frame nil t)) ;batch 模式下miniframe 有问题
    (add-to-list 'mini-frame-ignore-functions 'y-or-n-p)
    (add-to-list 'mini-frame-ignore-functions 'yes-or-no-p)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex)
    (add-to-list 'mini-frame-ignore-commands 'org-time-stamp)
    (add-to-list 'mini-frame-ignore-commands 'org-deadline)
    (add-to-list 'mini-frame-ignore-commands 'org-schedule)
    (add-to-list 'mini-frame-ignore-commands 'pp-eval-expression)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-forward)
    (add-to-list 'mini-frame-ignore-commands 'evil-ex-search-backward))
  )

(use-package all-the-icons-completion
  :straight (:host github :repo "iyefrat/all-the-icons-completion")
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup))

(provide 'init-mini-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-mini-buffer.el ends here
