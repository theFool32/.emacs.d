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
;;     Update #: 483
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

(defun +complete-fido-enter-dir ()
  (interactive)
  (let ((candidate (vertico--candidate))
        (current-input (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
    (cond
     ((and (vertico-directory--completing-file-p)
           (string= (car (last (s-split "/" current-input))) ".."))
      (progn
        (vertico-directory-delete-word 1)))

     ((and (vertico-directory--completing-file-p)
           (file-directory-p candidate)
           (not (string= candidate "~/")))
      (vertico-insert))

     (t (insert "/")))))


(use-package vertico
  :straight (vertico :includes (vertico-quick vertico-repeat vertico-directory)
                     :files (:defaults "extensions/vertico-*.el"))
  ;; :hook (+self/first-input . vertico-mode)
  :init
  (vertico-mode)
  :config
  (set-face-background 'vertico-current "#42444a")
  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle nil)

  ;; TODO: only for mac os now
  (defun open-in-external-app ()
    (interactive)
    (let ((candidate (+complete-get-current-candidate)))
      (when (eq (+complete--get-meta 'category) 'file)
        (shell-command (concat "open " candidate))
        (abort-recursive-edit))))
  (define-key vertico-map (kbd "C-<return>") 'open-in-external-app)

  (defun +vertico/jump-list (jump)
    "Go to an entry in evil's (or better-jumper's) jumplist."
    (interactive
     (let (buffers)
       (require 'consult)
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

  ;; Configure directory extension.
  (use-package vertico-quick
    :after vertico
    :ensure nil
    :bind (:map vertico-map
                ("M-q" . vertico-quick-insert)
                ("C-q" . vertico-quick-exit)))
  (use-package vertico-repeat
    :after vertico
    :ensure nil
    :config
    (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
    (with-eval-after-load 'general
      (general-def "C-c r" 'vertico-repeat)
      ))
  (use-package vertico-directory
    :after vertico
    :ensure nil
    ;; More convenient directory navigation commands
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word)
                ("C-w" . vertico-directory-up)
                ("/" . +complete-fido-enter-dir))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
    )
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; Alternatively try `consult-completing-read-multiple'.
  (defun crm-indicator (args)
    (cons (concat "[CRM] " (car args)) (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))


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
  (setq consult-narrow-key "<")
  (setq consult-async-input-debounce 0)
  (setq consult-async-input-throttle 0)
  (setq consult-buffer-sources '(consult--source-buffer consult--source-hidden-buffer))

  (setq consult-preview-key nil)
  (setq consult-narrow-key "<")
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-find-args "fd --color=never --full-path ARG OPTS")

  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

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
        (let ((consult-ripgrep-args
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

  (defun +consult-ripgrep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (consult-ripgrep dir initial))
  (defun consult--orderless-regexp-compiler (input type)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  ;; (setq consult--regexp-compiler #'consult--orderless-regexp-compiler)
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)
  )

(use-package consult-project-extra
  :straight (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra"))

(use-package orderless
  :demand t
  :config
  (defvar +orderless-dispatch-alist
    '((?% . char-fold-to-regexp)
      (?! . orderless-without-literal)
      (?`. orderless-initialism)
      (?= . orderless-literal)
      (?~ . orderless-flex)))

  ;; Recognizes the following patterns:
  ;; * ~flex flex~
  ;; * =literal literal=
  ;; * %char-fold char-fold%
  ;; * `initialism initialism`
  ;; * !without-literal without-literal!
  ;; * .ext (file extension)
  ;; * regexp$ (regexp matching at end)
  (defun +orderless-dispatch (pattern index _total)
    (cond
     ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x100000-\x10FFFD]*$")))
     ;; File extensions
     ((and
       ;; Completing filename or eshell
       (or minibuffer-completing-file-name
           (derived-mode-p 'eshell-mode))
       ;; File extension
       (string-match-p "\\`\\.." pattern))
      `(orderless-regexp . ,(concat "\\." (substring pattern 1) "[\x100000-\x10FFFD]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Prefix and suffix
     ((if-let (x (assq (aref pattern 0) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 1))
        (when-let (x (assq (aref pattern (1- (length pattern))) +orderless-dispatch-alist))
          (cons (cdr x) (substring pattern 0 -1)))))))

  ;; Define orderless style with initialism by default
  (orderless-define-completion-style +orderless-with-initialism
    (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

  ;; You may want to combine the `orderless` style with `substring` and/or `basic`.
  ;; There are many details to consider, but the following configurations all work well.
  ;; Personally I (@minad) use option 3 currently. Also note that you may want to configure
  ;; special styles for special completion categories, e.g., partial-completion for files.
  ;;
  ;; 1. (setq completion-styles '(orderless))
  ;; This configuration results in a very coherent completion experience,
  ;; since orderless is used always and exclusively. But it may not work
  ;; in all scenarios. Prefix expansion with TAB is not possible.
  ;;
  ;; 2. (setq completion-styles '(substring orderless))
  ;; By trying substring before orderless, TAB expansion is possible.
  ;; The downside is that you can observe the switch from substring to orderless
  ;; during completion, less coherent.
  ;;
  ;; 3. (setq completion-styles '(orderless basic))
  ;; Certain dynamic completion tables (completion-table-dynamic)
  ;; do not work properly with orderless. One can add basic as a fallback.
  ;; Basic will only be used when orderless fails, which happens only for
  ;; these special tables.
  ;;
  ;; 4. (setq completion-styles '(substring orderless basic))
  ;; Combine substring, orderless and basic.
  ;;
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
        completion-category-overrides '((file (flex styles basic partial-completion)) ;; partial-completion is tried first
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

(use-package marginalia
  :hook (+self/first-input . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

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

  (setq mini-frame-internal-border-color "gray50")

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
