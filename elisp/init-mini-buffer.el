;; -*- lexical-binding: t -*-

;;; init-mini-buffer.el ---


(eval-when-compile
  (require 'init-custom)
  (require 'init-const))

(autoload 'ffap-file-at-point "ffap")

(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config

  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))
  )
(use-package embark-consult
  :ensure t ; only
  :after consult)

(use-package vertico
  :straight (vertico :includes (vertico-quick vertico-repeat vertico-directory)
                     :files (:defaults "extensions/vertico-*.el"))
  ;; :hook (+my/first-input . vertico-mode)
  :bind
  (:map vertico-map
        ("C-<return>" . open-in-external-app))
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle nil)

  (defun +vertico-restrict-to-matches ()
    (interactive)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (insert " ")
      (add-text-properties (minibuffer-prompt-end) (point-max)
                           '(invisible t read-only t cursor-intangible t rear-nonsticky t))))
  (define-key vertico-map (kbd "S-SPC") #'+vertico-restrict-to-matches)

  (defun open-in-external-app ()
    (interactive)
    (let ((candidate (+complete-get-current-candidate)))
      (when (eq (+complete--get-meta 'category) 'file)
        (shell-command (concat "open " candidate))
        (abort-recursive-edit))))

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
                ;; ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("C-w" . vertico-directory-delete-word))
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))
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
  :demand t
  :after orderless
  :straight (:host github :repo "minad/consult")
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
  (setq consult-preview-key nil)
  (setq consult-narrow-key "<")
  (setq consult-buffer-sources '(consult--source-buffer consult--source-hidden-buffer))

  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; consult-imenu
  (with-eval-after-load 'consult-imenu
    (add-to-list 'consult-imenu-config '(python-mode :types
                                                     ((?c "Class"    font-lock-type-face)
                                                      (?C "Constant"    font-lock-constant-face)
                                                      (?f "Function"  font-lock-function-name-face)
                                                      (?m "Method"  font-lock-function-name-face)
                                                      (?v "Variable"  font-lock-variable-name-face)))))

  (autoload 'org-buffer-list "org")
  (defvar org-buffer-source
    `(:name     "Org"
                :narrow   ?o
                :category buffer
                :state    ,#'consult--buffer-state
                :hidden   t
                :items    ,(lambda () (mapcar #'buffer-name (org-buffer-list)))))
  (add-to-list 'consult-buffer-sources 'org-buffer-source 'append)

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
        "rga --null --line-buffered --color=never --max-columns=1000 --path-separator /\
   --smart-case --no-heading --line-number .")

  (defun +consult-ripgrep-at-point (&optional dir initial)
    (interactive (list prefix-arg (when-let ((s (symbol-at-point)))
                                    (symbol-name s))))
    (consult-ripgrep dir initial))

  ;; HACK add `ignore' according to upstream, wihout meaning
  (defun consult--orderless-regexp-compiler (input type igore)
    (setq input (orderless-pattern-compiler input))
    (cons
     (mapcar (lambda (r) (consult--convert-regexp r type)) input)
     (lambda (str) (orderless--highlight input str))))
  (defun consult--with-orderless (&rest args)
    (minibuffer-with-setup-hook
        (lambda ()
          (setq-local consult--regexp-compiler #'consult--orderless-regexp-compiler))
      (apply args)))
  (advice-add #'consult-ripgrep :around #'consult--with-orderless)

  (defvar consult--fd-command nil)
  (defun consult--fd-builder (input)
    (unless consult--fd-command
      (setq consult--fd-command
            (if (eq 0 (call-process-shell-command "fdfind"))
                "fdfind"
              "fd")))
    (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
                 (`(,re . ,hl) (funcall consult--regexp-compiler
                                        arg 'extended t)))
      (when re
        (list :command (append
                        (list consult--fd-command
                              "--color=never" "--full-path"
                              (consult--join-regexps re 'extended))
                        opts)
              :highlight hl))))

  (defun consult-fd (&optional dir initial)
    (interactive "P")
    (let* ((prompt-dir (consult--directory-prompt "Fd" dir))
           (default-directory (cdr prompt-dir)))
      (find-file (consult--find (car prompt-dir) #'consult--fd-builder initial))))
  )

(use-package consult-project-extra
  :after consult
  :straight (consult-project-extra :type git :host github :repo "Qkessler/consult-project-extra")
  :config
  ;; WORKAROUND
  (setq consult-project-buffer-sources consult-project-extra-sources)
  )

(use-package consult-flycheck
  :after (consult flycheck))

(use-package consult-dir
  :ensure t
  :after consult
  :bind (("C-x C-d" . consult-dir)
         :map vertico-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file))
  :config
  (defun consult-dir--zlua-dirs ()
    "Return list of fasd dirs."
    (mapcan
     (lambda (str) (last (split-string str " ")))
     (split-string (shell-command-to-string "z -l") "\n" t)))
  (defvar consult-dir--source-zlua
    `(:name     "z.lua dirs"
                :narrow   ?z
                :category file
                :face     consult-file
                :history  file-name-history
                :enabled  ,(lambda () t)  ;;  FIXME: check whether z.lua is installed
                :items    ,#'consult-dir--zlua-dirs)
    "Fasd directory source for `consult-dir'.")
  (add-to-list 'consult-dir-sources 'consult-dir--source-zlua t)
  )

(use-package consult-git-log-grep
  :after consult
  :commands consult-git-log-grep
  :straight (:host github :repo "ghosty141/consult-git-log-grep")
  :custom
  (consult-git-log-grep-open-function #'magit-show-commit))


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

  ;; FIX for tramp
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))

  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))

  (setq completion-styles '(orderless)
        completion-category-defaults nil
        ;;; Enable partial-completion for files.
        ;;; Either give orderless precedence or partial-completion.
        ;;; Note that completion-category-overrides is not really an override,
        ;;; but rather prepended to the default completion-styles.
        completion-category-overrides '((file (styles basic-remote orderless))
                                        ;; enable initialism by default for symbols
                                        (command (styles +orderless-with-initialism))
                                        (variable (styles +orderless-with-initialism))
                                        (symbol (styles +orderless-with-initialism))
                                        (elgot (styles +orderless-with-initialism)))
        orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
        orderless-style-dispatchers '(+orderless-dispatch)))

(use-package marginalia
  :hook (+my/first-input . marginalia-mode)
  :config
  (setq marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light)))

(use-package mini-frame
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :config
  (setq mini-frame-detach-on-hide nil)
  (setq resize-mini-frames t)
  (setq mini-frame-create-lazy nil)
  (setq mini-frame-show-parameters `((left . 0.5)
                                     (top . ,(/ (frame-pixel-height) 2))
                                     (min-width . 80)
                                     (width . 0.8)
                                     (no-accept-focus . t)))

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
