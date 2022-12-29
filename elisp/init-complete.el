;; -*- lexical-binding: t -*-

(use-package corfu
  :straight (corfu :includes (corfu-indexed corfu-quick corfu-popupinfo corfu-history) :files (:defaults "extensions/corfu-*.el"))
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.01)
  (corfu-echo-documentation 0.3)
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  (corfu-on-exact-match 'quit)
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
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-round)
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-bracket)
  (add-to-list 'corfu-auto-commands 'grammatical-edit-open-curly)

  (advice-add #'keyboard-quit :before #'corfu-quit)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)

  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)

  (use-package corfu-quick
    :bind
    (:map corfu-map
          ("C-q" . corfu-quick-insert)))

  (use-package corfu-popupinfo
    :config
    (setq corfu-popupinfo-delay '(0.2 . 0.1))
    :hook (corfu-mode . corfu-popupinfo-mode))

  (use-package corfu-history
    :config
    (corfu-history-mode))

  ;; kind ui
  (with-eval-after-load 'all-the-icons
    (defvar kind-all-the-icons--cache nil
      "The cache of styled and padded label (text or icon).
An alist.")

    (defun kind-all-the-icons-reset-cache ()
      "Remove all cached icons from `kind-all-the-icons-mapping'."
      (interactive)
      (setq kind-all-the-icons--cache nil))

    (defun kind-all-the-icons--set-default-clear-cache (&rest args)
      (kind-all-the-icons-reset-cache)
      (apply #'set-default args))

    (defvar kind-all-the-icons--icons
      `((unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
        (text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
        (method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (fun . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (ctor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
        (field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (var . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
        (class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (i/f . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (mod . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (prop . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
        (unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
        (value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
        (enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (k/w . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
        (snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (sn . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
        (color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
        (file . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
        (reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (ref . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
        (folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (dir . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
        (enum-member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (enummember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (member . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
        (constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (const . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
        (struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
        (event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
        (operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (op . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
        (type-parameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (param . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
        (template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15))
        (tmux . ,(all-the-icons-alltheicon "terminal-alt" :height 0.8 :v-adjust 0))
        (tabnine . ,(all-the-icons-material "cloud" :height 0.8))
        (t . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))))


    (defsubst kind-all-the-icons--metadata-get (metadata type-name)
      (or
       (plist-get completion-extra-properties (intern (format ":%s" type-name)))
       (cdr (assq (intern type-name) metadata))))

    (defun kind-all-the-icons-formatted (kind)
      "Format icon kind with all-the-icons"
      (or (alist-get kind kind-all-the-icons--cache)
          (let ((map (assq kind kind-all-the-icons--icons)))
            (let*  ((icon (if map
                              (cdr map)
                            (cdr (assq t kind-all-the-icons--icons))))
                    (half (/ (default-font-width) 2))
                    (pad (propertize " " 'display `(space :width (,half))))
                    (disp (concat pad icon pad)))
              (setf (alist-get kind kind-all-the-icons--cache) disp)
              disp))))

    (defun kind-all-the-icons-margin-formatter (metadata)
      "Return a margin-formatter function which produces kind icons.
METADATA is the completion metadata supplied by the caller (see
info node `(elisp)Programmed Completion').  To use, add this
function to the relevant margin-formatters list."
      (if-let ((kind-func (kind-all-the-icons--metadata-get metadata "company-kind")))
          (lambda (cand)
	        (if-let ((kind (funcall kind-func cand)))
	            (kind-all-the-icons-formatted kind)
	          (kind-all-the-icons-formatted t))))) ;; as a backup
    (add-to-list 'corfu-margin-formatters #'kind-all-the-icons-margin-formatter))


  ;; allow evil-repeat
  ;; https://github.com/minad/corfu/pull/225
  (defun corfu--unread-this-command-keys ()
    (when (> (length (this-command-keys)) 0)
      (setq unread-command-events (nconc
                                   (listify-key-sequence (this-command-keys))
                                   unread-command-events))
      (clear-this-command-keys t)))

  (defun corfu--pre-command ()
    "Insert selected candidate unless command is marked to continue completion."
    (when corfu--preview-ov
      (delete-overlay corfu--preview-ov)
      (setq corfu--preview-ov nil))
    ;; Ensure that state is initialized before next Corfu command
    (when (and (symbolp this-command) (string-prefix-p "corfu-" (symbol-name this-command)))
      (corfu--update))
    (when (and (eq corfu-preview-current 'insert)
               (/= corfu--index corfu--preselect)
               ;; See the comment about `overriding-local-map' in `corfu--post-command'.
               (not (or overriding-terminal-local-map
                        (corfu--match-symbol-p corfu-continue-commands this-command))))
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

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'completion))

(use-package tempel
  :after corfu
  :straight (:host github :repo "minad/tempel")
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
  :hook ((prog-mode . my/set-basic-capf)
         (org-mode . my/set-basic-capf)
         ((lsp-completion-mode eglot-managed-mode lsp-bridge-mode lspce-mode). my/set-lsp-capf))
  :config

  ;;  TODO: Not test whether it works
  (defun cape--limited-table (table)
    (lambda (prefix pred action)
      (if (eq action t)
          (let ((count 0) result)
            (catch 'cape--limit
              (all-completions prefix table
                               (lambda (cand)
                                 (when (or (not pred) (funcall pred cand))
                                   (if (> count 100)
                                       (throw 'cape--limit t)
                                     (cl-incf count)
                                     (when (symbolp cand)
                                       (setq cand (symbol-name cand)))
                                     (push cand result)))
                                 nil)))
            result)
        (complete-with-action action table prefix pred))))


  (defun cape-wrap-limited (capf)
    (pcase (funcall capf)
      (`(,beg ,end ,table . ,plist)
       `(,beg ,end ,(cape--limited-table table) ,@plist))))

  (cape--capf-wrapper limited)


  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     ;; (cape-capf-buster
     (cape-super-capf
      arg-capf
      #'tabnine-capf)
     ;; 'equal)
     #'tmux-capf
     ;; #'cape-dabbrev
     #'eng-capf
     ))

  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (cape-capf-limited (car (last completion-at-point-functions 2))))))

  (defun my/set-lsp-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf
                                               'lsp-capf))
    (when (derived-mode-p 'latex-mode) ;;  HACK: reftex not working in latex-mode
      (add-to-list 'completion-at-point-functions #'+my/reftex-citation-completion)))

  (defun my/set-text-capf ()
    (setq-local completion-at-point-functions (append completion-at-point-functions
                                                      '(capf-english-helper-search))))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package corfu-english-helper
  :after cape
  ;; :bind (("C-x C-e" . corfu-english-helper-search))
  :bind (("C-x C-e" . eng-capf))
  :commands (corfu-english-helper-search)
  :defer t
  :straight (:host github :repo "manateelazycat/corfu-english-helper")
  :config
  (fset 'eng-capf (cape-interactive-capf (cape-capf-limited #'corfu-english-helper-search)))
  )

(use-package tabnine-capf
  :after cape
  :commands (tabnine-capf tabnine-capf-start-process)
  :straight (:host github :repo "50ways2sayhard/tabnine-capf" :files ("*.el" "*.sh" "*.py"))
  :hook ((kill-emacs . tabnine-capf-kill-process))
  :config
  (defalias 'tabnine-capf 'tabnine-completion-at-point))

(use-package tmux-capf
  :after cape
  :commands tmux-capf
  :straight (:host github :repo "theFool32/tmux-capf" :files ("*.el" "*.sh")))

(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-complete ends here
