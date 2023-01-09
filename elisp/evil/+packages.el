;;; +packages.el ---


(use-package evil-embrace
  :after evil
  :commands (embrace-add-pair embrace-add-pair-regexp)
  :config
  (require 'evil/+embrace)

  (add-hook 'LaTeX-mode-hook 'embrace-LaTeX-mode-hook)
  (add-hook 'org-mode-hook 'embrace-org-mode-hook)
  (add-hook 'emacs-lisp-mode-hook 'embrace-emacs-lisp-mode-hook)
  (add-hook 'c++-mode-hook '+evil-embrace-angle-bracket-modes-hook-h)

  (with-eval-after-load 'evil-surround
    (evil-embrace-enable-evil-surround-integration))

  (defun +evil-embrace-latex-mode-hook-h ()
    (embrace-add-pair-regexp ?l "\\[a-z]+{\" \"}" #'+evil--embrace-latex))

  (defun +evil-embrace-lisp-mode-hook-h ()
    ;; Avoid `embrace-add-pair-regexp' because it would overwrite the default
    ;; `f' rule, which we want for other modes
    (push (cons ?f (make-embrace-pair-struct
                    :key ?f
                    :read-function #'+evil--embrace-elisp-fn
                    :left-regexp "([^ ]+ \"
                    :right-regexp \")"))
          embrace--pairs-list))

  (defun +evil-embrace-angle-bracket-modes-hook-h ()
    (let ((var (make-local-variable 'evil-embrace-evil-surround-keys)))
      (set var (delq ?< evil-embrace-evil-surround-keys))
      (set var (delq ?> evil-embrace-evil-surround-keys)))
    (embrace-add-pair ?> "<" ">"))

  ;; Add escaped-sequence support to embrace
  (setf (alist-get ?\\ (default-value 'embrace--pairs-list))
        (make-embrace-pair-struct
         :key ?\\
         :left-regexp "\\[[{(]"
         :right-regexp "\\[]})]"))
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
             evilnc-comment-or-uncomment-lines))


;; for search
;; key: f
(use-package evil-snipe
  :commands (evil-snipe-mode
             evil-snipe-override-mode
             evil-snipe-local-mode
             evil-snipe-override-local-mode)
  :init
  (setq evil-snipe-smart-case t
        evil-snipe-scope 'line
        evil-snipe-repeat-scope 'visible
        evil-snipe-char-fold t)
  :config
  (evil-snipe-mode +1)
  (evil-snipe-override-mode +1))


(use-package evil-surround
  :after evil
  :commands (global-evil-surround-mode
             evil-surround-edit
             evil-Surround-edit
             evil-surround-region)
  :config (global-evil-surround-mode 1))


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
  ;;  TODO: init when loading specific package
  (let ((modes '(atomic-chrome calc calendar consult devdocs diff-hl diff-mode dired doc-view ebib edebug ediff eglot eldoc elisp-mode eval-sexp-fu evil-mc flymake  git-timemachine gnus grep help helpful buffer image image-dired image+ imenu imenu-list (indent "indent")  info log-view man (magit magit-repos magit-submodule) magit-section magit-todos markdown-mode mu4e mu4e-conversation org (pdf pdf-view) popup proced (process-menu simple) profiler python reftex replace rtags sh-script shortdoc so-long tab-bar tablist tabulated-list tar-mode thread tide timer-list vc-annotate vc-dir vc-git vdiff vertico view vterm vundo wdired wgrep which-key xref yaml-mode (ztree ztree-diff ztree-dir))))
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

(use-package evil-textobj-tree-sitter
  :after (tree-sitter evil)
  :straight (evil-textobj-tree-sitter :type git
                                      :host github
                                      :repo "meain/evil-textobj-tree-sitter"
                                      :files (:defaults "queries"))
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

(use-package evil-mc
  ;;  FIXME: still not easy to use, need finetune
  :after evil
  :hook (+my/first-input . global-evil-mc-mode)
  :config
  (global-set-key (kbd "s-<mouse-1>") 'evil-mc-toggle-cursor-on-click)
  )

(provide 'evil/+packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +packages.el ends here
