(eval-when-compile
  (require 'init-const))

;; evil mode
(setq evil-want-keybinding nil)
(evil-define-key 'normal 'global
  "j" 'evil-next-visual-line
  "k" 'evil-previous-visual-line
  ",w" 'evil-avy-goto-word-0
  "/" 'swiper
  ",/" 'evilnc-comment-or-uncomment-lines
  "gd" 'xref-find-definitions
  "gD" 'xref-find-references
  )
(evil-define-key 'visual 'global
  ",/" 'evilnc-comment-or-uncomment-lines)
(evil-define-key 'normal org-mode-map (kbd "<tab>") #'org-cycle)

;; Navigation
(define-key evil-insert-state-map (kbd "C-a") 'beginning-of-line)
(define-key evil-insert-state-map (kbd "C-e") 'end-of-line)
                                        ;(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(defun smarter-yas-expand-next-field-complete ()
  "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
  (interactive)
  (if yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (progn
              (call-interactively 'company-abort)
              (call-interactively 'company-yasnippet))
            )
          ))
    ;; FIXME: c-k tab c-k
    (company-complete-common)
    )
  )
(with-eval-after-load 'company
  ;; (define-key evil-insert-state-map (kbd "C-k") 'smarter-yas-expand-next-field-complete)
  (general-define-key
   :keymaps '(company-active-map evil-insert-state-map)
   "C-k" 'smarter-yas-expand-next-field-complete)
  (general-define-key
   :states 'insert
   :prefix "C-x"
   "C-f" 'company-files
   "C-s" 'company-ispell
   )
  )

;; Leader def
(use-package general
  :ensure t
  :config
  (general-create-definer leader-def
    ;; FIXME: not work in *Message*
    :states '(normal insert visual emacs motion)
    :keymaps 'override
    ;; :keymaps '(normal motion visual emacs)
    :prefix "SPC"
    :non-normal-prefix "C-,"
    )
  )

(leader-def
  "" nil
  "<SPC>" '(counsel-projectile :wk "Project find file")
  "/" '(counsel-rg :wk "Search here")
  "." '(counsel-find-file :wk "Find file")
  ";" '(execute-extended-command :wk "M-x")
  ":" '(pp-eval-expression :wk "Evil expression")
  "x" '(org-capture :wk "Org capture")

  "b" '(:wk "Buffer")
  "b[" '(previous-buffer :wk "Previous buffer")
  "b]" '(next-buffer :wk "Next buffer")
  "bb" '(ivy-switch-buffer :wk "Switch buffer")
  "bk" '(kill-current-buffer :wk "Kill buffer")
  "bs" '(basic-save-buffer :wk "Save buffer")
  "bS" '(evil-write-all :wk "Save all buffer")

  "f" '(:which-key "Files")
  "ff" '(find-file :which-key "Find file")
  "fr" '(counsel-recentf :which-key "Recent file")
  "fs" '(save-buffer :which-key "Save file")
  "fd" '(dired :which-key "Find directory")
  "fR" '(rename-file :which-key "Rename file")
  ;; "fl" '(locate-file :which-key "Locate file")
  "fp" '((lambda() (interactive)(counsel-find-file "~/.emacs.d/elisp/")) :which-key ".emacs.d")

  "j" '(:wk "Jump")
  "jj" '(evil-avy-goto-char :wk "Jump to character")
  "jl" '(evil-avy-goto-line :wk "Jump to line")

  "s" '(:wk "Search")
  "sb" '(swiper :wk "Search buffer")
  "sf" '(locate :wk "Locate file")
  "si" '(imenu :wk "Jump to symbol")
  "sp" '(counsel-projectile-rg :wk "Search project")
  "sT" '(load-theme :wk "Load theme")
  "sd" '(counsel-rg :wk "Search here")

  "c" '(:wk "Code")
  "cD" '(lsp-ui-peek-find-references :wk "Jump to implementation")
  "cd" '(evil-goto-definition :wk "Jump to definition")
  "cf" '(format-all-buffer :wk "Format buffer")
  "ci" '(lsp-organize-imports :wk "Organize import")
  "cr" '(lsp-rename :wk "LSP rename")
  "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
  "co" '(lsp-ui-imenu :wk "Outline")
  "es" '(flycheck-select-checker :wk "Select checker")


  "e" '(:wk "Error")
  "eb" '(flycheck-buffer :wk "Check current buffer")
  "el" '((lambda()(interactive)(call-interactively 'flycheck-list-errors) (select-window (get-buffer-window "*Flycheck errors*"))) :wk "List errors")
  "en" '(flycheck-next-error :wk "Next error")
  "ep" '(flycheck-previous-error :wk "Previous error")
  "ee" '(flycheck-explain-error-at-point :wk "Explain error at point")
  "ev" '(flycheck-verify-setup :wk "Verify setup")

  "g" '(:wh "Git")
  "gs" '(magit-status :wk "Git status")
  "ga" '(magit-stage-file :wk "Git stage file")
  "gp" '(magit-push :wk "Git push")
  "gc" '(magit-commit :wk "Git commit")
  "gu" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "Git stage and commit")
  "gb" '(magit-branch-checkout :wk "Git checkout")
  "gB" '(magit-blame :wk "Git blame")
  "gm" '(gitmoji-picker :wk "Gitmoji")
  "gf" '(magit-fetch :wk "Git fetch")
  "gF" '(magit-pull :wk "Git pull")


  "o" '(:wk "Open")
  "op" '(treemacs :wk "Treemacs")
  "oy" '(my-youdao-search-at-point :wk "Youdao")
  "oe" '(shell-here :wk "Shell")
  ;; "og" '(google-this :wk "Google")
  "ot" '(org-todo-list :wk "Org Todo")
  "ox" '(org-agenda :wk "Org agenda")

  "p" '(:wk "Project")
  "pp" '(projectile-switch-project :wk "Switch project")
  "pf" '(counsel-projectile :wk "Find file in project")
  "pr" '(projectile-recentf :wk "Recent file in project")
  "pt" '(magit-todos-list :wk "List project tasks")

  "q" '(:wk "Quit")
  "qq" '(kill-emacs :wk "Quit")
  "qr" '(restart-emacs :wk "Restart")

  "u" '((lambda() (interactive)(call-process-shell-command "rc" nil 0)) :wk "Sync code")
  )

(provide 'init-bindings)
