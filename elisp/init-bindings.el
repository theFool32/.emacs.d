;;; init-bindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-func)
  (require 'init-utils))

;; Leader def
(use-package general
  :after evil
  :ensure
  :config
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
    "w" 'evil-avy-goto-char
    "/" 'evilnc-comment-or-uncomment-lines)

  ;; evil mode
  (general-def 'normal
    "/" 'noct-consult-ripgrep-or-line
    "gd" 'xref-find-definitions
    "gD" 'xref-find-references)

  ;; Navigation
  (general-def 'insert
    "C-a" 'beginning-of-line
    "C-e" 'end-of-line)

  (general-def "<escape>" 'keyboard-quit)
  (general-def "C-;" 'embrace-commander)

  (leader-def
    "" nil
    "<SPC>" '(execute-extended-command :wk "M-x")
    "/" '((lambda() (interactive) (affe-grep default-directory)) :wk "Search here")
    "." '(find-file :wk "Find file")
    ";" '(pp-eval-expression :wk "Evil expression")
    ":" '((lambda() (interactive "") (org-agenda nil "n")) :wk "Agenda")
    "x" '(org-capture :wk "Org capture")

    "b" '(:wk "Buffer")
    "b[" '(previous-buffer :wk "Previous buffer")
    "b]" '(next-buffer :wk "Next buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-current-buffer :wk "Kill buffer")
    "bK" '(kill-other-buffers :wk "Kill other buffers")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(evil-write-all :wk "Save all buffer")

    "f" '(:which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fr" '(consult-recent-file :which-key "Recent file")
    "fs" '(save-buffer :which-key "Save file")
    "fd" '(dired :which-key "Find directory")
    "fR" '(my-rename-file :which-key "Rename file")
    "fc" '(my-copy-file :which-key "Copy file")
    "fD" '(my-delete-file :which-key "Delete file")
    "fe" '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :which-key "init.el")
    "fp" '((lambda() (interactive)(find-file (read-file-name ".emacs.d: " "~/.emacs.d/elisp/"))) :which-key ".emacs.d")
    "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :which-key "Org files")

    "j" '(:wk "Jump")
    "jj" '(evil-avy-goto-char :wk "Jump to character")
    "jl" '(evil-avy-goto-line :wk "Jump to line")

    "s" '(:wk "Search")
    "sb" '(my-consult-line-symbol-at-point :wk "Search buffer")
    "si" '(+my-imenu :wk "Jump to symbol")
    "sp" '(affe-grep :wk "Search project")
    "sT" '(load-theme :wk "Load theme")
    "sh" '((lambda() (interactive) (affe-grep default-directory)) :wk "Search here")
    "sd" '(devdocs-lookup-at-point :wk "Search devdocs")
    "sD" '(devdocs-search-at-point :wk "Search devdocs")
    "sP" '(color-rg-search-project :wk "Color-rg Search project")
    "sy" '(color-rg-search-symbol-in-project :wk "Color-rg Search symbol")

    "c" '(:wk "Code")
    "cD" '(xref-find-references :wk "Jump to implementation")
    "cd" '(lsp-ui-peek-find-definitions :wk "Jump to definition")
    "cf" '(format-all-buffer :wk "Format buffer")
    "cr" '(lsp-rename :wk "LSP rename")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "co" '(lsp-ui-imenu :wk "Outline")

    "e" '(:wk "Error")
    "eb" '(flycheck-buffer :wk "Check current buffer")
    "el" '((lambda()(interactive)(call-interactively 'flycheck-list-errors) (select-window (get-buffer-window "*Flycheck errors*"))) :wk "List errors")
    "en" '(flycheck-next-error :wk "Next error")
    "ep" '(flycheck-previous-error :wk "Previous error")
    "ee" '(flycheck-explain-error-at-point :wk "Explain error at point")
    "ev" '(flycheck-verify-setup :wk "Verify setup")
    "es" '(flycheck-select-checker :wk "Select checker")

    "g" '(:wk "Git")
    "gs" '(magit-status :wk "status")
    "ga" '(magit-stage-file :wk "stage file")
    "gp" '(magit-push :wk "push")
    "gc" '(magit-commit :wk "commit")
    "gu" '((lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit))) :wk "stage and commit")
    "gb" '(magit-branch-checkout :wk "checkout")
    "gB" '(magit-blame :wk "blame")
    "gm" '(gitmoji-picker :wk "Gitmoji")
    "gf" '(magit-fetch :wk "fetch")
    "gF" '(magit-pull :wk "pull")


    "t" '(:wk "Toggle")
    "tl" '(toggle-truncate-lines :wk "Toggle line wrap")
    "td" '(toggle-debug-on-error :wk "Toggle debug on error")
    "tt" '(treemacs :wk "Treemacs")
    "tj" '(lsp-treemacs-symbols-toggle :wk "LSP Treemacs")
    "ti" '(imenu-list-smart-toggle :wk "imenu-list")

    "o" '(:wk "Open")
    "oy" '(my-youdao-search-at-point :wk "Youdao")
    "oY" '(youdao-dictionary-search-from-input :wk "Youdao from input")
    ;; "oe" '((lambda() (interactive)(if (get-buffer "vterm") (switch-to-buffer "vterm") (call-interactively #'vterm))) :wk "Shell")
    ;; "ov" '(vterm-other-window :wk "Shell in window")
    "ot" '(org-todo-list :wk "Org Todo")
    "ox" '(org-agenda :wk "Org agenda")
    "ob" '(ebib :wk "Ebib")
    "oB" '(ebib-import-ref :wk "Ebib import")

    "p" '(:wk "Project")
    "pp" '(projectile-switch-project :wk "Switch project")
    "pf" '(consult-projectile :wk "Find file in project")
    "pr" '(projectile-recentf :wk "Recent file in project")
    "pt" '(magit-todos-list :wk "List project tasks")
    "pk" '(projectile-kill-buffers :wk "Kill project buffers")

    "q" '(:wk "Quit")
    ;; "qq" '(kill-emacs :wk "Quit")
    "qq" '(save-buffers-kill-terminal :wk "Quit")
    "qr" '(restart-emacs :wk "Restart")

    "u" '((lambda() (interactive)(call-process-shell-command "rc" nil 0)) :wk "Sync code")
    )
  )

(provide 'init-bindings)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
