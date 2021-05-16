;;; init-bindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-func))

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
    "/" 'swiper
    "gd" 'xref-find-definitions
    "gD" 'xref-find-references)

  ;; Navigation
  (general-def 'insert
    "C-a" 'beginning-of-line
    "C-e" 'end-of-line)

  (leader-def
    "" nil
    "<SPC>" '(counsel-projectile :wk "Project find file")
    ;; "<SPC>" '(snails :wk "Project find file")
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
    "bK" '(kill-other-buffers :wk "Kill other buffers")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(evil-write-all :wk "Save all buffer")

    "f" '(:which-key "Files")
    "ff" '(find-file :which-key "Find file")
    "fr" '(counsel-recentf :which-key "Recent file")
    "fs" '(save-buffer :which-key "Save file")
    "fd" '(dired :which-key "Find directory")
    "fR" '(rename-file :which-key "Rename file")
    "fc" '(copy-file :which-key "Copy file")
    "fD" '(delete-file :which-key "Delete file")
    "fe" '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :which-key "init.el")
    "fp" '((lambda() (interactive)(counsel-find-file "~/.emacs.d/elisp/")) :which-key ".emacs.d")
    "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :which-key "Org files")

    "j" '(:wk "Jump")
    "jj" '(evil-avy-goto-char :wk "Jump to character")
    "jl" '(evil-avy-goto-line :wk "Jump to line")

    "s" '(:wk "Search")
    "sb" '(swiper :wk "Search buffer")
    ;; "sf" '(locate :wk "Locate file")
    "si" '(imenu :wk "Jump to symbol")
    "sp" '(counsel-projectile-rg :wk "Search project")
    "sT" '(load-theme :wk "Load theme")
    "sd" '(counsel-rg :wk "Search here")

    "c" '(:wk "Code")
    "cD" '(xref-find-references :wk "Jump to implementation")
    "cd" '(lsp-ui-peek-find-definitions :wk "Jump to definition")
    "cf" '(format-all-buffer :wk "Format buffer")
    "ci" '(color-rg-search-input-in-project :wk "Color-rg search")
    "cr" '(lsp-rename :wk "LSP rename")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "co" '(lsp-ui-imenu :wk "Outline")
    "cJ" '(lsp-ivy-global-workspace-symbol :wk "Jump to Symbol in workspace")

    "e" '(:wk "Error")
    "eb" '(flycheck-buffer :wk "Check current buffer")
    "el" '((lambda()(interactive)(call-interactively 'flycheck-list-errors) (select-window (get-buffer-window "*Flycheck errors*"))) :wk "List errors")
    "en" '(flycheck-next-error :wk "Next error")
    "ep" '(flycheck-previous-error :wk "Previous error")
    "ee" '(flycheck-explain-error-at-point :wk "Explain error at point")
    "ev" '(flycheck-verify-setup :wk "Verify setup")
    "es" '(flycheck-select-checker :wk "Select checker")

    "g" '(:wh "Git")
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


    "t" '(:wh "Toggle")
    "tl" '(toggle-truncate-lines :wh "Toggle line wrap")

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
    "pf" '(counsel-projectile :wk "Find file in project")
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
