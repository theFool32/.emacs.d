;;; init-bindings.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(eval-when-compile
  (require 'init-const)
  (require 'init-func)
  (require 'init-utils))

;; Leader def
(use-package general
  ;; :after evil
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
    "w" 'evil-avy-goto-word-1
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
    "/" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "?" '(+consult-ripgrep-at-point :wk "Search symbol here")
    "." '(find-file :wk "Find file")
    ";" '(pp-eval-expression :wk "Evil expression")
    ":" '(+my/open-org-agenda :wk "Agenda")
    "x" '(org-capture :wk "Org capture")
    "r" '(er/expand-region :wk "expand-region")

    "b" '(:wk "Buffer")
    "b[" '(previous-buffer :wk "Previous buffer")
    "b]" '(next-buffer :wk "Next buffer")
    "bb" '(switch-to-buffer :wk "Switch buffer")
    "bk" '(kill-current-buffer :wk "Kill buffer")
    "bq" '(evil-quit :wk "evil-quit")
    "bK" '(kill-other-buffers :wk "Kill other buffers")
    "bs" '(basic-save-buffer :wk "Save buffer")
    "bS" '(evil-write-all :wk "Save all buffer")

    "f" '(:wk "Files")
    "ff" '(find-file :wk "Find file")
    "fr" '(my-open-recent :wk "Recent file")
    "fs" '(save-buffer :wk "Save file")
    "fd" '(dired-jump :wk "Current directory")
    "fR" '(my-rename-file :wk "Rename file")
    "fc" '(my-copy-file :wk "Copy file")
    "fD" '(my-delete-file :wk "Delete file")
    "fe" '((lambda() (interactive)(find-file "~/.emacs.d/init.el")) :wk "init.el")
    "fp" '((lambda() (interactive)(find-file (read-file-name ".emacs.d: " "~/.emacs.d/elisp/"))) :wk ".emacs.d")
    ;; "fo" '((lambda() (interactive)(find-file +org-capture-file-gtd)) :wk "Org files")
    "fo" '((lambda() (interactive)(find-file (completing-read "Open org fiels:"
                                                         (list +org-capture-file-gtd
                                                               +org-capture-file-tickler
                                                               +org-capture-file-someday
                                                               +org-capture-file-note
                                                               +org-capture-file-idea)))) :wk "Org files")
    "fh" '((lambda() (interactive)(consult-fd default-directory)) :wk "Find file here")
    "fH" '((lambda() (interactive)(find-file (read-file-name "Remote: " "/scp:"))) :wk "Remote")

    "fE" '(:wk "File Encoding")
    "fEr" '(revert-buffer-with-coding-system :wk "Revert encoding")
    "fEs" '(set-buffer-file-coding-system :wk "Set encoding")

    "j" '(:wk "Jump")
    "jj" '(evil-avy-goto-char :wk "Jump to character")
    "jl" '(evil-avy-goto-line :wk "Jump to line")
    "je" '(+vertico/jump-list :wk "Jump-list")

    "s" '(:wk "Search")
    "sb" '(my-consult-line-symbol-at-point :wk "Search buffer")
    "si" '(+my-imenu :wk "Jump to symbol")
    "sp" '(consult-ripgrep :wk "Search project")
    "sT" '(load-theme :wk "Load theme")
    "sh" '((lambda() (interactive) (consult-ripgrep default-directory)) :wk "Search here")
    "sd" '(devdocs-lookup-at-point :wk "Search devdocs")
    "sD" '(devdocs-search-at-point :wk "Search devdocs")
    "sP" '(color-rg-search-project :wk "Color-rg Search project")
    "sy" '(color-rg-search-symbol-in-project :wk "Color-rg Search symbol")

    "c" '(:wk "Code")
    "cD" '(xref-find-references :wk "Jump to implementation")
    "cf" '(format-all-buffer :wk "Format buffer")
    "cr" '(lsp-rename :wk "LSP rename")
    "cw" '(delete-trailing-whitespace :wk "Delete trailing whitespace")
    "cm" '(symbol-overlay-put :wk "Mark")

    "e" '(:wk "Error")
    "eb" '(flycheck-buffer :wk "Check current buffer")
    "el" '((lambda()(interactive)(call-interactively 'flycheck-list-errors) (select-window (get-buffer-window "*Flycheck errors*"))) :wk "List errors")
    "en" '(flycheck-next-error :wk "Next error")
    "ep" '(flycheck-previous-error :wk "Previous error")
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

    "w" '(:wk "Window")
    ;; :sp
    "wH" '(evil-window-split :wk "Split window vertically")
    "-" '(evil-window-split :wk "Split window vertically")
    ;; :vsp
    "wv" '(evil-window-vsplit :wk "Split window horizontally")
    "\\" '(evil-window-vsplit :wk "Split window horizontally")
    "wj" '(evil-window-down :wk "Focus window down")
    "wk" '(evil-window-up :wk "Focus window up")
    "wh" '(evil-window-left :wk "Focus window left")
    "wl" '(evil-window-right :wk "Focus window right")
    "w=" '(balance-windows :wk "balance windows")
    "wq" '(evil-quit :wk "close window")

    "t" '(:wk "Toggle")
    "tl" '(toggle-truncate-lines :wk "Line wrap")
    "td" '(toggle-debug-on-error :wk "Debug on error")
    "tt" '(dirvish :wk "Dirvish")
    "te" '(vterm-posframe-toggle :wk "Shell")
    "tc" '(olivetti-mode :wk "Center")

    "o" '(:wk "Open")
    "oy" '(my-youdao-search-at-point :wk "Youdao")
    "oY" '(youdao-dictionary-search-from-input :wk "Youdao from input")
    "oe" '((lambda() (interactive)(if (get-buffer "vterm") (switch-to-buffer "vterm") (call-interactively #'vterm))) :wk "Shell")
    "ov" '(vterm-other-window :wk "Shell in window")
    "ot" '(org-todo-list :wk "Org Todo")
    "ox" '(org-agenda :wk "Org agenda")
    "ob" '(ebib :wk "Ebib")
    "oB" '(ebib-import-ref :wk "Ebib import")
    "oc" '(cfw:open-org-calendar :wk "Calendar")
    "oi" '(consult-clock-in :wk "Clock in")
    "oo" '(org-clock-out :wk "Clock out")

    "p" '(:wk "Project")
    "pp" '(project-switch-project :wk "Switch project")
    "pf" '(consult-project-extra-find :wk "Find file in project")
    "pt" '(magit-todos-list :wk "List project tasks")
    "pk" '(project-kill-buffers :wk "Kill project buffers")

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
