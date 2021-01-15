;;; bindings.el -*- lexical-binding: t; -*-

(when IS-MAC (setq mac-command-modifier 'meta
                   mac-option-modifier  'alt))

(setq doom-localleader-key ",")
(map! :leader :nvm "g C" nil)

;; Distinguish C-i from TAB
(when (display-graphic-p)
  (define-key input-decode-map "\C-i" [C-i])
  (map! "<C-i>" #'better-jumper-jump-forward))

(map! :map global-map
      :localleader
      :desc "goto word" "w" #'evil-avy-goto-char
      :desc "comment" "/" #'comment-line

      :leader
      :desc "Eval expression"       ":"    #'eval-expression
      :desc "M-x"                   ";"    #'execute-extended-command
      :desc "Org Capture"           "x"    #'org-capture

      (:prefix-map ("g" . "git")
        (:when (featurep! :ui vc-gutter)
          :desc "Git stage hunk"            "S"   #'git-gutter:stage-hunk
          )
        (:when (featurep! :tools magit)
          :desc "Magit commit"      "c"   #'magit-commit
          :desc "Magit stage file"  "s"   #'magit-stage-file
          :desc "Magit push"        "p"   #'magit-push
          :desc "Magit clone"        "n"   #'magit-clone
          )
        (:prefix ("C" . "create")
          :desc "Initialize repo"           "r"   #'magit-init
          :desc "Clone repo"                "R"   #'+magit/clone
          :desc "Commit"                    "c"   #'magit-commit-create
          :desc "Branch"                    "b"   #'magit-branch-and-checkout
          :desc "Issue"                     "i"   #'forge-create-issue
          :desc "Pull request"              "p"   #'forge-create-pullreq)
        )
      )

(map!
 (:after company
  (:map (company-active-map evil-insert-state-map)
   "C-k" #'smarter-yas-expand-next-field-complete)
  ))

(defun disable-all-minor-modes ()
  (interactive)
  (mapc
   (lambda (mode-symbol)
     (when (functionp mode-symbol)
       ;; some symbols are functions which aren't normal mode functions
       (ignore-errors
         (funcall mode-symbol -1))))
   minor-mode-list))

(map!
 ;; :gi "C-n" #'next-line
 ;; :gi "C-p" #'previous-line
 ;; :gi "C-b" #'backward-char
 ;; :gi "C-f" #'forward-char
 ;; :gi "C-k" #'kill-line
 ;; :gi "C-d" #'delete-forward-char
 ;; :gi "C-<backspace>" #'delete-block-backward

 :v "DEL" (kbd "\"_d")
 :v "<del>" (kbd "\"_d")
 :v "<backspace>" (kbd "\"_d")
 :nmv "-" (λ! (better-jumper-jump-backward 1))
 :nmv "=" (λ! (better-jumper-jump-forward 1))
 :nmv "/" #'swiper
 :nmv "C-h C-m" #'(lambda() (interactive)(disable-all-minor-modes))
 )

(map! :leader
      :desc "project-find-file" :nmv "SPC" #'projectile-find-file
      :desc "Sync code" :nmv "r" #'(lambda() (interactive)(call-process-shell-command "rc" nil 0))

      (:prefix "c"
       :desc "Toggle Comment" "/" #'doom/toggle-comment-region-or-line
       :desc "Glance doc" "g" #'lsp-ui-doc-glance
       )
      (:prefix "TAB"
       :desc "Switch workspace" "TAB" #'+workspace/other)
      (:prefix "f"
       :desc "Save all" "S" #'evil-write-all
       :desc "Deer" "j" #'deer
       :desc "Org file" "o" #'(lambda() (interactive)(find-file "~/Dropbox/org-notes/main.org"))
       )
      (:prefix "g"
       "s" nil
       :desc "Status" "s" #'magit-status
       :desc "Gitmoji picker" "m" #'gitmoji-picker
       :desc "Stage file" "a" #'magit-stage-file
       :desc "Stage and Commit" "u" #'(lambda() (interactive)(progn (call-interactively 'magit-stage-file) (call-interactively 'magit-commit)))
       )
      (:prefix "h"
       :desc "Helpful command" "C" #'helpful-command)
      (:prefix-map ("j" . "jump")
       :desc "Jump to character" "j" #'evil-avy-goto-char-timer
       :desc "Jump to line" "l" #'evil-avy-goto-line
       :desc "Jump to character 2" "J" #'evil-avy-goto-char-2)
      (:prefix-map ("e" . "error")
       :desc "Flymake next error"      "N" #'flymake-goto-next-error
       :desc "Flymake previous error"  "P" #'flymake-goto-prev-error
       :desc "Flymake list errors"     "L" #'flymake-show-diagnostics-buffer
       :desc "Flycheck next error"     "n" #'flycheck-next-error
       :desc "Flycheck previous error" "p" #'flycheck-previous-error
       :desc "Flycheck explain error"  "e" #'flycheck-explain-error-at-point
       :desc "Flycheck list errors"    "l" #'flycheck-list-errors
       :desc "Flycheck verify setup"   "v" #'flycheck-verify-setup)
      (:prefix "l"
       :desc "Treemacs Symbols" "i" #'lsp-treemacs-symbols
       :desc "Treemacs References" "r" #'lsp-treemacs-references
       :desc "Treemacs Errors" "e" #'lsp-treemacs-errors-list)
      (:prefix "o"                      ; open
       :desc "Kill ring"             "k" #'helm-show-kill-ring
       :desc "Treemacs Symbols"      "i" #'lsp-treemacs-symbols
       :desc "Open link"             "x" #'link-hint-open-link
       :desc "Open link at point"    "X" #'link-hint-open-link-at-point
       :desc "Vterm"                 "s" #'+vterm/toggle
       :desc "Project run Vterm"     "S" #'+vterm/here
       :desc "Toggle eshell popup"   "e" #'+eshell/toggle
       :desc "Project run Eshell"    "E" #'projectile-run-eshell
       :desc "Youdao dictionary"     "y" #'youdao-dictionary-search-at-point-tooltip
       :desc "Youdao play voice"     "Y" #'youdao-dictionary-play-voice-at-point
       :desc "Docker open apps"      ";" #'+docker/reveal-in-apps
       :desc "Org Todo"              "t" #'org-todo-list
       :desc "Ebib"                  "b" #'ebib
       (:when IS-MAC
        :desc "Reveal in default program"  "f" #'+macos/open-in-default-program
        :desc "Reveal in Finder"           "o" #'+macos/reveal-in-finder
        :desc "Reveal project in Finder"   "O" #'+macos/reveal-project-in-finder
        ;; :desc "Reveal in Terminal"         "t" #'+macos/reveal-in-terminal
        ;; :desc "Reveal project in Terminal" "T" #'+macos/reveal-project-in-terminal
        :desc "Reveal file in Apps"        "," #'+shell/reveal-in-apps
        :desc "Reveal project in Apps"     "." #'+shell/reveal-project-in-apps))
      (:prefix "p"                      ; project
       :desc "Update projectile list" "u" #'update-projectile-known-projects)
      (:prefix ("d" . "debug")
       "b" #'dap-breakpoint-toggle
       "h" #'dap-hydra
       "l" #'dap-ui-locals
       "s" #'dap-ui-sessions
       "k" #'dap-delete-session
       "K" #'dap-delete-all-sessions
       "S" #'realgud-short-key-mode)
      (:prefix "t"                      ; toggle
       "c" #'centered-window-mode
       "d" #'toggle-debug-on-error
       "L" #'toggle-truncate-lines
       "S" #'size-indication-mode
       "I" #'ivy-rich-mode
       "v" #'visual-line-mode)
      (:prefix "s"                      ; search
       (:when IS-LINUX
        :desc "Open App" "a" #'counsel-linux-app)
       (:when IS-MAC
        :desc "Open App" "a" #'counsel-osx-app)
       :desc "Comments"  "c" #'counsel-imenu-comments
       :desc "Search project with regex" "P" #'+default/search-project-regex
       :desc "Project (hidden)" "h" #'+ivy/project-search-with-hidden-files))

(map!
 (:after lsp-ui
  :map lsp-ui-mode-map
  "C-j" (λ!! #'lsp-ui-doc-mode))
 (:after lsp-ui-peek
  :map lsp-ui-peek-mode-map
  "h" #'lsp-ui-peek--select-prev-file
  "j" #'lsp-ui-peek--select-next
  "k" #'lsp-ui-peek--select-prev
  "l" #'lsp-ui-peek--select-next-file)
 (:after python
  :localleader
  :map python-mode-map
  (:prefix ("i" . "Import")
   :desc "Import at point" "i" #'importmagic-fix-symbol-at-point
   :desc "Import all"      "a" #'importmagic-fix-imports
   :desc "Sort imports"    "s" #'+python/python-sort-imports)
  (:prefix ("p" . "Poetry")
   :desc "Poetry Menu" "p" #'poetry
   :desc "Add" "a" #'poetry-add
   :desc "Lock" "l" #'poetry-lock
   :desc "Show" "s" #'poetry-show
   :desc "virtualenv" "v" #'poetry-venv-toggle
   )
  (:prefix ("t" . "Test")
   :desc "Test file" "f" #'python-pytest-file
   :desc "Test function" "t" #'python-pytest-function
   :desc "Test last failed" "l" #'python-pytest-last-failed
   :desc "Popup test panel" "p" #'python-pytest-popup
   :desc "Test repeat" "r" #'python-pytest-repeat
   )
  :desc "Docstring" "d" #'sphinx-doc
  (:prefix ("v" . "ENV")
   "w" #'pyvenv-workon
   "v" #'pyvenv-activate
   "V" #'pyvenv-deactivate
   ))
 )

(map!
 (:map awesome-pair-mode-map
  "M-p" #'awesome-pair-jump-left
  "M-n" #'awesome-pair-jump-right
  "M-:" #'awesome-pair-jump-out-pair-and-newline

  "M-{" #'awesome-pair-wrap-curly
  "M-[" #'awesome-pair-wrap-bracket
  "M-(" #'awesome-pair-wrap-round
  "M-)" #'awesome-pair-unwrap)
 )
