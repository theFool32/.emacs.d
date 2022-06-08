;;; init-evil.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

(use-package evil
  :hook (after-init . evil-mode)
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-ex-search-vim-style-regexp t
        evil-ex-substitute-global t
        evil-ex-visual-char-range t  ; column range for ex commands
        evil-mode-line-format 'nil
        ;; more vim-like behavior
        evil-symbol-word-search t
        ;; cursor appearance
        evil-normal-state-cursor 'box
        evil-insert-state-cursor 'bar
        evil-visual-state-cursor 'hollow
        evil-want-keybinding 'nil
        ;; Only do highlighting in selected window so that Emacs has less work
        ;; to do highlighting them all.
        evil-ex-interactive-search-highlight 'selected-window
        evil-split-window-below t
        evil-vsplit-window-right t
        evil-undo-system 'undo-redo)

  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (put 'evil-define-key* 'lisp-indent-function 'defun)

  ;; stop copying each visual state move to the clipboard:
  ;; https://bitbucket.org/lyro/evil/issue/336/osx-visual-state-copies-the-region-on
  ;; grokked from:
  ;; http://stackoverflow.com/questions/15873346/elisp-rename-macro
  (advice-add #'evil-visual-update-x-selection :override #'ignore)

  ;; Start help-with-tutorial in emacs state
  (advice-add #'help-with-tutorial :after (lambda (&rest _) (evil-emacs-state +1)))

  ;; Allows you to click buttons without initiating a selection
  (define-key evil-motion-state-map [down-mouse-1] nil)

  (with-eval-after-load 'general
    (general-define-key :keymaps 'evil-window-map
                        "C-h" 'evil-window-left
                        "C-j" 'evil-window-down
                        "C-k" 'evil-window-up
                        "C-l" 'evil-window-right))

  (require 'evil/+packages))


(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
