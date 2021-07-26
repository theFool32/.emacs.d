;;; init-evil.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-evil.el
;; Description: Introduce evil configuration
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Fri Mar  6 19:52:14 2020 (+0800)
;; Last-Updated:
;;           By:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This file configures evil
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
  (require 'init-const))

(use-package evil
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
  (evil-mode 1)
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

  ;; Force *message* buffer into evil-normal-state to use <spc>
  ;; HACK: donot know why `evil-emacs-state`
  (with-current-buffer "*Messages*"
    (evil-emacs-state))
  (add-hook 'messages-buffer-mode-hook #'(lambda ()
                                           (evil-emacs-state)))

  (with-eval-after-load 'general
    (general-define-key :keymaps 'evil-window-map
                        "C-h" 'evil-window-left
                        "C-j" 'evil-window-down
                        "C-k" 'evil-window-up
                        "C-l" 'evil-window-right)
    )

  (require 'evil/+commands)
  (require 'evil/+packages)
  )

;;


(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
