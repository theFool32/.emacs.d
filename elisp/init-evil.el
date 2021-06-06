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
  :hook (after-init . evil-mode)
  :demand t
  :init
  (setq evil-want-keybinding nil)
  :preface
  (setq evil-want-visual-char-semi-exclusive t
        evil-want-C-i-jump 'nil
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
        evil-vsplit-window-right t)

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

  (general-define-key :keymaps 'evil-window-map
                      "C-h" 'evil-window-left
                      "C-j" 'evil-window-down
                      "C-k" 'evil-window-up
                      "C-l" 'evil-window-right)
  )

;;
;;; Packages


(use-package evil-easymotion
  :commands evilem-create evilem-default-keybindings
  :config
  ;; Use evil-search backend, instead of isearch
  (evilem-make-motion evilem-motion-search-next #'evil-ex-search-next
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-previous #'evil-ex-search-previous
                      :bind ((evil-ex-search-highlight-all nil)))

  (evilem-make-motion evilem-motion-search-word-forward #'evil-ex-search-word-forward
                      :bind ((evil-ex-search-highlight-all nil)))
  (evilem-make-motion evilem-motion-search-word-backward #'evil-ex-search-word-backward
                      :bind ((evil-ex-search-highlight-all nil))))


(use-package evil-embrace
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
  :hook (after-init . evil-escape-mode)
  :commands evil-escape
  :init
  (setq evil-escape-excluded-states '(normal visual multiedit emacs motion)
        evil-escape-excluded-major-modes '(neotree-mode treemacs-mode vterm-mode)
        evil-escape-key-sequence "jk"
        evil-escape-delay 0.15)
  (evil-define-key* '(insert replace visual operator) 'global "\C-g" #'evil-escape)
  :config
  ;; no `evil-escape' in minibuffer
  (add-hook 'evil-escape-inhibit-functions #'minibufferp)
  ;; so that evil-escape-mode-hook runs, and can be toggled by evil-mc
  (evil-escape-mode +1))


(use-package evil-nerd-commenter
  :commands (evilnc-comment-operator
             evilnc-inner-comment
             evilnc-outer-commenter))


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
  :config (global-evil-surround-mode 1))


;; for visualization like substitute
(use-package evil-traces
  :after evil-ex
  :config
  (evil-traces-mode))


;; Allows you to use the selection for * and #
(use-package evil-visualstar
  :commands (evil-visualstar/begin-search
             evil-visualstar/begin-search-forward
             evil-visualstar/begin-search-backward)
  :init
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))


(use-package evil-collection
  :after evil
  :init
  (setq evil-want-keybinding 'nil)
  :preface
  (setq evil-want-keybinding 'nil)
  :config
  (evil-collection-init))

;; indent textobj
(use-package evil-indent-plus)
;; in/decrease number
;; (use-package evil-numbers)

(require 'evil/+commands)

(provide 'init-evil)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-bindings.el ends here
