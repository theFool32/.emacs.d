;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun Feb 27 12:15:55 2022 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d lsp
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes lsp-mode and dap-mode
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

(pcase my-lsp
  ('lsp-bridge
   (use-package lsp-bridge
     :commands (lsp-bridge-enable)
     :straight nil
     :load-path "/Users/lijie/.emacs.d/site-lisp/lsp-bridge/" ;;  TODO: under construction
     :hook ((python-mode c-mode c++-mode LaTeX-mode) . lsp-bridge-enable)
     ))
  ('eglot
   (use-package eglot
     :commands (+eglot-organize-imports +eglot-help-at-point)
     :hook (
            (eglot-managed-mode . (lambda ()
                                    (+lsp-optimization-mode)
                                    (leader-def :keymaps 'override
                                      "ca" '(eglot-code-actions :wk "Code Actions")
                                      "cr" '(eglot-rename :wk "Rename symbol")
                                      "cI" '(eglot-code-action-organize-imports :wk "Organize import")
                                      "ci" '(consult-imenu :wk "imenu")
                                      "cJ" '(consult-eglot-symbols :wk "Symbols in project")
                                      "cd" '(eglot-find-declaration :wk "Jump to definition")
                                      "cF" '(eglot-find-implementation :wk "Find implementation")
                                      "cD" '(eglot-find-typeDefinition :wk "Find type definition"))

                                    (evil-define-key 'normal 'global
                                      "K" '+eglot-help-at-point)
                                    ))
            ((python-mode c-mode c++-mode LaTeX-mode) . eglot-ensure)
            )
     :init
     (require 'lsp/+optimization)
     :config
     (setq eglot-sync-connect 1
           eglot-connect-timeout 10
           eglot-autoshutdown t
           eglot-send-changes-idle-time 0.5
           ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
           ;;      its popup rule causes eglot to steal focus too often.
           eglot-auto-display-help-buffer nil)
     (setq eldoc-echo-area-use-multiline-p nil)
     (setq eglot-stay-out-of '(flymake))
     (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
     (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
     (add-to-list 'eglot-server-programs '((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) "texlab"))

     ;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
     ;;      more problematic approach of deferred to eldoc. Here, I've restored it.
     ;;      Doom's lookup handlers try to open documentation in a separate window
     ;;      (so they can be copied or kept open), but doing so with an eldoc buffer
     ;;      is difficult because a) its contents are generated asynchronously,
     ;;      making them tough to scrape, and b) their contents change frequently
     ;;      (every time you move your cursor).
     (defvar +eglot--help-buffer nil)
     (defun +eglot-lookup-documentation (_identifier)
       "Request documentation for the thing at point."
       (eglot--dbind ((Hover) contents range)
                     (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                      (eglot--TextDocumentPositionParams))
                     (let ((blurb (and (not (seq-empty-p contents))
                                       (eglot--hover-info contents range)))
                           (hint (thing-at-point 'symbol)))
                       (if blurb
                           (with-current-buffer
                               (or (and (buffer-live-p +eglot--help-buffer)
                                        +eglot--help-buffer)
                                   (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
                             (with-help-window (current-buffer)
                               (rename-buffer (format "*eglot-help for %s*" hint))
                               (with-current-buffer standard-output (insert blurb))
                               (setq-local nobreak-char-display nil)))
                         (display-local-help))))
       'deferred)

     (defun +eglot-help-at-point()
       (interactive)
       (+eglot-lookup-documentation nil))
     )
   )
  ('lsp-mode
   ;; LSPPac
   (use-package lsp-mode
     :commands (lsp lsp-deferred)
     :custom
     (lsp-keymap-prefix nil)
     (lsp-enable-indentation nil)
     ;; (lsp-signature-auto-activate nil)
     (lsp-modeline-code-actions-enable nil)
     (lsp-semantic-tokens-enable nil)
     (lsp-headerline-breadcrumb-enable nil)

     (lsp-enable-imenu nil)
     (lsp-idle-delay 0.5)
     (lsp-log-io nil)
     (lsp-enable-folding nil)
     (lsp-auto-guess-root t)
     (lsp-prefer-flymake nil) ; Use flycheck instead of flymake
     (lsp-flycheck-live-reporting nil)
     (lsp-diagnostic-package :none)
     (lsp-enable-snippet nil)
     (lsp-enable-file-watchers nil)
     (lsp-enable-text-document-color nil)
     (lsp-enable-symbol-highlighting nil)
     (lsp-enable-on-type-formatting nil)
     (lsp-restart 'auto-restart)

     (lsp-eldoc-enable-hover t)
     (lsp-eldoc-render-all nil)

     (read-process-output-max (* 1024 1024))
     (lsp-keep-workspace-alive nil)
     (gc-cons-threshold 100000000)
     (lsp-modeline-code-actions-enable nil)
     (lsp-modeline-diagnostics-enable nil)
     (lsp-modeline-workspace-status-enable nil)
     (lsp-completion-provider :none)

     :hook (((python-mode c-mode c++-mode LaTeX-mode) . lsp-deferred)
            (lsp-mode . +my-lsp-setup))
     :init
     (defun +my-lsp-setup ()
       (require 'lsp/+optimization)
       (lsp-enable-which-key-integration)
       (+lsp-optimization-mode +1))
     )

   (use-package lsp-ui
     :after lsp-mode
     :custom-face
     (lsp-ui-doc-background ((t (:background nil))))
     (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
     :hook (lsp-mode . lsp-ui-mode)
     :bind
     (:map lsp-ui-doc-frame-mode-map
           ("C-g" . lsp-ui-doc-unfocus-frame))
     :custom
     (lsp-ui-doc-header nil)
     (lsp-ui-doc-include-signature t)
     (lsp-ui-doc-enable nil)
     (lsp-ui-doc-delay 1)
     (lsp-ui-doc-border (face-foreground 'default))
     (lsp-ui-sideline-enable nil)
     (lsp-ui-sideline-ignore-duplicate t)
     (lsp-ui-sideline-show-code-actions nil)
     (lsp-ui-sideline-show-diagnostics nil)
     (lsp-ui-doc-position 'at-point)
     :config
     (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
     (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
       (setq mode-line-format nil)))
   ;; -LSPPac
   ))

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
