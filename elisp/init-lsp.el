;;; init-lsp.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-lsp.el
;; Description: Initialize LSP
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:42:09 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Sep 11 01:32:58 2021 (+0800)
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
  (lsp-eldoc-render-all t)

  (read-process-output-max (* 1024 1024))
  (lsp-keep-workspace-alive nil)
  (gc-cons-threshold 100000000)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-modeline-workspace-status-enable nil)
  (lsp-completion-provider :none)

  :hook (((python-mode c-mode c++-mode) . lsp-deferred)
         (lsp-mode . +my-lsp-setup))
  :init
  (defun +my-lsp-setup ()
    (require 'lsp/+optimization)
    (lsp-enable-which-key-integration)
    (+lsp-optimization-mode +1))
  )
;; -LSPPac

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
