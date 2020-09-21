;;; init-python.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-python.el
;; Description: Initialize Python
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Mon Jun 10 18:58:02 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Jul 11 22:13:59 2020 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: lsp-python-ms
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes lsp-python-ms
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
  (require 'init-flycheck)
  (require 'init-const))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  (setq python-indent-offset 4)
  (setq python-shell-interpreter "python3")
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  (use-package py-isort
    :defer t
    :init
    (setq python-sort-imports-on-save t)
    (defun +python/python-sort-imports ()
      (interactive)
      (when (and python-sort-imports-on-save
                 (derived-mode-p 'python-mode))
        (py-isort-before-save)))
    (add-hook 'python-mode-hook
              (lambda() (add-hook 'before-save-hook #'+python/python-sort-imports)))
    )
  ;; (use-package company-jedi
  ;;   :config
  ;;   (setq jedi:environment-virtualenv (list (expand-file-name "~/.emacs.d/.python-environments/")))
  ;;   (add-hook 'python-mode-hook 'jedi:setup)
  ;;   (setq jedi:complete-on-dot t)
  ;;   (setq jedi:use-shortcuts t)
  ;;   (defun config/enable-company-jedi ()
  ;;     (add-to-list 'company-backends 'company-jedi))
  ;;   (add-hook 'python-mode-hook 'config/enable-company-jedi)
  ;;   )
  )

;; LSPPythonPac
(use-package lsp-python-ms
  :hook (python-mode . (lambda () (require 'lsp-python-ms) (lsp-deferred)))
  :after lsp-mode python
  :if (or *python3* *python*)
  :custom
  (lsp-python-executable-cmd "python3")
  (lsp-python-ms-dir "~/.local/mspyls/")
  )
;; -LSPPythonPac

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
