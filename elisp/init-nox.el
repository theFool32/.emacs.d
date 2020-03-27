;;; package --- Summary
;;; init-nox.el ---
;;
;; Filename: init-nox.el
;; Description: For nox lsp
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Fri Mar 27 17:25:02 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 19
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(use-package nox
  :quelpa (nox :fetcher github :repo "manateelazycat/nox")
  :hook (python-mode . nox-ensure)
  :config

  (defun push-tabnine ()
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    ;; (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
    (setq company-backends '((company-capf :with company-tabnine :separate) company-files company-dabbrev))
    )
  (add-hook 'nox-managed-mode-hook #'push-tabnine)

  (defvar lsp-python "/usr/bin/python3")
  (defvar lsp-search-paths [])

  (defclass nox-mspyls (nox-lsp-server) ()
    :documentation
    "MS Python Language Server.")

  (setq-default nox-workspace-configuration
                '((:python :autoComplete (:extraPaths nil)
                           :analysis (:autoSearchPaths :json-false :usePYTHONPATH :json-false))))

  (cl-defmethod nox-initialization-options ((_server nox-mspyls))
    `(:interpreter
      (:properties
       (:InterpreterPath ,lsp-python))
      :searchPaths ,lsp-search-paths))

  (add-to-list 'nox-server-programs
               `(python-mode nox-mspyls
                             "/home/lijie/.local/mspyls/Microsoft.Python.LanguageServer"))
  )

(provide 'init-nox)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-nox.el ends here
