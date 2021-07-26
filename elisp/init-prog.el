;;; init-prog.el ---
;;
;; Filename: init-prog.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Tue Jul  6 10:42:00 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 11
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

(use-package devdocs
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :config
  (add-hook 'python-mode-hook
            (lambda() (setq-local devdocs-current-docs '("python~3.9" "PyTorch" "NumPy~1.20"))))
  (defun devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))
  (defun devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol))))

(use-package citre
  :disabled
  :straight (:type git :host github :repo "universal-ctags/citre")
  :bind (("C-x c j" . citre-jump+)
         ("C-x c k" . citre-jump-back)
         ("C-x c p" . citre-peek)
         ("C-x c a" . citre-ace-peek))
  :hook (prog-mode . citre-auto-enable-citre-mode)
  :config
  (defun citre-jump+ ()
    (interactive)
    (condition-case _
        (citre-jump)
      (error (call-interactively #'xref-find-definitions))))

  (with-eval-after-load 'projectile
    (setq citre-project-root-function #'projectile-project-root))
  )


(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
