;;; init-rime.el ---
;;
;; Filename: init-rime.el
;; Description: For rime
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Wed Apr 15 17:32:45 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 40
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

(use-package rime
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist/"
        rime-user-data-dir "~/Library/Rime"  ;; FIXME: 无法读到自定义词库
        default-input-method "rime")

  :custom
  (rime-show-candidate 'posframe)
  (rime-posframe-properties (list :font "sarasa ui sc"
                                  :internal-border-width 10))
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-after-ascii-char-p
     rime-predicate-space-after-cc-p))

  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@28/include"))

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-active-mode-map (kbd "M-j") 'rime-inline-ascii)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
