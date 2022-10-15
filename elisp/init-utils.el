;;; init-utils.el ---
;;
;; Filename: init-utils.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Thu May 27 22:07:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 39
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

(use-package restart-emacs
  :commands restart-emacs)

(use-package atomic-chrome
  :defer
  :commands (atomic-chrome-start-server)
  :config
  (setq atomic-chrome-url-major-mode-alist
	    '(("overleaf\\.com" . LaTeX-mode))))

(use-package tramp
  :defer 1
  :straight nil
  :config
  (setq tramp-completion-use-auth-sources nil
        tramp-verbose 0
        tramp-chunksize 2000)
  (setq remote-file-name-inhibit-cache nil)
  (setq vc-ignore-dir-regexp
        (format "%s\\|%s"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))
  )

(use-package vundo
  :straight (:host github :repo "casouri/vundo")
  :commands vundo
  :defer t
  :config
  (setf (alist-get 'selected-node vundo-glyph-alist) ?X
        (alist-get 'node vundo-glyph-alist) ?O))

(use-package super-save
  :diminish
  :defer 0.5
  :init
  (setq auto-save-default nil)
  :config
  (add-to-list 'super-save-triggers 'switch-window)
  (add-to-list 'super-save-triggers 'switch-to-buffer)
  (setq super-save-exclude '(".gpg"))
  (setq super-save-idle-duration 5)
  (setq save-silently t)
  (super-save-mode 1)
  (advice-add 'super-save-command :override 'save-all-buffers))

(use-package ztree
  :commands ztree-diff
  :defer t)

(provide 'init-utils)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-utils.el ends here
