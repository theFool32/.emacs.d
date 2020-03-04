;;; init-search.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-search.el
;; Description: Initialize Packages for Searching
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:01:43 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Feb 22 10:40:23 2020 (+0800)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d color-rg rg
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes ivy swiper counsel color-rg snails
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
  (require 'init-global-config)
  (require 'init-const))


;; ColorRGPac
;; (use-package color-rg
;;   :load-path (lambda () (expand-file-name "site-elisp/color-rg" user-emacs-directory))
;;   :if *rg*
;;   :bind ("C-M-s" . color-rg-search-input))
;; -ColorRGPac


(use-package exec-path-from-shell
  :defer t
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables '("PATH" "MANPATH" "https_proxy")
        exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun my-youdao-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (youdao-dictionary-search-at-point-posframe)
      (youdao-dictionary-search-at-point))))


;; SnailsPac
;; (use-package snails
;;   :load-path (lambda () (expand-file-name "site-elisp/snails/" user-emacs-directory))
;;   :if *sys/gui*
;;   :custom-face
;;   (snails-content-buffer-face ((t (:background "#111" :height 110))))
;;   (snails-input-buffer-face ((t (:background "#222" :foreground "gold" :height 110))))
;;   (snails-header-line-face ((t (:inherit font-lock-function-name-face :underline t :height 1.1))))
;;   :init
;;   (use-package exec-path-from-shell :if (featurep 'cocoa) :defer t)
;;   :config
;;   ;; Functions for specific backends
;;   (defun snails-current-project ()
;;     (interactive)
;;     (snails '(snails-backend-projectile snails-backend-rg snails-backend-fd)))
;;   (defun snails-active-recent-buffers ()
;;     (interactive)
;;     (snails '(snails-backend-buffer snails-backend-recentf)))
;;   (defun snails-everywhere ()
;;     (interactive)
;;     (snails '(snails-backend-everything snails-backend-mdfind)))
;;   )
;; -SnailsPac

(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
