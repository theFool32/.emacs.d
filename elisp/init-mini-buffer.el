;;; init-selectrum.el ---
;;
;; Filename: init-selectrum.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Sun May  2 14:40:03 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 59
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


(eval-when-compile
  (require 'init-custom))

(cond
 ((string-equal my-mini-buffer-completion "vertico")
  (use-package vertico
    :straight (:type git :host github :repo "minad/vertico" :branch "main")
    :init
    (vertico-mode)
    :config
    ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
    (setq vertico-cycle t))
  )
 ((string-equal my-mini-buffer-completion "selectrum")
  (use-package selectrum
    :config
    (selectrum-mode +1)
    )
  ))

(use-package orderless)
(use-package embark)
(use-package marginalia
  :hook (after-init-hook . marginalia-mode))

(use-package mini-frame
  :straight (:type git :host github :repo "muffinmad/emacs-mini-frame")
  :hook (after-init . mini-frame-mode)
  :commands (mini-frame-mode)
  :custom
  (mini-frame-show-parameters `((left . 0.5)
                                (top . ,(/ (frame-pixel-height) 2))
                                (background-mode 'dark)
                                (foreground-color . "#bbc2cf")
                                (background-color . "#242730")
                                (min-width . 80)
                                (min-height . ,(if (member this-command
                                                           '(swiper
                                                             swiper-backward swiper-all
                                                             swiper-isearch swiper-isearch-backward
                                                             counsel-grep-or-swiper counsel-grep-or-swiper-backward))
                                                   16
                                                 0))
                                (width . 0.8)
                                ))


  (mini-frame-advice-functions '(read-from-minibuffer
                                 read-string
                                 completing-read))
  )

(provide 'init-mini-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-selectrum.el ends here
