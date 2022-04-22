;;; init-shell.el ---
;;
;; Filename: init-shell.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2021 theFool32
;; Created: Thu Sep  9 15:49:09 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 52
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

(when (and module-file-suffix
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands (vterm--internal vterm-posframe-toggle)
    :init
    (setq vterm-always-compile-module t)
    (setq vterm-shell "tmux")
    :config
    (evil-define-key 'insert vterm-mode-map (kbd "C-c") 'vterm-send-C-c)


    ;; (advice-add 'vterm-send-return :before (lambda ()
    ;;                                          (vterm-send-string " && printf '\\033[6 q'")))

    ;; https://github.com/akermu/emacs-libvterm#how-can-i-get-the-directory-tracking-in-a-more-understandable-way
    ;; (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

    (with-no-warnings
      (defvar vterm-posframe--frame nil)

      (defun vterm-posframe-hidehandler (_)
        "Hidehandler used by `vterm-posframe-toggle'."
        (not (eq (selected-frame) posframe--frame)))

      (defun get-vterm-buffer ()
        "Return vterm buffer."
        (let ((buffer (get-buffer "vterm")))
          (if buffer
              buffer
            (vterm--internal #'ignore))))

      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (get-vterm-buffer))
              (width  (max 80 (/ (frame-width) 2)))
              (height (/ (frame-height) 2)))
          (if (and vterm-posframe--frame
                   (frame-live-p vterm-posframe--frame)
                   (frame-visible-p vterm-posframe--frame))
              (progn
                (posframe-hide buffer)
                ;; Focus the parent frame
                (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'vterm-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . 't))
                   :accept-focus t))
            ;; Focus the child frame
            (select-frame-set-input-focus vterm-posframe--frame))))
      )))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
