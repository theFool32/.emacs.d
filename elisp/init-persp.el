;;; init-persp.el ---
;;
;; Filename: init-persp.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2021 theFool32
;; Created: Tue Sep 28 20:36:27 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 128
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

;; Windows/buffers sets shared among frames + save/load.


(use-package persp-mode
  :diminish
  :defines (recentf-exclude)
  :commands (get-current-persp persp-contain-buffer-p persp-save-frame)
  :hook ((+my/first-input-hook . persp-mode)
         (kill-emacs . persp-save-frame))
  :init
  (setq persp-keymap-prefix (kbd "C-x p")
        persp-nil-name "default"
        persp-set-last-persp-for-new-frames nil
        persp-kill-foreign-buffer-behaviour 'kill
        persp-auto-resume-time -0.1
        )
  (defun +my/persp-resume ()
    "Resume previous layout"
    (interactive)
    (persp-mode +1)
    (condition-case error
        (persp-load-state-from-file (expand-file-name "persp-auto-save" persp-save-dir))
      (error))
    (persp-load-frame))
  :config
  ;; Save and load frame parameters (size & position)
  (defvar persp-frame-file (expand-file-name "persp-frame" persp-save-dir)
    "File of saving frame parameters.")

  (defun persp-save-frame ()
    "Save the current frame parameters to file."
    (interactive)
    (when (and (display-graphic-p) persp-mode)
      (condition-case error
          (with-temp-buffer
            (erase-buffer)
            (insert
             ";;; -*- mode: emacs-lisp; coding: utf-8-unix -*-\n"
             ";;; This is the previous frame parameters.\n"
             ";;; Last generated " (current-time-string) ".\n"
             "(setq initial-frame-alist\n"
             (format "      '((top . %d)\n" (eval (frame-parameter nil 'top)))
             (format "        (left . %d)\n" (eval (frame-parameter nil 'left)))
             (format "        (width . %d)\n" (eval (frame-parameter nil 'width)))
             (format "        (height . %d)\n" (eval (frame-parameter nil 'height)))
             (format "        (fullscreen . %s))\n" (frame-parameter nil 'fullscreen))
             (format "      my-neo-global--window-exists-p %d \n" (if (and (fboundp 'neo-global--window-exists-p) (neo-global--window-exists-p)) 1 0))
             (format ")\n")
             )
            (write-file persp-frame-file))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  (defun persp-load-frame ()
    "Load frame with the previous frame's geometry."
    (interactive)
    (when (and (display-graphic-p)
               persp-mode)
      (condition-case error
          (progn
            (load persp-frame-file)

            ;; Handle multiple monitors gracefully
            (when (or (>= (eval (frame-parameter nil 'left)) (display-pixel-width))
                      (>= (eval (frame-parameter nil 'top)) (display-pixel-height)))
              (set-frame-parameter nil 'left 0)
              (set-frame-parameter nil 'top 0)))
        (error
         (warn "persp frame: %s" (error-message-string error))))))

  ;; Don't save dead or temporary buffers
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore dead and unneeded buffers."
              (or (not (buffer-live-p b))
                  (string-prefix-p " *" (buffer-name b)))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore temporary buffers."
              (let ((bname (file-name-nondirectory (buffer-name b))))
                (or (string-prefix-p "magit" bname)
                    (string-prefix-p "COMMIT_EDITMSG" bname)
                    (string-prefix-p "\*Minibuf-." bname)
                    (string-prefix-p "\*scratch\*" bname)
                    (string-match-p "\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                    (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude))

  ;;  FIXME: Not work for tab-bar
  ;; (advice-add #'persp-save-state-to-file :before
  ;; ;; (advice-add #'persp-asave-on-exit :before
  ;;             (defun +workspaces-save-tab-bar-data-h (&optional _)
  ;;               (set-persp-parameter
  ;;                'tab-bar-tabs (tab-bar-tabs))))

  ;; (advice-add #'persp-load-state-from-file :after
  ;;             (defun +workspaces-load-tab-bar-data-h (&optional _)
  ;;               (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
  ;;               (tab-bar--update-tab-bar-lines t)))
  )


(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
