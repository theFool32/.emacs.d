;;; init-dashboard.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dashboard.el
;; Description: Initialize Dashboard
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 17:21:46 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Jul 29 11:19:49 2021 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d dashboard
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dashboard
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

;; DashboardPac
(use-package dashboard
  :demand
  :diminish (dashboard-mode)
  :functions (all-the-icons-faicon all-the-icons-material winner-undo widget-forward)
  ;; :bind
  ;; (("C-z d" . open-dashboard)
  ;;  :map dashboard-mode-map
  ;;  (("n" . dashboard-next-line)
  ;;   ("p" . dashboard-previous-line)
  ;;   ("N" . dashboard-next-section)
  ;;   ("F" . dashboard-previous-section)))
  :hook (dashboard-mode . (lambda () (setq-local frame-title-format "")))
  :custom
  (dashboard-banner-logo-title "EMACS - Enjoy Programming & Writing")
  (dashboard-startup-banner (expand-file-name "images/banner.txt" user-emacs-directory))
  (dashboard-center-content t)
  (dashboard-items '((recents  . 7)
                     (agenda . 5)))

  (initial-buffer-choice (lambda () (get-buffer dashboard-buffer-name)))
  (dashboard-set-heading-icons t)
  (dashboard-set-navigator t)
  (dashboard-set-init-info t)
  (dashboard-set-heading-icons t)
  (dashboard-heading-icons '((recents   . "file-text")
                             (bookmarks . "bookmark")
                             (agenda    . "calendar")
                             (projects  . "briefcase")
                             (registers . "database")))
  (dashboard-set-footer t)
  (dashboard-footer-icon (all-the-icons-faicon "heart"
                                               :height 1.1
                                               :v-adjust -0.05
                                               :face 'error))
  ;; (dashboard-page-separator "\n\f\n")
  :custom-face
  (dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
  (dashboard-banner-logo-title ((t (:family "CaskaydiaCove Nerd Font" :height 200))))
  :config
  (setq dashboard-set-file-icons t)
  (set-face-attribute 'dashboard-text-banner-face nil :foreground "#E5C07B")
  (dashboard-setup-startup-hook)
  ;; WORKAROUND: fix differnct background color of the banner image.
  ;; @see https://github.com/emacs-dashboard/emacs-dashboard/issues/203
  (defun my-dashboard-insert-image-banner (banner)
    "Display an image BANNER."
    (when (file-exists-p banner)
      (let* ((title dashboard-banner-logo-title)
             (spec (create-image banner))
             (size (image-size spec))
             (width (car size))
             (left-margin (max 0 (floor (- dashboard-banner-length width) 2))))
        (goto-char (point-min))
        (insert "\n")
        (insert (make-string left-margin ?\ ))
        (insert-image spec)
        (insert "\n\n")
        (when title
          (dashboard-center-line title)
          (insert (format "%s\n\n" (propertize title 'face 'dashboard-banner-logo-title)))))))
  (advice-add #'dashboard-insert-image-banner :override #'my-dashboard-insert-image-banner)
  ;; Open Dashboard function
  (defun open-dashboard ()
    "Open the *dashboard* buffer and jump to the first widget."
    (interactive)
    (if (get-buffer dashboard-buffer-name)
        (kill-buffer dashboard-buffer-name))
    (dashboard-insert-startupify-lists)
    (switch-to-buffer dashboard-buffer-name)
    (goto-char (point-min))
    (delete-other-windows))
  (defun restore-previous-session ()
    "Restore the previous session."
    (interactive)
    (when (bound-and-true-p persp-mode)
      (restore-session persp-auto-save-fname)))
  (defun restore-session (fname)
    "Restore the specified session."
    (interactive (list (read-file-name "Load perspectives from a file: "
                                       persp-save-dir)))
    (when (bound-and-true-p persp-mode)
      (message "Restoring session...")
      (quit-window t)
      (condition-case-unless-debug err
          (persp-load-state-from-file fname)
        (error "Error: Unable to restore session -- %s" err))
      (message "Restoring session...done")))
  (defun dashboard-goto-recent-files ()
    "Go to recent files."
    (interactive)
    (let ((func (local-key-binding "r")))
      (and func (funcall func))))
  )
;; -DashboardPac

(provide 'init-dashboard)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dashboard.el ends here
