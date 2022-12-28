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
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((+my/first-input-hook . persp-mode))
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
      (error)))
  :config
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

  (advice-add #'persp-save-state-to-file :before
              (lambda (&optional _)
                (set-persp-parameter
                 'tab-bar-tabs
                 (frameset-filter-tabs (tab-bar-tabs) nil nil t))))

  (advice-add #'persp-load-state-from-file :after
              (lambda (&optional _)
                (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
                (tab-bar--update-tab-bar-lines t)))
  )


(provide 'init-persp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-persp.el ends here
