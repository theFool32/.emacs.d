;;; init-dired.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dired.el
;; Description: Initialize Dired and Related Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:37:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri Mar  4 20:18:50 2022 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d dired auto-save
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes dired, disk-usage
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

;; DiredPackage
(use-package dired
  :defer t
  :straight nil
  :custom
  ;; Always delete and copy recursively
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  ;; Auto refresh Dired, but be quiet about it
  (global-auto-revert-non-file-buffers t)
  (auto-revert-verbose nil)
  ;; Quickly copy/move file in Dired
  (dired-dwim-target t)
  ;; Move files to trash when deleting
  (delete-by-moving-to-trash t)
  ;; Load the newest version of a file
  (load-prefer-newer t)
  ;; Detect external file changes and auto refresh file
  (auto-revert-use-notify nil)
  (auto-revert-interval 3) ; Auto revert every 3 sec
  :config
  ;; Enable global auto-revert
  (global-auto-revert-mode t)
  ;; Reuse same dired buffer, to prevent numerous buffers while navigating in dired
  (put 'dired-find-alternate-file 'disabled nil)

  (defun xah-open-in-external-app (&optional @fname)
    "Open the current file or dired marked files in external app.
The app is chosen from your OS's preference.
When called in emacs lisp, if @fname is given, open that.
URL `http://ergoemacs.org/emacs/emacs_dired_open_file_in_ext_apps.html'
Version 2019-11-04"
    (interactive)
    (let* (($file-list
            (if @fname
                (progn (list @fname))
              (if (derived-mode-p major-mode 'dired-mode)
                  (dired-get-marked-files)
                (list (buffer-file-name)))))
           ($do-it-p (if (<= (length $file-list) 5)
                         t
                       (y-or-n-p "Open more than 5 files? "))))
      (when $do-it-p
        (cond
         ((string-equal system-type "darwin")
          (mapc
           (lambda ($fpath)
             (message "%s" $fpath)
             (shell-command
              (concat "open " (shell-quote-argument $fpath))))
           $file-list))
         ((string-equal system-type "gnu/linux")
          (mapc
           (lambda ($fpath) (let ((process-connection-type nil))
                         (start-process "" nil "xdg-open" $fpath)))
           $file-list))))))

  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "l" 'dired-find-alternate-file
                        "h"  'dired-up-directory
                        "C-<return>" 'xah-open-in-external-app)
    )

  ;; Colourful dired
  (use-package diredfl
    :hook (dired-mode . diredfl-mode))

  (use-package dired-git-info
    :after dired
    :config
    (evil-define-key 'normal dired-mode-map ")" 'dired-git-info-mode))

  ;; Extra Dired functionality
  (use-package dired-x
    :straight nil
    :demand
    :hook (dired-mode . dired-omit-mode)
    :config
    (setq dired-omit-files
          (concat dired-omit-files
                  "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))

  ;; `find-dired' alternative using `fd'
  (when (executable-find "fd")
    (use-package fd-dired))

  (use-package dired-narrow) ;; use `s' for fliter
  (use-package dired-open
    :config
    (setq dired-open-extensions
          (mapcar (lambda (ext)
                    (cons ext "open")) '("pdf" "doc" "docx" "ppt" "pptx"))))

  (use-package dirvish  ;; `(' for details.
    :straight (dirvish :type git :host github :repo "alexluigit/dirvish")
    :after dired
    :custom
    (dirvish-attributes '(all-the-icons file-size))
    :init
    (dirvish-override-dired-mode)
    :config
    (set-face-attribute 'ansi-color-blue nil :foreground "#FFFFFF")

    (use-package dirvish-menu
      :straight nil
      :config
      (with-eval-after-load 'general
        (general-define-key :states '(normal)
                            :keymaps 'dirvish-mode-map
                            "?" 'dirvish-menu-all-cmds))))
  (use-package dirvish-extras
    :straight nil)
  )



;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-bffers' with ARG t."
  (interactive)
  (save-some-buffers t))
(with-eval-after-load 'general
  (general-def "C-x C-s" nil)
  (general-def "C-x C-s" 'save-all-buffers)
  )
;; -SaveAllBuffers

(provide 'init-dired)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-dired.el ends here
