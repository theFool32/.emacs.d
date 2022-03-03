;;; init-dired.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-dired.el
;; Description: Initialize Dired and Related Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 11:37:00 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Mar  3 13:14:16 2022 (+0800)
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

  (with-eval-after-load 'general
    (general-define-key :states '(normal)
                        :keymaps 'dired-mode-map
                        "l" 'dired-find-alternate-file
                        "h"  'dired-up-directory)
    )

  ;; Colourful dired
  (use-package diredfl
    :init (diredfl-global-mode 1))


  (use-package all-the-icons-dired
    :disabled
    :defer t
    :after dired
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package dired-git-info
    :after dired
    :config
    (evil-define-key 'normal dired-mode-map ")" 'dired-git-info-mode))

  ;; Extra Dired functionality
  (use-package dired-x
    :straight nil
    :demand
    :config
    (add-hook 'dired-mode-hook 'dired-omit-mode)
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
                            "?" 'dirvish-menu-all-cmds)))
    )
  )



;; SaveAllBuffers
(defun save-all-buffers ()
  "Instead of `save-buffer', save all opened buffers by calling `save-some-buffers' with ARG t."
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
