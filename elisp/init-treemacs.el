;;; init-treemacs.el ---
;;
;; Filename: init-treemacs.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Thu May 27 22:10:12 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 15
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

(use-package treemacs
  :commands (treemacs-follow-mode
             treemacs-filewatch-mode
             treemacs-fringe-indicator-mode
             treemacs-git-mode)
  :bind (:map treemacs-mode-map
              ("j" . treemacs-next-line)
              ("k" . treemacs-previous-line)
              :map treemacs-mode-map
              ([mouse-1]   . treemacs-single-click-expand-action))
  :config
  (setq treemacs-collapse-dirs           (if treemacs-python-executable 3 0)
        treemacs-sorting                 'alphabetic-case-insensitive-desc
        treemacs-follow-after-init       t
        treemacs-is-never-other-window   t
        treemacs-silent-filewatch        t
        treemacs-silent-refresh          t
        treemacs-width                   30)
  :config
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode t)
  (pcase (cons (not (null (executable-find "git")))
               (not (null (executable-find "python3"))))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  ;; Projectile integration
  (use-package treemacs-projectile
    :after projectile
    :bind (:map projectile-command-map
                ("h" . treemacs-projectile)))

  (use-package treemacs-evil
    :after evil
    )

  (use-package treemacs-magit
    :after magit
    :commands treemacs-magit--schedule-update
    :hook ((magit-post-commit
            git-commit-post-finish
            magit-post-stage
            magit-post-unstage)
           . treemacs-magit--schedule-update))

  (use-package treemacs-persp
    :after persp-mode
    :demand t
    :functions treemacs-set-scope-type
    :config (treemacs-set-scope-type 'Perspectives))

  (use-package lsp-treemacs
    :after lsp-mode
    :init (lsp-treemacs-sync-mode 1)
    :config
    (defun lsp-treemacs-symbols-toggle ()
      "Toggle the lsp-treemacs-symbols buffer."
      (interactive)
      (if (get-buffer "*LSP Symbols List*")
          (kill-buffer "*LSP Symbols List*")
        (progn (lsp-treemacs-symbols)
               (other-window -1))))

    ))

(provide 'init-treemacs)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-treemacs.el ends here
