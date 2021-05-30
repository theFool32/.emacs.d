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
;;     Update #: 98
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

(use-package consult
  ;; Enable automatic preview at point in the *Completions* buffer.
  ;; This is relevant when you use the default completion UI,
  ;; and not necessary for Selectrum, Vertico etc.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-recent-file
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-root-function #'projectile-project-root)
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-root-function #'vc-root-dir)
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-root-function (lambda () (locate-dominating-file "." ".git")))
  )

(use-package counsel
  :disabled
  :hook (after-init . counsel-mode)
  :bind (:map counsel-mode-map
              ([remap swiper] . counsel-grep-or-swiper)
              ([remap swiper-backward] . counsel-grep-or-swiper-backward)
              ([remap dired] . counsel-dired)
              ([remap set-variable] . counsel-set-variable)
              ([remap insert-char] . counsel-unicode-char)
              ([remap recentf-open-files] . counsel-recentf)

              ("C-x j"   . counsel-mark-ring)
              ("C-h F"   . counsel-faces)

              ("C-c B" . counsel-bookmarked-directory)
              ("C-c L" . counsel-load-library)
              ("C-c O" . counsel-find-file-extern)
              ("C-c P" . counsel-package)
              ("C-c R" . counsel-list-processes)
              ("C-c f" . counsel-find-library)
              ("C-c g" . counsel-grep)
              ("C-c h" . counsel-command-history)
              ("C-c i" . counsel-git)
              ("C-c j" . counsel-git-grep)
              ("C-c o" . counsel-outline)
              ("C-c r" . counsel-rg)
              ("C-c z" . counsel-fzf)

              ("C-c c B" . counsel-bookmarked-directory)
              ("C-c c F" . counsel-faces)
              ("C-c c L" . counsel-load-library)
              ("C-c c O" . counsel-find-file-extern)
              ("C-c c P" . counsel-package)
              ("C-c c R" . counsel-list-processes)
              ("C-c c a" . counsel-apropos)
              ("C-c c e" . counsel-colors-emacs)
              ("C-c c f" . counsel-find-library)
              ("C-c c g" . counsel-grep)
              ("C-c c h" . counsel-command-history)
              ("C-c c i" . counsel-git)
              ("C-c c j" . counsel-git-grep)
              ("C-c c l" . counsel-locate)
              ("C-c c m" . counsel-minibuffer-history)
              ("C-c c o" . counsel-outline)
              ("C-c c p" . counsel-pt)
              ("C-c c r" . counsel-rg)
              ("C-c c s" . counsel-ag)
              ("C-c c t" . counsel-load-theme)
              ("C-c c u" . counsel-unicode-char)
              ("C-c c w" . counsel-colors-web)
              ("C-c c v" . counsel-set-variable)
              ("C-c c z" . counsel-fzf)

              :map counsel-find-file-map
              ("C-w" . counsel-up-directory)
              )
  :init
  (setq enable-recursive-minibuffers t)
  (setq swiper-action-recenter t)

  (setq counsel-find-file-at-point t
        counsel-yank-pop-separator "\n────────\n")

  ;; Use the faster search tool: ripgrep (`rg')
  (when (executable-find "rg")
    (setq counsel-grep-base-command "rg -S --no-heading --line-number --color never %s %s")
    (when (and *sys/mac* (executable-find "gls"))
      (setq counsel-find-file-occur-use-find nil
            counsel-find-file-occur-cmd
            "gls -a | grep -i -E '%s' | tr '\\n' '\\0' | xargs -0 gls -d --group-directories-first")))

  )

(use-package orderless
  :custom (completion-styles '(orderless))
  :config
  (savehist-mode)
  (setq orderless-skip-highlighting (lambda () selectrum-is-active))
  (setq selectrum-highlight-candidates-function #'orderless-highlight-matches)
  )
;; (use-package embark)
(use-package marginalia
  :hook (after-init . marginalia-mode))


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
