;;; private/better-edit/config.el -*- lexical-binding: t; -*-

(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll

(use-package! color-rg)


(defun delete-trailing-whitespace-except-current-line ()
  "An alternative to `delete-trailing-whitespace'.

The original function deletes trailing whitespace of the current line."
  (interactive)
  (let ((begin (line-beginning-position))
        (end (line-end-position)))
    (save-excursion
      (when (< (point-min) (1- begin))
        (save-restriction
          (narrow-to-region (point-min) (1- begin))
          (delete-trailing-whitespace)
          (widen)))
      (when (> (point-max) (+ end 2))
        (save-restriction
          (narrow-to-region (+ end 2) (point-max))
          (delete-trailing-whitespace)
          (widen))))))

(add-hook 'before-save-hook #'delete-trailing-whitespace-except-current-line)

(use-package! smartparens
  :hook (prog-mode . smartparens-mode)
  :diminish smartparens-mode
  :custom
  (sp-escape-quotes-after-insert nil)
  )

;; Indent
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(add-hook 'after-change-major-mode-hook
          (lambda () (if (equal electric-indent-mode 't)
                         (when (derived-mode-p 'text-mode)
                           (electric-indent-mode -1))
                       (electric-indent-mode 1))))

(use-package! delete-block)

(use-package! awesome-pair
  :hook (prog-mode . awesome-pair-mode))

(after! evil
  (setq evil-split-window-below t
        evil-vsplit-window-right t)
  )

(remove-hook 'undo-fu-mode-hook #'global-undo-fu-session-mode)


;; use for sync my code
(add-hook 'after-save-hook (lambda () (call-process-shell-command "rc" nil 0)))


;; disable `s'
(after! evil-snipe (evil-snipe-mode -1))

(setq ispell-dictionary "english")
