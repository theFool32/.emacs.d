;;; private/myivy/config.el -*- lexical-binding: t; -*-

(use-package! all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :config
  (setq all-the-icons-ivy-rich-icon-size 0.9)
  )

;; More friendly display transformer for Ivy
(use-package! ivy-rich
  :hook (;; Must load after `counsel-projectile'
         (counsel-projectile-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            "Use abbreviate in `ivy-rich-mode'."
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil))


(after! ivy
  (after! ivy-prescient
    (setq ivy-prescient-retain-classic-highlighting t)
    (setq ivy-prescient-enable-sorting t)))


(after! ivy-posframe
  ;; Lower internal-border-width on MacOS
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  )


(after! counsel
  (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
        counsel-describe-function-function 'helpful-callable
        counsel-describe-variable-function 'helpful-variable
        counsel-rg-base-command "rg -zS --no-heading --line-number --max-columns 1000 --color never %s ."
        counsel-grep-base-command counsel-rg-base-command))
