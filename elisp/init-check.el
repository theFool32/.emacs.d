;;; init-check.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;; FlyCheckPac
(use-package flycheck
  :disabled
  :defer t
  :diminish
  :hook ((prog-mode markdown-mode LaTeX-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
         diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (if *sys/gui*
      (use-package flycheck-posframe
        :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-border-width 1)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (setq flycheck-check-syntax-automatically '(save))
  )
;; -FlyCheckPac

;; flymake
(use-package flymake
  :ensure nil
  :hook ((prog-mode markdown-mode LaTeX-mode) . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil)
  ;; (setq-local flymake-diagnostic-functions nil)
  (setq flymake-fringe-indicator-position 'right-fringe)
  (use-package flymake-posframe
    :straight (:host github :repo "articuluxe/flymake-posframe" :branch "feature/eglot")
    :hook (flymake-mode . flymake-posframe-mode))
  ;;  TODO: use `flymake-flycheck' or `flymake-collection' to enhance backends
  )

(use-package wucuo
  ;;  FIXME: flyspell mode will override C-; for embrace-commander
  :hook (LaTeX-mode . wucuo-mode)
  :config
  (setq ispell-program-name "aspell")
  ;; You could add extra option "--camel-case" for since Aspell 0.60.8
  ;; @see https://github.com/redguardtoo/emacs.d/issues/796
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  (add-hook 'text-mode-hook #'wucuo-start)
  (general-define-key :states 'normal
                      :keymaps 'LaTeX-mode-map
                      :prefix ","
                      "g" '(flyspell-auto-correct-word :wk "Auto correct")
                      "d" '(flyspell-correct-word-before-point :wk "Correct word")))

(provide 'init-check)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-check.el ends here
