;;; init-check.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

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
