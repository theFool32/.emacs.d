;;; init-indent.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;; IndentConfig
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)

(dolist (hook '(text-mode fundamental-mode conf-space-mode))
  (add-hook hook (lambda ()
                   (electric-indent-mode -1)
                   (electric-indent-local-mode -1)
                   (setq-local evil-auto-indent nil))))
;; -IndentConfig

(provide 'init-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-indent.el ends here
