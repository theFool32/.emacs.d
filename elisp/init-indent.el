;;; init-indent.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;; IndentConfig
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)

(defun toggle-indent (&optional indent)
  "Toggle indent based on `evil-auto-indent'"
  (interactive (list evil-auto-indent))
  (if indent
      (progn
        (electric-indent-local-mode -1)
        (setq-local evil-auto-indent nil))
    (progn
      (electric-indent-local-mode 1)
      (setq-local evil-auto-indent t))))

(dolist (hook '(text-mode-hook conf-mode-hook conf-space-mode-hook))
  (add-hook hook (lambda ()
                   (toggle-indent t))))
;; -IndentConfig

(provide 'init-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-indent.el ends here
