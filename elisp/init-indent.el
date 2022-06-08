;;; init-indent.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

;; IndentConfig
(setq-default indent-tabs-mode nil)
(setq-default indent-line-function 'insert-tab)
(setq-default tab-width 4)
(add-hook 'after-change-major-mode-hook
          (lambda () (and electric-indent-mode
			         (derived-mode-p 'text-mode 'fundamental-mode) ;;  FIXME: still not work for new buffer (e.g., leetcode)
			         (setq-local electric-indent-mode nil
                                 evil-auto-indent nil
                                 electric-indent-local-mode nil))))
;; -IndentConfig

(provide 'init-indent)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-indent.el ends here
