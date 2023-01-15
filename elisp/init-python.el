;;; init-python.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

(use-package python
  :defer t
  :mode ("\\.py\\'" . python-mode)
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  (setq python-indent-guess-indent-offset-verbose nil)
  :config

  (add-hook 'python-mode-hook (lambda ()
                                (setq-local tab-width 4)))

  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        importmagic-python-interpreter "python"
        flycheck-python-flake8-executable "flake8")

  (use-package py-isort
    :hook (python-mode . (lambda ()
                           (add-hook 'before-save-hook #'py-isort-before-save nil t))))
  )

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
