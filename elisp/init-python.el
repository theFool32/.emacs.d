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
  :config

  (add-hook 'python-mode-hook (lambda ()
                                (setq-local tab-width 4)))

  (setq python-indent-offset 4
        python-shell-interpreter "python3"
        importmagic-python-interpreter "python"
        flycheck-python-flake8-executable "flake8")

  (when (eq my-lsp 'lsp-mode)
    (use-package lsp-pyright
      :after lsp-mode
      :init
      (setq lsp-pyright-multi-root nil)
      (when (executable-find "python3") (setq lsp-pyright-python-executable-cmd "python3"))
      :config
      (setq lsp-pyright-use-library-code-for-types t)
      (setq lsp-pyright-auto-search-paths nil)
      (setq lsp-pyright-auto-import-completions nil)
      (setq lsp-pyright-venv-path ".venv")

      (lsp-register-client
       (make-lsp-client
        :new-connection (lsp-tramp-connection (cons "pyright-langserver" lsp-pyright-langserver-command-args))
        :major-modes '(python-mode)
        :remote? t
        :server-id 'pyright-remote
        :multi-root t
        :priority 3
        :initialized-fn (lambda (workspace)
                          (with-lsp-workspace workspace
    		                                  ;; we send empty settings initially, LSP server will ask for the
    		                                  ;; configuration of each workspace folder later separately
    		                                  (lsp--set-configuration
    		                                   (make-hash-table :test 'equal))))
        :download-server-fn (lambda (_client callback error-callback _update?)
                              (lsp-package-ensure 'pyright callback error-callback))
        :notification-handlers (lsp-ht ("pyright/beginProgress" 'lsp-pyright--begin-progress-callback)
                                       ("pyright/reportProgress" 'lsp-pyright--report-progress-callback)
                                       ("pyright/endProgress" 'lsp-pyright--end-progress-callback))))
      ))

  (use-package py-isort
    :init
    (defun +python/python-sort-imports ()
      (when (derived-mode-p 'python-mode)
        (py-isort-before-save)))
    :config
    (add-hook 'before-save-hook #'+python/python-sort-imports)
    )
  )

(provide 'init-python)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-python.el ends here
