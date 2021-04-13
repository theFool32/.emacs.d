;;; private/mypython/config.el -*- lexical-binding: t; -*-


(after! python
  (use-package! lsp-pyright
    :disabled
    :init (when (executable-find "python3") (setq lsp-pyright-python-executable-cmd "python3"))
    :config
    (setq lsp-pyright-multi-root nil)
    (setq lsp-pyright-use-library-code-for-types t)
    (setq lsp-pyright-auto-search-paths nil)
    (setq lsp-pyright-auto-import-completions nil)
    (setq lsp-pyright-venv-path ".venv")
    )

  ;; (use-package lsp-python-ms
  ;;   :ensure t
  ;;   :init (setq lsp-python-ms-auto-install-server t)
  ;;   :config
  ;;   (setq lsp-python-ms-completion-add-brackets nil)
  ;;   :hook (python-mode . (lambda ()
  ;;                          (require 'lsp-python-ms)
  ;;                          (lsp))))  ; or lsp-deferred


  (setq lsp-pylance-ms-executable "~/bin/pylance.sh")  ;; Use the pyright from vscode instead.
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection (lambda () lsp-pylance-ms-executable)
                                          (lambda () (f-exists? lsp-pylance-ms-executable)))
    :major-modes '(python-mode)
    :server-id 'mspylance
    :priority 3
    :initialized-fn (lambda (workspace)
                      (with-lsp-workspace workspace
                        (lsp--set-configuration (lsp-configuration-section "python"))))))

  (setq python-indent-offset 4
        importmagic-python-interpreter "python"
        flycheck-python-flake8-executable "flake8"))


(use-package! py-isort
  :defer t
  :init
  (setq python-sort-imports-on-save t)
  (defun +python/python-sort-imports ()
    (interactive)
    (when (and python-sort-imports-on-save
               (derived-mode-p 'python-mode))
      (py-isort-before-save)))
  (add-hook! 'python-mode-hook
    (add-hook 'before-save-hook #'+python/python-sort-imports nil t))
  )

(add-hook! 'pyvenv-post-activate-hooks #'lsp-restart-workspace)
(add-hook! 'pyvenv-post-deactivate-hooks #'lsp-restart-workspace)
