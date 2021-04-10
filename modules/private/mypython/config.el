;;; private/mypython/config.el -*- lexical-binding: t; -*-


(after! python
  (use-package! lsp-pyright
    :init (when (executable-find "python3") (setq lsp-pyright-python-executable-cmd "python3"))
    :config
    (setq lsp-pyright-multi-root nil)
    (setq lsp-pyright-use-library-code-for-types t)
    (setq lsp-pyright-auto-search-paths nil)
    )

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
