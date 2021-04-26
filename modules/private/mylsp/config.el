;;; private/mylsp/config.el -*- lexical-binding: t; -*-

(use-package! lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024))

  (setq lsp-auto-guess-root nil        ; Detect project root
        lsp-keep-workspace-alive nil ; Auto-kill LSP server
        lsp-enable-indentation nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-semantic-tokens-enable nil
        lsp-keep-workspace-alive nil
        lsp-diagnostics-provider :none
        lsp-idle-delay 0.5
        lsp-enable-on-type-formatting nil
        lsp-enable-snippet nil
        lsp-enable-text-document-color nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-log-io nil
        lsp-enable-folding nil
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers nil
        lsp-keymap-prefix "C-c l"
        lsp-eldoc-render-all nil
        )
  (setq gc-cons-threshold 100000000)
  )

(use-package! lsp-ui
  :bind (("C-c u" . lsp-ui-imenu)
         :map lsp-ui-mode-map
         ("C-M-l" . lsp-ui-hydra/body)
         ("M-RET" . lsp-ui-sideline-apply-code-actions))
  :hook (lsp-mode . lsp-ui-mode)
  :init (setq lsp-ui-sideline-show-diagnostics nil
              lsp-ui-sideline-ignore-duplicate t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-border (face-foreground 'font-lock-comment-face)
              lsp-ui-imenu-colors `(,(face-foreground 'font-lock-keyword-face)
                                    ,(face-foreground 'font-lock-string-face)
                                    ,(face-foreground 'font-lock-constant-face)
                                    ,(face-foreground 'font-lock-variable-name-face)))
  :config
  (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)
  (add-hook 'after-load-theme-hook (lambda ()
                                     (setq lsp-ui-doc-border (face-foreground 'font-lock-comment-face))
                                     (set-face-background 'lsp-ui-doc-background (face-background 'tooltip))))
  )
