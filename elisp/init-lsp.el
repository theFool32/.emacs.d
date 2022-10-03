;;; init-lsp.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

(pcase my-lsp
  ('lsp-bridge
   (use-package lsp-bridge
     :commands (lsp-bridge-mode)
     :straight (:host github :repo "theFool32/lsp-bridge" :branch "develop" :files ("*.el" "*.py" "core/*" "langserver/*"))
     ;; :straight nil
     ;; :load-path "~/dev/lsp-bridge/"
     :hook (((python-mode c-mode c++-mode LaTeX-mode) . lsp-bridge-mode)
            (lsp-bridge-mode . (lambda ()
                                 (leader-def :keymaps 'override
                                   "cr" '(lsp-bridge-rename :wk "Rename symbol")
                                   "cF" '(lsp-bridge-find-impl :wk "Find implementation")
                                   "cD" '(lsp-bridge-find-references :wk "Find type definition"))

                                 (evil-define-key 'normal 'global
                                   "K" 'lsp-bridge-lookup-documentation)
                                 )))
     :custom
     (lsp-bridge-enable-diagnostics nil)
     (lsp-bridge-lookup-doc-tooltip-border-width 2)
     (lsp-bridge-enable-signature-help nil)
     (lsp-bridge-enable-show-in-mode-line nil)
     :config
     (fset 'lsp-capf 'lsp-bridge-capf)
     (add-to-list 'lsp-bridge-completion-popup-predicates
                  (lambda ()
                    (and
                     (< corfu--index 0)
                     )))

     (add-hook 'lsp-bridge-mode-hook
               (lambda () (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil t)))))
  ('eglot
   (use-package eglot
     :commands (+eglot-organize-imports +eglot-help-at-point)
     :hook (
            (eglot-managed-mode . (lambda ()
                                    (+lsp-optimization-mode)
                                    (leader-def :keymaps 'override
                                      "ca" '(eglot-code-actions :wk "Code Actions")
                                      "cr" '(eglot-rename :wk "Rename symbol")
                                      "cI" '(eglot-code-action-organize-imports :wk "Organize import")
                                      "ci" '(consult-imenu :wk "imenu")
                                      "cJ" '(consult-eglot-symbols :wk "Symbols in project")
                                      "cd" '(eglot-find-declaration :wk "Jump to definition")
                                      "cF" '(eglot-find-implementation :wk "Find implementation")
                                      "cD" '(eglot-find-typeDefinition :wk "Find type definition"))

                                    (evil-define-key 'normal 'global
                                      "K" '+eglot-help-at-point)
                                    ))
            ((python-mode c-mode c++-mode LaTeX-mode) . eglot-ensure)
            )
     :init
     (defvar +lsp--default-read-process-output-max nil)
     (defvar +lsp--default-gcmh-high-cons-threshold nil)
     (defvar +lsp--optimization-init-p nil)

     (define-minor-mode +lsp-optimization-mode
       "Deploys universal GC and IPC optimizations for `lsp-mode' and `eglot'."
       :global t
       :init-value nil
       (if (not +lsp-optimization-mode)
           (setq-default read-process-output-max +lsp--default-read-process-output-max
                         gcmh-high-cons-threshold +lsp--default-gcmh-high-cons-threshold
                         +lsp--optimization-init-p nil)
         ;; Only apply these settings once!
         (unless +lsp--optimization-init-p
           (setq +lsp--default-read-process-output-max
                 ;; DEPRECATED Remove check when 26 support is dropped
                 (if (boundp 'read-process-output-max)
                     (default-value 'read-process-output-max))
                 +lsp--default-gcmh-high-cons-threshold
                 (default-value 'gcmh-high-cons-threshold))
           ;; `read-process-output-max' is only available on recent development
           ;; builds of Emacs 27 and above.
           (setq-default read-process-output-max (* 1024 1024))
           ;; REVIEW LSP causes a lot of allocations, with or without Emacs 27+'s
           ;;        native JSON library, so we up the GC threshold to stave off
           ;;        GC-induced slowdowns/freezes. Doom uses `gcmh' to enforce its
           ;;        GC strategy, so we modify its variables rather than
           ;;        `gc-cons-threshold' directly.
           (setq-default gcmh-high-cons-threshold (* 2 +lsp--default-gcmh-high-cons-threshold))
           (gcmh-set-high-threshold)
           (setq +lsp--optimization-init-p t))))

     :config
     (use-package consult-eglot)
     (fset 'lsp-capf 'eglot-completion-at-point)
     (setq eglot-sync-connect 1
           eglot-connect-timeout 10
           eglot-autoshutdown t
           eglot-send-changes-idle-time 0.5
           eglot-events-buffer-size 0
           ;; NOTE We disable eglot-auto-display-help-buffer because :select t in
           ;;      its popup rule causes eglot to steal focus too often.
           eglot-auto-display-help-buffer nil)
     (setq eldoc-echo-area-use-multiline-p nil)
     (setq eglot-stay-out-of '(flymake))
     (setq eglot-ignored-server-capabilities '(:documentHighlightProvider :foldingRangeProvider :colorProvider :codeLensProvider :documentOnTypeFormattingProvider :executeCommandProvider))
     (defun +eglot-organize-imports() (call-interactively 'eglot-code-action-organize-imports))
     (add-to-list 'eglot-server-programs '((latex-mode Tex-latex-mode texmode context-mode texinfo-mode bibtex-mode) "texlab"))

     ;; HACK Eglot removed `eglot-help-at-point' in joaotavora/eglot@a044dec for a
     ;;      more problematic approach of deferred to eldoc. Here, I've restored it.
     ;;      Doom's lookup handlers try to open documentation in a separate window
     ;;      (so they can be copied or kept open), but doing so with an eldoc buffer
     ;;      is difficult because a) its contents are generated asynchronously,
     ;;      making them tough to scrape, and b) their contents change frequently
     ;;      (every time you move your cursor).
     (defvar +eglot--help-buffer nil)
     (defun +eglot-lookup-documentation (_identifier)
       "Request documentation for the thing at point."
       (eglot--dbind ((Hover) contents range)
                     (jsonrpc-request (eglot--current-server-or-lose) :textDocument/hover
                                      (eglot--TextDocumentPositionParams))
                     (let ((blurb (and (not (seq-empty-p contents))
                                       (eglot--hover-info contents range)))
                           (hint (thing-at-point 'symbol)))
                       (if blurb
                           (with-current-buffer
                               (or (and (buffer-live-p +eglot--help-buffer)
                                        +eglot--help-buffer)
                                   (setq +eglot--help-buffer (generate-new-buffer "*eglot-help*")))
                             (with-help-window (current-buffer)
                               (rename-buffer (format "*eglot-help for %s*" hint))
                               (with-current-buffer standard-output (insert blurb))
                               (setq-local nobreak-char-display nil)))
                         (display-local-help))))
       'deferred)

     (defun +eglot-help-at-point()
       (interactive)
       (+eglot-lookup-documentation nil)))
   )
  )

(provide 'init-lsp)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lsp.el ends here
