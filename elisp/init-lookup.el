;;; init-lookup.el ---
;;

;;  TODO: should this be combined with `init-search.el'?

(require 'lookup/+autoloads)

(defvar +lookup-definition-functions
  '(+lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-ffap-backend-fn
    +lookup-project-search-backend-fn))

(defvar +lookup-references-functions
  '(+lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn))

;;
;;; dumb-jump

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project "~/.emacs.d/"
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-quiet t
        dumb-jump-selector 'completing-read)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))

;;
;;; xref
(use-package xref
  :straight nil
  :init
  ;; On Emacs 28, `xref-search-program' can be set to `ripgrep'.
  ;; `project-find-regexp' benefits from that.
  (when (>= emacs-major-version 28)
    (setq xref-search-program 'ripgrep)
    (setq xref-show-xrefs-function #'xref-show-definitions-completing-read)
    (setq xref-show-definitions-function #'xref-show-definitions-completing-read))
  :hook ((xref-after-return xref-after-jump) . recenter))


;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(use-package better-jumper
  :hook (+my/first-input . better-jumper-mode)
  :commands doom-set-jump-a
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))

(with-eval-after-load 'xref
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; This integration is already built into evil
  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)
  )

(provide 'init-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lookup.el ends here
