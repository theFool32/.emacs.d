;;; early-init.el --- -*- lexical-binding: t -*-

;; Skip handler lookups while loading, then restore TRAMP, compressed and
;; encrypted file support.  Keep handlers registered during startup.
(defvar +my/file-name-handler-alist file-name-handler-alist)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist
                  (delete-dups (append file-name-handler-alist
                                       +my/file-name-handler-alist)))))

(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      package-enable-at-startup nil
      file-name-handler-alist nil
      site-run-file nil
      default-frame-alist
      '((font . "CaskaydiaCove Nerd Font-14")
        (vertical-scroll-bars . nil)
        (menu-bar-lines . 0)
        (tool-bar-lines . 0)

        (undecorated . t);会导致所有边框全部消失无法拖动调整窗口大小 需要加上后面两句
        (drag-internal-border . 1)
        (internal-border-width . 5)
        ;; (ns-transparent-titlebar . t)
        )

      mode-line-format nil
      byte-compile-warnings nil
      native-comp-async-report-warnings-errors nil
      warning-suppress-log-types '((comp) (bytecomp)(emacs)) ;; disable `emacs` warnings for `youdao` autoloads
      display-time-default-load-average nil

      inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil
      inhibit-compacting-font-caches t

      frame-inhibit-implied-resize t
      frame-resize-pixelwise t

      ;; Elpaca builds link to editable sources; never load a stale .elc.
      load-prefer-newer t
      auto-mode-case-fold nil

      bidi-display-reordering 'left-to-right
      bidi-paragraph-direction 'left-to-right
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil

      fast-but-imprecise-scrolling t
      ffap-machine-p-known 'reject
      redisplay-skip-fontification-on-input t

      idle-update-delay 1.0
      )

(fset 'display-startup-echo-area-message 'ignore)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3
      auto-revert-check-vc-info t
      auto-revert-verbose nil
      auto-revert-use-notify nil
      global-auto-revert-non-file-buffers t)
(global-auto-revert-mode)

(savehist-mode)

(setq use-package-always-defer t
      use-package-enable-imenu-support t
      ;; use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-verbose nil
      use-package-expand-minimally t
      use-package-always-ensure t
      use-package-compute-statistics nil)

(setq warning-minimum-level :emergency)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
