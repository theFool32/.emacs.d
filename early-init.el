;;; early-init.el --- -*- lexical-binding: t -*-

;; DeferGC
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)
;; -DeferGC

;; UnsetPES
(setq package-enable-at-startup nil)
;; -UnsetPES

;; UnsetFNHA
(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)
;; -UnsetFNHA

;; UnsetSRF
(setq site-run-file nil)
;; -UnsetSRF

;; DisableUnnecessaryInterface
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; -DisableUnnecessaryInterface

(setq byte-compile-warnings nil)
(setq native-comp-async-report-warnings-errors nil)
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq display-time-default-load-average nil)

(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Automatically reread from disk if the underlying file changes
(setq auto-revert-interval 3)
(setq auto-revert-check-vc-info t)
(global-auto-revert-mode)

(savehist-mode)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)

(setq use-package-always-defer t
      use-package-enable-imenu-support t
      use-package-verbose (not (bound-and-true-p byte-compile-current-file))
      use-package-expand-minimally t
      use-package-compute-statistics nil)

(provide 'early-init)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; early-init.el ends here
