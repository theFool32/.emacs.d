;;; init-scroll.el --- -*- lexical-binding: t -*-

;; SmoothScroll
;; Vertical Scroll
(setq scroll-step 1)
(setq scroll-margin 1)
(setq scroll-conservatively 101)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq auto-window-vscroll nil)
(setq fast-but-imprecise-scrolling nil)
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1)))
(setq mouse-wheel-progressive-speed t)
;; Horizontal Scroll
(setq hscroll-step 1)
(setq hscroll-margin 1)
;; -SmoothScroll

;; (use-package smooth-scrolling
;;   :config
;;   (smooth-scrolling-mode 1)
;;   )

(provide 'init-scroll)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-scroll.el ends here
