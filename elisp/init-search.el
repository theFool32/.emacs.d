;;; init-search.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-global-config)
  (require 'init-const))

(use-package youdao-dictionary
  :commands youdao-dictionary-play-voice-of-current-word
  :init
  (setq url-automatic-caching t
        youdao-dictionary-use-chinese-word-segmentation t) ; 中文分词

  (defun +my/youdao-search-at-point ()
    "Search word at point and display result with `posframe', `pos-tip', or buffer."
    (interactive)
    (if (display-graphic-p)
        (youdao-dictionary-search-at-point-posframe)
      (youdao-dictionary-search-at-point))))

;; ColorRGPac
(use-package color-rg
  :commands (color-rg-search-input color-rg-search-project color-rg-search-symbol-in-project)
  :straight (:host github :repo "manateelazycat/color-rg")
  :if *rg*
  :bind
  (:map color-rg-mode-map
        ("q" . +my/quit-color-rg))
  :init
  (setq color-rg-mac-load-path-from-shell nil)
  :config
  (fset 'color-rg-project-root-dir #'+my/project-root)
  (evil-make-overriding-map color-rg-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'color-rg-mode-hook #'evil-normalize-keymaps)
  (defun +my/quit-color-rg ()
    (interactive)
    (kill-current-buffer)
    (evil-quit))
  )
;; -ColorRGPac

(use-package pinyinlib
  :after orderless
  :config
  (defun completion--regex-pinyin (str)
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  (add-to-list 'orderless-matching-styles 'completion--regex-pinyin)
  )


(provide 'init-search)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-search.el ends here
