;;; init-ui-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui-config.el
;; Description: Initialize UI Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 16:12:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Thu Apr 29 01:56:02 2021 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d ui
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes prettify-symbols-mode and other UI configurations
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(eval-when-compile
  (require 'init-const))

;; PreSym
(global-prettify-symbols-mode 1)
;; -PreSym

;; TitleBar
(setq-default frame-title-format '("EMACS" " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
;; (setq initial-major-mode 'text-mode)
;; -StartupScreen

;; DisLineNum
;; Hook line numbers to only when files are opened, also use linum-mode for emacs-version< 26
(if (version< emacs-version "26")
    (global-linum-mode)
  (add-hook 'text-mode-hook #'display-line-numbers-mode)
  (add-hook 'prog-mode-hook #'display-line-numbers-mode))
;; Display column numbers in modeline
(column-number-mode 1)
(setq display-line-numbers-type 'relative)
;; -DisLineNum

;; DisTimeBat
(display-time-mode 1)
;; (display-battery-mode 1)
;; -DisTimeBat

;;Font

(defun my-default-frame-face ()
  (when (display-graphic-p)
    ;; Set default font
    ;; (cl-loop for font in '("SauceCodePro Nerd Font")
    (cl-loop for font in '("CaskaydiaCove Nerd Font")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :font font
                                        :height (cond (*sys/mac* 150)
                                                      (*sys/win32* 140)
                                                      (*sys/linux* 200)
                                                      (t 140))))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Symbola" "Apple Symbols" "Symbol" "icons-in-terminal")
             when (font-installed-p font)
             return (set-fontset-font t 'unicode font nil 'prepend))

    ;; Specify font for Chinese characters
    ;; (cl-loop for font in '("Noto Sans CJK SC" "WenQuanYi Micro Hei" "Microsoft Yahei")
    (cl-loop for font in '("Sarasa Mono SC" "Noto Sans CJK SC" "WenQuanYi Micro Hei" "Microsoft Yahei")
             when (font-installed-p font)
             return (set-fontset-font t '(#x4e00 . #x9fff) font))))
(my-default-frame-face)
(add-hook 'after-make-frame-functions
	      (lambda (new-frame)
	        (select-frame new-frame)
	        (my-default-frame-face) ))


(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))

(setq split-width-threshold 0
      split-height-threshold nil)


(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
