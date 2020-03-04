;;; init-ui-config.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-ui-config.el
;; Description: Initialize UI Configurations
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 14 16:12:56 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Feb 22 11:39:04 2020 (+0800)
;;           By: John
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
;; (defun add-pretty-lambda ()
;;   "Make some word or string show as pretty Unicode symbols.  See https://unicodelookup.com for more."
;;   (defvar +pretty-code-fira-code-font-ligatures
;;     '(("www"    . #Xe100)
;;       ("**"     . #Xe101)
;;       ("***"    . #Xe102)
;;       ("**/"    . #Xe103)
;;       ("*>"     . #Xe104)
;;       ("*/"     . #Xe105)
;;       ("\\\\"   . #Xe106)
;;       ("\\\\\\" . #Xe107)
;;       ("{-"     . #Xe108)
;;       ("[]"     . #Xe109)
;;       ("::"     . #Xe10a)
;;       (":::"    . #Xe10b)
;;       (":="     . #Xe10c)
;;       ("!!"     . #Xe10d)
;;       ("!="     . #Xe10e)
;;       ("!=="    . #Xe10f)
;;       ("-}"     . #Xe110)
;;       ("--"     . #Xe111)
;;       ("---"    . #Xe112)
;;       ("-->"    . #Xe113)
;;       ("->"     . #Xe114)
;;       ("->>"    . #Xe115)
;;       ("-<"     . #Xe116)
;;       ("-<<"    . #Xe117)
;;       ("-~"     . #Xe118)
;;       ("#{"     . #Xe119)
;;       ("#["     . #Xe11a)
;;       ("##"     . #Xe11b)
;;       ("###"    . #Xe11c)
;;       ("####"   . #Xe11d)
;;       ("#("     . #Xe11e)
;;       ("#?"     . #Xe11f)
;;       ("#_"     . #Xe120)
;;       ("#_("    . #Xe121)
;;       (".-"     . #Xe122)
;;       (".="     . #Xe123)
;;       (".."     . #Xe124)
;;       ("..<"    . #Xe125)
;;       ("..."    . #Xe126)
;;       ("?="     . #Xe127)
;;       ("??"     . #Xe128)
;;       (";;"     . #Xe129)
;;       ("/*"     . #Xe12a)
;;       ("/**"    . #Xe12b)
;;       ("/="     . #Xe12c)
;;       ("/=="    . #Xe12d)
;;       ("/>"     . #Xe12e)
;;       ("//"     . #Xe12f)
;;       ("///"    . #Xe130)
;;       ("&&"     . #Xe131)
;;       ("||"     . #Xe132)
;;       ("||="    . #Xe133)
;;       ("|="     . #Xe134)
;;       ("|>"     . #Xe135)
;;       ("^="     . #Xe136)
;;       ("$>"     . #Xe137)
;;       ("++"     . #Xe138)
;;       ("+++"    . #Xe139)
;;       ("+>"     . #Xe13a)
;;       ("=:="    . #Xe13b)
;;       ("=="     . #Xe13c)
;;       ("==="    . #Xe13d)
;;       ("==>"    . #Xe13e)
;;       ("=>"     . #Xe13f)
;;       ("=>>"    . #Xe140)
;;       ("=<"     . #Xe141)
;;       ("=<<"    . #Xe142)
;;       ("=/="    . #Xe143)
;;       (">-"     . #Xe144)
;;       (">="     . #Xe145)
;;       (">=>"    . #Xe146)
;;       (">>"     . #Xe147)
;;       (">>-"    . #Xe148)
;;       (">>="    . #Xe149)
;;       (">>>"    . #Xe14a)
;;       ("<*"     . #Xe14b)
;;       ("<*>"    . #Xe14c)
;;       ("<|"     . #Xe14d)
;;       ("<|>"    . #Xe14e)
;;       ("<$"     . #Xe14f)
;;       ("<$>"    . #Xe150)
;;       ("<!--"   . #Xe151)
;;       ("<-"     . #Xe152)
;;       ("<--"    . #Xe153)
;;       ("<->"    . #Xe154)
;;       ("<+"     . #Xe155)
;;       ("<+>"    . #Xe156)
;;       ("<="     . #Xe157)
;;       ("<=="    . #Xe158)
;;       ("<=>"    . #Xe159)
;;       ("<=<"    . #Xe15a)
;;       ("<>"     . #Xe15b)
;;       ("<<"     . #Xe15c)
;;       ("<<-"    . #Xe15d)
;;       ("<<="    . #Xe15e)
;;       ("<<<"    . #Xe15f)
;;       ("<~"     . #Xe160)
;;       ("<~~"    . #Xe161)
;;       ("</"     . #Xe162)
;;       ("</>"    . #Xe163)
;;       ("~@"     . #Xe164)
;;       ("~-"     . #Xe165)
;;       ("~="     . #Xe166)
;;       ("~>"     . #Xe167)
;;       ("~~"     . #Xe168)
;;       ("~~>"    . #Xe169)
;;       ("%%"     . #Xe16a)
;;       ("x"      . #Xe16b)
;;       (":"      . #Xe16c)
;;       ("+"      . #Xe16d)
;;       ("+"      . #Xe16e)
;;       ("*"      . #Xe16f)))
;;   (setq prettify-symbols-alist
;;         '(
;;           ("src_block"     . "»")
;;           ("src_block_end" . "«")
;;                                         ; Functional
;;           ("lambda"        . "λ")
;;           ("def"           . "ƒ")
;;                                         ; Types
;;           ("null"          . "∅")
;;           ("True"          . "𝕋")
;;           ("False"         . "𝔽")
;;           ("int"           . "ℤ")
;;           ("float"         . "ℝ")
;;           ("str"           . "𝕊")
;;           ("bool"          . "𝔹")
;;                                         ; Flow
;;           ("not"           . "￢")
;;           ("in"            . "∈")
;;           ("not-in"        . "∉")
;;           ("and"           . "∧")
;;           ("or"            . "∨")
;;           ("for"           . "∀")
;;           ("some"          . "∃")
;;           ("return"        . "⟼")
;;           ("yield"         . "⟻")
;;           ))

;;   (if nil
;;       (setq-default prettify-symbols-alist
;;                     (append prettify-symbols-alist
;;                             +pretty-code-fira-code-font-ligatures)))
;;   )
;; (add-hook 'prog-mode-hook 'add-pretty-lambda)
;; (add-hook 'org-mode-hook 'add-pretty-lambda)
;; -PreSym

;; TitleBar
(setq-default frame-title-format '("SPC-M-EMACS - " user-login-name "@" system-name " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t)
(setq initial-major-mode 'text-mode)
(setq initial-scratch-message "Present Day, Present Time...\n")
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

(when (display-graphic-p)
  ;; Set default font
  (cl-loop for font in '("SauceCodePro Nerd Font")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                 :font font
                                 :height (cond (*sys/mac* 160)
                                               (*sys/win32* 160)
                                               (t 160))))

  ;; Specify font for all unicode characters
  (cl-loop for font in '("Symbola" "Apple Symbols" "Symbol" "icons-in-terminal")
           when (font-installed-p font)
           return (set-fontset-font t 'unicode font nil 'prepend))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("WenQuanYi Micro Hei" "Microsoft Yahei")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))


(setq mac-command-modifier 'meta) ; make cmd key do Meta
(setq mac-option-modifier 'super) ; make opt key do Super
(setq mac-control-modifier 'control) ; make Control key do Control
(setq ns-function-modifier 'hyper)  ; make Fn key do Hyper

(add-to-list 'default-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))

(use-package nyan-mode
  :custom
  (nyan-cat-face-number 4)
  (nyan-animate-nyancat t)
  :hook
  (doom-modeline-mode . nyan-mode))


(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
