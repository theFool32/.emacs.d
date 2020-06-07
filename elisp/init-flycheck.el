;;; init-flycheck.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-flycheck.el
;; Description: Initialize Flycheck
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 10:08:22 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sun Jun  7 15:10:00 2020 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d flycheck
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes flycheck
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

;; FlyCheckPac
(use-package flycheck
  :defer t
  :diminish
  :hook ((prog-mode markdown-mode LaTeX-mode) . flycheck-mode)
  :custom
  (flycheck-global-modes
   '(not text-mode outline-mode fundamental-mode org-mode
         diff-mode shell-mode eshell-mode term-mode))
  (flycheck-emacs-lisp-load-path 'inherit)
  (flycheck-indication-mode 'right-fringe)
  :init
  (if *sys/gui*
      (use-package flycheck-posframe
        :custom-face (flycheck-posframe-border-face ((t (:inherit default))))
        :hook (flycheck-mode . flycheck-posframe-mode)
        :custom
        (flycheck-posframe-border-width 1)
        (flycheck-posframe-inhibit-functions
         '((lambda (&rest _) (bound-and-true-p company-backend)))))
    (use-package flycheck-pos-tip
      :defines flycheck-pos-tip-timeout
      :hook (flycheck-mode . flycheck-pos-tip-mode)
      :custom (flycheck-pos-tip-timeout 30)))
  :config
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center))
  (setq flycheck-check-syntax-automatically '(save))

  (if (and *sys/mac* nil)
      ;; Copy from https://raw.githubusercontent.com/xuchunyang/emacs.d/master/lisp/flycheck-languagetool.el
      ;; Not very useful
      (progn
        (defun flycheck-languagetool--parser (output checker buffer)
          (mapcar
           (lambda (match)
             (let-alist match
               (flycheck-error-new-at
                (line-number-at-pos (1+ .offset))
                (save-excursion
                  (goto-char (1+ .offset))
                  ;; Flycheck 1-base, Emacs 0-base
                  (1+ (current-column)))
                'warning
                .message
                :id .rule.id
                :checker checker
                :buffer buffer
                :filename (buffer-file-name buffer))))
           (alist-get 'matches (car (flycheck-parse-json output)))))

        (flycheck-def-option-var flycheck-languagetool-commandline-jar
            (expand-file-name "/usr/local/Cellar/languagetool/4.9.1/libexec/languagetool-commandline.jar")
            languagetool
          "The path of languagetool-commandline.jar."
          :type '(file :must-match t))

        (flycheck-def-option-var flycheck-languagetool-language "en-US" languagetool
          "The language code of the text to check."
          :type '(string :tag "Language")
          :safe #'stringp)
        (make-variable-buffer-local 'flycheck-languagetool-language)

        (flycheck-define-checker languagetool
          "Style and grammar checker using LanguageTool."
          :command ("java"
                    (option "-jar" flycheck-languagetool-commandline-jar)
                    (option "-l" flycheck-languagetool-language)
                    "-l" "en-US"
                    "--json"
                    "-")
          :standard-input t
          :error-parser flycheck-languagetool--parser
          :modes (text-mode markdown-mode LaTeX-mode latex-mode)
          :predicate
          (lambda ()
            (and flycheck-languagetool-commandline-jar
                 (file-exists-p flycheck-languagetool-commandline-jar)))
          :verify
          (lambda (_)
            (let ((have-jar
                   (and flycheck-languagetool-commandline-jar
                        (file-exists-p flycheck-languagetool-commandline-jar))))
              (list
               (flycheck-verification-result-new
                :label (or flycheck-languagetool-commandline-jar
                           "languagetool-commandline.jar")
                :message (if have-jar "exist" "doesn't exist")
                :face (if have-jar 'success '(bold error)))))))

        (add-to-list 'flycheck-checkers 'languagetool)))
  )
;; -FlyCheckPac

;; TODO: to slow to use
;; (use-package flycheck-grammarly)
(if *sys/mac*
    ;; https://github.com/mmagnus/emacs-grammarly
    (progn
      (defun grammarly-push ()
        "Save region to a tempfile and run Grammarly on it."
        (interactive)
        (kill-region (region-beginning) (region-end))
        ;;(insert "<<here>>")
        (call-process-shell-command "osascript ~/bin/push.scpt")
        )

      (defun grammarly-pull()
        "Save region to a tempfile and run Grammarly on it."
        (interactive)
        (call-process-shell-command "osascript ~/bin/pull.scpt")
        (yank)
        ))
  )



(provide 'init-flycheck)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-flycheck.el ends here
