;;; init-complete.el ---
;;
;; Filename: init-complete.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sat Nov 27 21:36:42 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated: Fri Mar  4 21:09:23 2022 (+0800)
;;           By: theFool32
;;     Update #: 540
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
;;
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

(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.05)
  (corfu-echo-documentation 0.3)
  ;; (corfu-commit-predicate nil)   ;; Do not commit selected candidates on next input
  ;; (corfu-quit-at-boundary t)     ;; Automatically quit at word boundary
  (corfu-quit-no-match 'separator)        ;; Automatically quit if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  (corfu-preselect-first nil)    ;; Disable candidate preselection
  ;; (corfu-echo-documentation nil) ;; Disable documentation in the echo area
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin
  (corfu-on-exact-match 'quit)

  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
        ([tab] . corfu-next)
        ("C-n" . corfu-next)
        ("S-SPC" . corfu-insert-separator)
        ("S-TAB" . corfu-previous)
        ("C-p" . corfu-previous)
        ([?\r] . newline)
        ([backtab] . corfu-previous))
  :init
  (corfu-global-mode)
  :config
  (defun corfu-beginning-of-prompt ()
    "Move to beginning of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (car completion-in-region--data)))

  (defun corfu-end-of-prompt ()
    "Move to end of completion input."
    (interactive)
    (corfu--goto -1)
    (goto-char (cadr completion-in-region--data)))

  ;; (define-key corfu-map [remap move-beginning-of-line] #'corfu-beginning-of-prompt)
  ;; (define-key corfu-map [remap move-end-of-line] #'corfu-end-of-prompt)
  (add-to-list 'corfu-auto-commands 'end-of-visual-line)
  (defun my/corfu-commit-predicate ()
    ;;     "Auto-commit candidates if:
    ;; 1. A "." is typed, except after a SPACE.
    ;; 2. A selection was made, aside from entering SPACE.
    ;; 3. Just one candidate exists, and we continue to non-symbol info.
    ;; 4. The 1st match is exact."
    (cond
     ((seq-contains-p (this-command-keys-vector) ?.)
      (or (string-empty-p (car corfu--input))
	      (not (string= (substring (car corfu--input) -1) " "))))

     ((/= corfu--index corfu--preselect) ; a selection was made
      (not (seq-contains-p (this-command-keys-vector) ? )))

     ((eq corfu--total 1) ;just one candidate
      (seq-intersection (this-command-keys-vector) [?: ?, ?\) ?\] ?\( ? ]))

     ((and corfu--input ; exact 1st match
	       (string-equal (substring (car corfu--input) corfu--base)
			             (car corfu--candidates)))
      (seq-intersection (this-command-keys-vector) [?: ?. ?, ?\) ?\] ?\" ?' ? ]))))
  ;; (setq corfu-commit-predicate #'my/corfu-commit-predicate)

  ;; https://github.com/minad/corfu/issues/12#issuecomment-869037519
  (advice-add 'corfu--setup :after 'evil-normalize-keymaps)
  (advice-add 'corfu--teardown :after 'evil-normalize-keymaps)
  (evil-make-overriding-map corfu-map)

  (defun corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if Vertico/Mct are not active."
    (unless (or (bound-and-true-p mct--active)
                (bound-and-true-p vertico--input))
      ;; (setq-local corfu-auto nil) Enable/disable auto completion
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-always-in-minibuffer 1)
  )

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'completion))

(use-package cape
  :after corfu
  ;; Bind dedicated completion commands
  :bind (("C-x C-f" . cape-file)
         ("C-x C-e" . cape-english)
         ("C-x C-l" . cape-line))

  :hook ((prog-mode . my/set-basic-capf)
         (org-mode . my/set-basic-capf)
         (lsp-completion-mode . my/set-lsp-capf))
  :config
  (setq dabbrev-upcase-means-case-search t)
  (setq case-fold-search nil)
  (fset 'cape-tabnine (cape-company-to-capf #'company-tabnine))
  (defun my/convert-super-capf (arg-capf)
    (list
     #'cape-file
     (cape-capf-buster
      (cape-super-capf arg-capf
                       #'cape-tabnine
                       #'tempel-expand)
      'equal)
     ;; #'cape-dabbrev
     )
    )

  (defun my/set-basic-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf (car completion-at-point-functions))))

  (defun my/set-lsp-capf ()
    (setq completion-category-defaults nil)
    (setq-local completion-at-point-functions (my/convert-super-capf #'lsp-completion-at-point))

    ;; HACK
    (when (derived-mode-p 'latex-mode)
      (add-to-list 'completion-at-point-functions #'+my/reftex-citation-completion))
    )

  (defun my/set-text-capf ()
    (setq-local completion-at-point-functions (append completion-at-point-functions
                                                      '(cape-english))))

  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'tempel-complete)
  (add-to-list 'completion-at-point-functions #'cape-file)

  (use-package tempel
    :init
    )

  ;; TODO use pure `corfu+cape' instead
  (use-package company-english-helper
    :straight (:host github :repo "manateelazycat/company-english-helper" :depth 1)
    :init
    (defvaralias 'company-candidates 'corfu--candidates)
    (defun company-grab-symbol ()
      "If point is at the end of a symbol, return it.
Otherwise, if point is not inside a symbol, return an empty string."
      (if (looking-at "\\_>")
          (buffer-substring (point) (save-excursion (skip-syntax-backward "w_")
                                                    (point)))
        (unless (and (char-after) (memq (char-syntax (char-after)) '(?w ?_)))
          "")))
    (provide 'company))
  (fset 'cape-english (cape-interactive-capf (cape-company-to-capf #'company-english-helper-search)))
  )

(use-package company-tabnine
  :defer 1
  :after corfu
  :hook (kill-emacs . company-tabnine-kill-process)
  :custom
  (company-tabnine-max-num-results 3))



(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  (kind-icon-blend-background nil)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)
  (setq kind-icon-mapping
        '((array "a" :icon "code-brackets" :face font-lock-type-face)
          (boolean "b" :icon "circle-half-full" :face font-lock-builtin-face)
          (class "c" :icon "video-input-component" :face font-lock-type-face) ;
          (color "#" :icon "palette" :face success) ;
          (constant "co" :icon "square-circle" :face font-lock-constant-face) ;
          (constructor "cn" :icon "cube-outline" :face font-lock-function-name-face) ;
          (enum-member "em" :icon "format-align-right" :face font-lock-builtin-face) ;
          (enum "e" :icon "server" :face font-lock-builtin-face) ;
          (event "ev" :icon "zip-box-outline" :face font-lock-warning-face) ;
          (field "fd" :icon "tag" :face font-lock-variable-name-face) ;
          (file "f" :icon "file-document-outline" :face font-lock-string-face) ;
          (folder "d" :icon "folder" :face font-lock-doc-face) ;
          (interface "if" :icon "share-variant" :face font-lock-type-face) ;
          (keyword "kw" :icon "image-filter-center-focus" :face font-lock-keyword-face) ;
          (macro "mc" :icon "lambda" :face font-lock-keyword-face)
          (method "m" :icon "cube-outline" :face font-lock-function-name-face) ;
          (function "f" :icon "cube-outline" :face font-lock-function-name-face) ;
          (module "{" :icon "view-module" :face font-lock-preprocessor-face) ;
          (numeric "nu" :icon "numeric" :face font-lock-builtin-face)
          (operator "op" :icon "plus-circle-outline" :face font-lock-comment-delimiter-face) ;
          (param "pa" :icon "tag" :face default)
          (property "pr" :icon "wrench" :face font-lock-variable-name-face) ;
          (reference "rf" :icon "collections-bookmark" :face font-lock-variable-name-face) ;
          (snippet "S" :icon "format-align-center" :face font-lock-string-face) ;
          (string "s" :icon "sticker-text-outline" :face font-lock-string-face)
          (struct "%" :icon "video-input-component" :face font-lock-variable-name-face) ;
          (text "tx" :icon "format-text" :face shadow)
          (type-parameter "tp" :icon "format-list-bulleted-type" :face font-lock-type-face)
          (unit "u" :icon "ruler-square" :face shadow)
          (value "v" :icon "format-align-right" :face font-lock-builtin-face) ;
          (variable "va" :icon "tag" :face font-lock-variable-name-face)
          (t "." :icon "file-find" :face shadow))) ;
  )

(use-package corfu-doc
  :after corfu
  :straight (:host github :repo "galeo/corfu-doc")
  :hook (corfu-mode . corfu-doc-mode)
  :bind (:map corfu-map
              ("M-d" . corfu-doc-toggle))
  )

(use-package copilot
  :after corfu
  :straight (:host github :repo "theFool32/copilot.el"
  ;; :straight (copilot :local-repo "/Users/lijie/dev/copilot.el"
                   :files ("dist" "copilot.el"))
  :ensure t
  ;; :hook (kill-emacs . copilot--kill-process)
  :config
  (add-hook 'post-command-hook (lambda ()
                                 (copilot-clear-overlay)
                                 (when (evil-insert-state-p)
                                   (copilot-complete))))

  (add-hook 'evil-insert-state-exit-hook (lambda () (copilot-clear-overlay)))


  (defun my/copilot-or-tempel-expand-or-next ()
    (interactive)
    (or (copilot-accept-completion)
        (if tempel--active
            (tempel-next 1)
          (call-interactively #'tempel-expand))))

  (with-eval-after-load 'general
    (general-define-key
     :keymaps '(evil-insert-state-map)
     "C-k" 'my/copilot-or-tempel-expand-or-next))
  )

(provide 'init-complete)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-complete ends here
