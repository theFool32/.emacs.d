;;; init-hlightlight.el ---
;;
;; Filename: init-hlightlight.el
;; Description:
;; Author: Mingde (Matthew) Zeng
;; Maintainer:
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Feb 21 21:18:29 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 9
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

;; Highlight the current line
(use-package hl-line
  :ensure nil
  :custom-face (hl-line ((t (:extend t))))
  :hook ((after-init . global-hl-line-mode)
         ((term-mode vterm-mode) . hl-line-unload-function)))

;; Highlight matching parens
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t)
  :config
  (defun display-line-overlay (pos str &optional face)
    "Display line at POS as STR with FACE.

FACE defaults to inheriting from default and highlight."
    (let ((ol (save-excursion
                (goto-char pos)
                (make-overlay (line-beginning-position)
                              (line-end-position)))))
      (overlay-put ol 'display str)
      (overlay-put ol 'face
                   (or face '(:inherit highlight)))
      ol))

  (defvar-local show-paren--off-screen-overlay nil)
  (defun show-paren-off-screen (&rest _args)
    "Display matching line for off-screen paren."
    (when (overlayp show-paren--off-screen-overlay)
      (delete-overlay show-paren--off-screen-overlay))
    ;; check if it's appropriate to show match info,
    (when (and (overlay-buffer show-paren--overlay)
               (not (or cursor-in-echo-area
                        executing-kbd-macro
                        noninteractive
                        (minibufferp)
                        this-command))
               (and (not (bobp))
                    (memq (char-syntax (char-before)) '(?\) ?\$)))
               (= 1 (logand 1 (- (point)
                                 (save-excursion
                                   (forward-char -1)
                                   (skip-syntax-backward "/\\")
                                   (point))))))
      ;; rebind `minibuffer-message' called by
      ;; `blink-matching-open' to handle the overlay display
      (cl-letf (((symbol-function #'minibuffer-message)
                 (lambda (msg &rest args)
                   (let ((msg (apply #'format-message msg args)))
                     (setq show-paren--off-screen-overlay
                           (display-line-overlay
                            (window-start) msg ))))))
        (blink-matching-open))))
  (advice-add #'show-paren-function :after #'show-paren-off-screen))

;; Highlight symbols
(use-package symbol-overlay
  :diminish
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :custom-face (symbol-overlay-default-face ((t (:inherit (region bold)))))
  :hook ((prog-mode . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :init (setq symbol-overlay-idle-time 0.1)
  (with-eval-after-load 'all-the-icons
    (setq symbol-overlay-faces
          '((:inherit (all-the-icons-blue bold) :inverse-video t)
            (:inherit (all-the-icons-pink bold) :inverse-video t)
            (:inherit (all-the-icons-yellow bold) :inverse-video t)
            (:inherit (all-the-icons-purple bold) :inverse-video t)
            (:inherit (all-the-icons-red bold) :inverse-video t)
            (:inherit (all-the-icons-orange bold) :inverse-video t)
            (:inherit (all-the-icons-green bold) :inverse-video t)
            (:inherit (all-the-icons-cyan bold) :inverse-video t))))
  :config
  ;; Disable symbol highlighting while selecting
  (defun turn-off-symbol-overlay (&rest _)
    "Turn off symbol highlighting."
    (interactive)
    (symbol-overlay-mode -1))
  (advice-add #'set-mark :after #'turn-off-symbol-overlay)

  (defun turn-on-symbol-overlay (&rest _)
    "Turn on symbol highlighting."
    (interactive)
    (when (derived-mode-p 'prog-mode)
      (symbol-overlay-mode 1)))
  (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))

;; Highlight indentions
(use-package highlight-indent-guides
  :disabled
  :if *sys/gui*
  :diminish
  :hook ((prog-mode web-mode nxml-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-delay 0)
  (highlight-indent-guides-auto-character-face-perc 7)
  :functions (ivy-cleanup-string
              my-ivy-cleanup-indentation)
  :commands highlight-indent-guides--highlighter-default
  :functions my-indent-guides-for-all-but-first-column
  ;; :hook (prog-mode . highlight-indent-guides-mode)
  :init (setq highlight-indent-guides-method 'character
              highlight-indent-guides-responsive 'top)
  :config
  ;; Don't display indentations while editing with `company'
  (with-eval-after-load 'company
    (add-hook 'company-completion-started-hook
              (lambda (&rest _)
		        "Trun off indentation highlighting."
		        (when highlight-indent-guides-mode
                  (highlight-indent-guides-mode -1))))
    (add-hook 'company-after-completion-hook
              (lambda (&rest _)
		        "Trun on indentation highlighting."
		        (when (and (derived-mode-p 'prog-mode)
                           (not highlight-indent-guides-mode))
                  (highlight-indent-guides-mode 1)))))

  ;; Don't display first level of indentation
  (defun my-indent-guides-for-all-but-first-column (level responsive display)
    (unless (< level 1)
      (highlight-indent-guides--highlighter-default level responsive display)))
  (setq highlight-indent-guides-highlighter-function
	    #'my-indent-guides-for-all-but-first-column)

  ;; Don't display indentations in `swiper'
  ;; https://github.com/DarthFennec/highlight-indent-guides/issues/40
  (with-eval-after-load 'ivy
    (defun my-ivy-cleanup-indentation (str)
      "Clean up indentation highlighting in ivy minibuffer."
      (let ((pos 0)
            (next 0)
            (limit (length str))
            (prop 'highlight-indent-guides-prop))
	    (while (and pos next)
          (setq next (text-property-not-all pos limit prop nil str))
          (when next
            (setq pos (text-property-any next limit prop nil str))
            (ignore-errors
              (remove-text-properties next pos '(display nil face nil) str))))))
    (advice-add #'ivy-cleanup-string :after #'my-ivy-cleanup-indentation)))

;; Colorize color names in buffers
(use-package rainbow-mode
  :diminish
  :hook ((html-mode php-mode) . rainbow-mode)
  :config
  ;; HACK: Use overlay instead of text properties to override `hl-line' faces.
  ;; @see https://emacs.stackexchange.com/questions/36420
  (defun my-rainbow-colorize-match (color &optional match)
    (let* ((match (or match 0))
           (ov (make-overlay (match-beginning match) (match-end match))))
      (overlay-put ov 'ovrainbow t)
      (overlay-put ov 'face `((:foreground ,(if (> 0.5 (rainbow-x-color-luminance color))
                                                "white" "black"))
                              (:background ,color)))))
  (advice-add #'rainbow-colorize-match :override #'my-rainbow-colorize-match)

  (defun my-rainbow-clear-overlays ()
    "Clear all rainbow overlays."
    (remove-overlays (point-min) (point-max) 'ovrainbow t))
  (advice-add #'rainbow-turn-off :after #'my-rainbow-clear-overlays))

;; Highlight brackets according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :hook (after-init . global-hl-todo-mode)
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (cl-pushnew `(,keyword . ,(face-foreground 'error)) hl-todo-keyword-faces))
  (dolist (keyword '("WORKAROUND" "HACK" "TRICK"))
    (cl-pushnew `(,keyword . ,(face-foreground 'warning)) hl-todo-keyword-faces)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :custom-face
  (diff-hl-change ((t (:foreground ,(face-background 'highlight) :background nil))))
  (diff-hl-insert ((t (:background nil))))
  (diff-hl-delete ((t (:background nil))))
  :hook ((after-init . global-diff-hl-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-draw-borders nil)
  :config
  ;; Highlight on-the-fly
  (diff-hl-flydiff-mode 1)

  ;; Set fringe style
  (setq-default fringes-outside-margins t)

  (defun my-diff-hl-fringe-bmp-function (_type _pos)
    "Fringe bitmap function for use as `diff-hl-fringe-bmp-function'."
    (define-fringe-bitmap 'my-diff-hl-bmp
      (vector (if *sys/mac* #b11100000 #b11111100))
      1 8
      '(center t)))
  (setq diff-hl-fringe-bmp-function #'my-diff-hl-fringe-bmp-function)

  (unless (display-graphic-p)
    (setq diff-hl-margin-symbols-alist
          '((insert . " ") (delete . " ") (change . " ")
            (unknown . " ") (ignored . " ")))
    ;; Fall back to the display margin since the fringe is unavailable in tty
    (diff-hl-margin-mode 1)
    ;; Avoid restoring `diff-hl-margin-mode'
    (with-eval-after-load 'desktop
      (add-to-list 'desktop-minor-mode-table
                   '(diff-hl-margin-mode nil))))

  ;; Integration with magit
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; Highlight some operations
(use-package volatile-highlights
  :diminish
  :hook (after-init . volatile-highlights-mode)
  :config
  (when (fboundp 'pulse-momentary-highlight-region)
    (defun my-vhl-pulse (beg end &optional _buf face)
      "Pulse the changes."
      (pulse-momentary-highlight-region beg end face))
    (advice-add #'vhl/.make-hl :override #'my-vhl-pulse)))

;; Pulse current line
(use-package pulse
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region))))
  (pulse-highlight-face ((t (:inherit region))))
  :hook (((dumb-jump-after-jump
           imenu-after-jump) . my-recenter-and-pulse)
         ((bookmark-after-jump
           magit-diff-visit-file
           next-error) . my-recenter-and-pulse-line))
  :init
  (defun my-pulse-momentary-line (&rest _)
    "Pulse the current line."
    (pulse-momentary-highlight-one-line (point)))

  (defun my-pulse-momentary (&rest _)
    "Pulse the region or the current line."
    (if (fboundp 'xref-pulse-momentarily)
        (xref-pulse-momentarily)
      (my-pulse-momentary-line)))

  (defun my-recenter-and-pulse(&rest _)
    "Recenter and pulse the region or the current line."
    (recenter)
    (my-pulse-momentary))

  (defun my-recenter-and-pulse-line (&rest _)
    "Recenter and pulse the current line."
    (recenter)
    (my-pulse-momentary-line))

  (dolist (cmd '(recenter-top-bottom
                 other-window windmove-do-window-select
                 ace-window aw--select-window
                 pager-page-down pager-page-up
                 treemacs-select-window
                 symbol-overlay-basic-jump))
    (advice-add cmd :after #'my-pulse-momentary-line))

  (dolist (cmd '(pop-to-mark-command
                 pop-global-mark
                 goto-last-change))
    (advice-add cmd :after #'my-recenter-and-pulse)))

(provide 'init-highlight)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-hlightlight.el ends here
