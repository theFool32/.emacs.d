;;; init-edit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-edit.el
;; Description: Initialize Editing Configuration
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Mar 28 13:25:24 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Fri Apr 24 10:37:49 2020 (+0800)
;;           By: theFool32
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d iedit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes iedit, awesome-pair, delete-block
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
  (require 'init-global-config))

;; Used for fold
;; Copy from doom-emacs
;;  TODO: Re-organize
;;
;;; Helpers

(defun +fold--ensure-hideshow-mode ()
  (unless (bound-and-true-p hs-minor-mode)
    (hs-minor-mode +1)))

(defun +fold--vimish-fold-p ()
  (and (featurep 'vimish-fold)
       (cl-some #'vimish-fold--vimish-overlay-p
                (overlays-at (point)))))

(defun +fold--outline-fold-p ()
  (and (or (bound-and-true-p outline-minor-mode)
           (derived-mode-p 'outline-mode))
       (outline-on-heading-p)))

(defun +fold--hideshow-fold-p ()
  (+fold--ensure-hideshow-mode)
  (save-excursion
    (ignore-errors
      (or (hs-looking-at-block-start-p)
          (hs-find-block-beginning)
          (unless (eolp)
            (end-of-line)
            (+fold--hideshow-fold-p))))))

;; NOTE: does this need more?
(defun +fold--ts-fold-p ()
  (and (bound-and-true-p tree-sitter-mode)
       (featurep 'ts-fold)))

(defun +fold--invisible-points (count)
  (let (points)
    (save-excursion
      (catch 'abort
        (if (< count 0) (beginning-of-line))
        (while (re-search-forward hs-block-start-regexp nil t
                                  (if (> count 0) 1 -1))
          (unless (invisible-p (point))
            (end-of-line)
            (when (hs-already-hidden-p)
              (push (point) points)
              (when (>= (length points) count)
                (throw 'abort nil))))
          (forward-line (if (> count 0) 1 -1)))))
    points))

(defmacro +fold-from-eol (&rest body)
  "Perform action after moving to the end of the line."
  `(save-excursion
     (end-of-line)
     ,@body))


;;
;;; Commands

;;;###autoload
(defun +fold/toggle ()
  "Toggle the fold at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-toggle))
          ((+fold--outline-fold-p)
           (cl-letf (((symbol-function #'outline-hide-subtree)
                      (symbol-function #'outline-hide-entry)))
             (outline-toggle-children)))
          ((+fold--ts-fold-p) (ts-fold-toggle))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-toggle-hiding))))))

;;;###autoload
(defun +fold/open ()
  "Open the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-unfold))
          ((+fold--outline-fold-p)
           (outline-show-children)
           (outline-show-entry))
          ((+fold--ts-fold-p) (ts-fold-open))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-show-block))))))

;;;###autoload
(defun +fold/close ()
  "Close the folded region at point.

Targets `vimmish-fold', `hideshow', `ts-fold' and `outline' folds."
  (interactive)
  (save-excursion
    (cond ((+fold--vimish-fold-p) (vimish-fold-refold))
          ((+fold--ts-fold-p) (ts-fold-close))
          ((+fold--hideshow-fold-p) (+fold-from-eol (hs-hide-block)))
          ((+fold--outline-fold-p) (outline-hide-subtree)))))

;;;###autoload
(defun +fold/open-all (&optional level)
  "Open folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (cond ((+fold--ts-fold-p)
         (ts-fold-open-all))
        ((featurep 'vimish-fold)
         (vimish-fold-unfold-all))
        ((save-excursion
           (+fold--ensure-hideshow-mode)
           (if (integerp level)
               (progn
                 (outline-hide-sublevels (max 1 (1- level)))
                 (hs-life-goes-on
                  (hs-hide-level-recursive (1- level) (point-min) (point-max))))
             (hs-show-all)
             (when (fboundp 'outline-show-all)
               (outline-show-all)))))))

;;;###autoload
(defun +fold/close-all (&optional level)
  "Close folds at LEVEL (or all folds if LEVEL is nil)."
  (interactive
   (list (if current-prefix-arg (prefix-numeric-value current-prefix-arg))))
  (save-excursion
    (if (+fold--ts-fold-p)
        (ts-fold-close-all)
      (progn
        (when (featurep 'vimish-fold)
          (vimish-fold-refold-all))
        (+fold--ensure-hideshow-mode)
        (hs-life-goes-on
         (if (integerp level)
             (hs-hide-level-recursive (1- level) (point-min) (point-max))
           (hs-hide-all)))))))

;;;###autoload
(defun +fold/next (count)
  "Jump to the next vimish fold, outline heading or folded region."
  (interactive "p")
  (cl-loop with orig-pt = (point)
           for fn
           in (list (lambda ()
                      (when (bound-and-true-p hs-block-start-regexp)
                        (car (+fold--invisible-points count))))
                    (lambda ()
                      (when (featurep 'vimish-fold)
                        (if (> count 0)
                            (evil-vimish-fold/next-fold count)
                          (evil-vimish-fold/previous-fold (- count))))
                      (if (/= (point) orig-pt) (point)))
                    (lambda ()
                      ;; ts-fold does not define movement functions so we need to do it ourselves
                      (when (+fold--ts-fold-p)
                        (let* ((arg-list (if (> count 0) ;; depending on direction we need to change the ranges
                                             (list (point) (point-max))
                                           (list (point-min) (point))))
                               (comp-fun (if (> count 0) ;; also depending on direction we need to change how we sort the list
                                             #'<
                                           #'>))
                               (ovs (cl-remove-if-not
                                     (lambda (ov)
                                       (eq (overlay-get ov 'creator) 'ts-fold))
                                     ;; `overlays-in' does not provide a list that is sorted
                                     ;; (in the way we need it atleast) so we need to sort it based on direction
                                     (cl-sort (apply #'overlays-in arg-list) comp-fun :key #'overlay-start))))
                          (if (and ovs (<= (abs count) (length ovs)))
                              (goto-char (overlay-start (nth (- (abs count) 1) ovs))))))))
           if (save-excursion (funcall fn))
           collect it into points
           finally do
           (if-let* ((pt (car (sort points (if (> count 0) #'< #'>)))))
               (goto-char pt)
             (message "No more folds %s point" (if (> count 0) "after" "before"))
             (goto-char orig-pt))))

;;;###autoload
(defun +fold/previous (count)
  "Jump to the previous vimish fold, outline heading or folded region."
  (interactive "p")
  (+fold/next (- count)))

(defface +fold-hideshow-folded-face
  `((t (:inherit font-lock-comment-face :weight light)))
  "Face to hightlight `hideshow' overlays."
  :group 'doom-themes)

;;;###autoload
(defun +fold-hideshow-haml-forward-sexp-fn (arg)
  (haml-forward-sexp arg)
  (move-beginning-of-line 1))

;;;###autoload
(defun +fold-hideshow-forward-block-by-indent-fn (_arg)
  (let ((start (current-indentation)))
    (forward-line)
    (unless (= start (current-indentation))
      (let ((range (+fold-hideshow-indent-range)))
        (goto-char (cadr range))
        (end-of-line)))))

;;;###autoload
(defun +fold-hideshow-set-up-overlay-fn (ov)
  (when (eq 'code (overlay-get ov 'hs))
    (when (featurep 'vimish-fold)
      (overlay-put
       ov 'before-string
       (propertize "…" 'display
                   (list vimish-fold-indication-mode
                         'empty-line
                         'vimish-fold-fringe))))
    (overlay-put
     ov 'display (propertize "  [...]  " 'face '+fold-hideshow-folded-face))))


;;
;;; Indentation detection

(defun +fold--hideshow-empty-line-p (_)
  (string= "" (string-trim (thing-at-point 'line 'no-props))))

(defun +fold--hideshow-geq-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (>= (current-indentation) base-indent)))

(defun +fold--hideshow-g-or-empty-p (base-indent)
  (or (+fold--hideshow-empty-line-p base-indent)
      (> (current-indentation) base-indent)))

(defun +fold--hideshow-seek (start direction before skip predicate base-indent)
  "Seeks forward (if direction is 1) or backward (if direction is -1) from start, until predicate
fails. If before is nil, it will return the first line where predicate fails, otherwise it returns
the last line where predicate holds."
  (save-excursion
    (goto-char start)
    (goto-char (point-at-bol))
    (let ((bnd (if (> 0 direction)
                   (point-min)
                 (point-max)))
          (pt (point)))
      (when skip (forward-line direction))
      (cl-loop while (and (/= (point) bnd) (funcall predicate base-indent))
               do (progn
                    (when before (setq pt (point-at-bol)))
                    (forward-line direction)
                    (unless before (setq pt (point-at-bol)))))
      pt)))

(defun +fold-hideshow-indent-range (&optional point)
  "Return the point at the begin and end of the text block with the same (or
greater) indentation. If `point' is supplied and non-nil it will return the
begin and end of the block surrounding point."
  (save-excursion
    (when point
      (goto-char point))
    (let ((base-indent (current-indentation))
          (begin (point))
          (end (point)))
      (setq begin (+fold--hideshow-seek begin -1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            begin (+fold--hideshow-seek begin 1 nil nil #'+fold--hideshow-g-or-empty-p base-indent)
            end   (+fold--hideshow-seek end 1 t nil #'+fold--hideshow-geq-or-empty-p base-indent)
            end   (+fold--hideshow-seek end -1 nil nil #'+fold--hideshow-empty-line-p base-indent))
      (list begin end base-indent))))


(use-package hideshow
  :config
  (setq hs-special-modes-alist
        (append
         '((latex-mode
            ;; LaTeX-find-matching-end needs to be inside the env
            ("\\\\begin{[a-zA-Z*]+}\\(\\)" 1)
            "\\\\end{[a-zA-Z*]+}"
            "%"
            (lambda (_arg)
              ;; Don't fold whole document, that's useless
              (unless (save-excursion
                        (search-backward "\\begin{document}"
                                         (line-beginning-position) t))
                (LaTeX-find-matching-end)))
            nil))
         hs-special-modes-alist
         '((t)))))
(use-package vimish-fold)
(use-package evil-vimish-fold
  :after evil
  :bind
  (([remap evil-toggle-fold]   . #'+fold/toggle)
   ([remap evil-close-fold]    . #'+fold/close)
   ([remap evil-open-fold]     . #'+fold/open)
   ([remap evil-open-fold-rec] . #'+fold/open)
   ([remap evil-close-folds]   . #'+fold/close-all)
   ([remap evil-open-folds]    . #'+fold/open-all))
  :commands (evil-vimish-fold/next-fold evil-vimish-fold/previous-fold
                                        evil-vimish-fold/delete evil-vimish-fold/delete-all
                                        evil-vimish-fold/create evil-vimish-fold/create-line)
  :config
  (vimish-fold-global-mode +1))

(use-package ts-fold
  :disabled
  :after tree-sitter
  :straight (ts-fold :type git :host github :repo "emacs-tree-sitter/ts-fold"))
;; end of fold


(use-package avy
  :diminish
  :demand t
  :commands (avy-goto-char avy-goto-line))


(use-package wgrep)

(provide 'init-edit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
