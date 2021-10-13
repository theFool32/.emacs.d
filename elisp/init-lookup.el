;;; init-lookup.el ---
;;
;; Filename: init-lookup.el
;; Description:
;; Author: John
;; Maintainer:
;; Copyright (C) 2019 John
;; Created: Sun Aug  8 12:18:46 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 14
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

(require 'lookup/+autoloads)

(defvar +lookup-definition-functions
  '(+lookup-dictionary-definition-backend-fn
    +lookup-xref-definitions-backend-fn
    +lookup-dumb-jump-backend-fn
    +lookup-project-search-backend-fn
    +lookup-evil-goto-definition-backend-fn)
  "Functions for `+lookup/definition' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-implementations-functions ()
  "Function for `+lookup/implementations' to try. Stops at the first function to
return non-nil or change the current window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-type-definition-functions ()
  "Functions for `+lookup/type-definition' to try. Stops at the first function to
return non-nil or change the current window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-references-functions
  '(+lookup-thesaurus-definition-backend-fn
    +lookup-xref-references-backend-fn
    +lookup-project-search-backend-fn)
  "Functions for `+lookup/references' to try, before resorting to `dumb-jump'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-documentation-functions
  '(+lookup-online-backend-fn)
  "Functions for `+lookup/documentation' to try, before resorting to
`dumb-jump'. Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-file-functions
  '(+lookup-bug-reference-backend-fn
    +lookup-ffap-backend-fn)
  "Function for `+lookup/file' to try, before restoring to `find-file-at-point'.
Stops at the first function to return non-nil or change the current
window/point.
If the argument is interactive (satisfies `commandp'), it is called with
`call-interactively' (with no arguments). Otherwise, it is called with one
argument: the identifier at point. See `set-lookup-handlers!' about adding to
this list.")

(defvar +lookup-dictionary-prefer-offline t
  "If non-nil, look up dictionaries online.
Setting this to nil will force it to use offline backends, which may be less
than perfect, but available without an internet connection.
Used by `+lookup/dictionary-definition' and `+lookup/synonyms'.
For `+lookup/dictionary-definition', this is ignored on Mac, where Emacs users
Dictionary.app behind the scenes to get definitions.")


;;
;;; dumb-jump

(use-package dumb-jump
  :commands dumb-jump-result-follow
  :config
  (setq dumb-jump-default-project "~/.emacs.d/"
        dumb-jump-prefer-searcher 'rg
        dumb-jump-aggressive nil
        dumb-jump-quiet t
        dumb-jump-selector 'completing-read)
  (add-hook 'dumb-jump-after-jump-hook #'better-jumper-set-jump))


;;
;;; xref

;; The lookup commands are superior, and will consult xref if there are no
;; better backends available.
(global-set-key [remap xref-find-definitions] #'+lookup/definition)
(global-set-key [remap xref-find-references]  #'+lookup/references)

(use-package better-jumper
  :hook (+self/first-input . better-jumper-mode)
  :commands doom-set-jump-a doom-set-jump-maybe-a doom-set-jump-h
  :preface
  ;; REVIEW Suppress byte-compiler warning spawning a *Compile-Log* buffer at
  ;; startup. This can be removed once gilbertw1/better-jumper#2 is merged.
  (defvar better-jumper-local-mode nil)
  :init
  (global-set-key [remap evil-jump-forward]  #'better-jumper-jump-forward)
  (global-set-key [remap evil-jump-backward] #'better-jumper-jump-backward)
  (global-set-key [remap xref-pop-marker-stack] #'better-jumper-jump-backward)
  :config
  (defun doom-set-jump-a (fn &rest args)
    "Set a jump point and ensure fn doesn't set any new jump points."
    (better-jumper-set-jump (if (markerp (car args)) (car args)))
    (let ((evil--jumps-jumping t)
          (better-jumper--jumping t))
      (apply fn args)))

  (defun doom-set-jump-maybe-a (fn &rest args)
    "Set a jump point if fn returns non-nil."
    (let ((origin (point-marker))
          (result
           (let* ((evil--jumps-jumping t)
                  (better-jumper--jumping t))
             (apply fn args))))
      (unless result
        (with-current-buffer (marker-buffer origin)
          (better-jumper-set-jump
           (if (markerp (car args))
               (car args)
             origin))))
      result))

  (defun doom-set-jump-h ()
    "Run `better-jumper-set-jump' but return nil, for short-circuiting hooks."
    (better-jumper-set-jump)
    nil)

  ;; Creates a jump point before killing a buffer. This allows you to undo
  ;; killing a buffer easily (only works with file buffers though; it's not
  ;; possible to resurrect special buffers).
  ;;
  ;; I'm not advising `kill-buffer' because I only want this to affect
  ;; interactively killed buffers.
  (advice-add #'kill-current-buffer :around #'doom-set-jump-a)

  ;; Create a jump point before jumping with imenu.
  (advice-add #'imenu :around #'doom-set-jump-a))

(with-eval-after-load 'xref
  ;; We already have `projectile-find-tag' and `evil-jump-to-tag', no need for
  ;; xref to be one too.
  (remove-hook 'xref-backend-functions #'etags--xref-backend)
  ;; ...however, it breaks `projectile-find-tag', unless we put it back.
  (defun +lookup--projectile-find-tag-a (fn)
    (let ((xref-backend-functions '(etags--xref-backend t)))
      (funcall fn)))

  (advice-add #'projectile-find-tag :around #'+lookup--projectile-find-tag-a)

  ;; This integration is already built into evil
  ;; Use `better-jumper' instead of xref's marker stack
  (advice-add #'xref-push-marker-stack :around #'doom-set-jump-a)
  )

(provide 'init-lookup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-lookup.el ends here
