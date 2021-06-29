;;; init-company.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-company.el
;; Description: Initialize Company
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Thu Feb  6 16:25:12 2020 (-0500)
;; Version: 2.0.0
;; Last-Updated:
;;           By:
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d company company-tabnine
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes company
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
  (require 'init-const)
  (require 'init-bindings))


(defun smarter-yas-expand-next-field-complete ()
  "Try to `yas-expand' and `yas-next-field' at current cursor position.

If failed try to complete the common part with `company-complete-common'"
  (interactive)
  (if yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (progn
              (call-interactively 'company-abort)
              (call-interactively 'company-yasnippet))
            )
          ))
    ;; FIXME: c-k tab c-k
    (company-complete-common)
    )
  )


;;;###autoload
(defvar +company-backend-alist
  '((text-mode company-tabnine company-yasnippet company-dabbrev)
    ;; (prog-mode company-files company-capf company-yasnippet)
    (prog-mode company-files (company-capf :with company-tabnine :separate) company-yasnippet)
    (conf-mode company-capf company-dabbrev-code company-yasnippet))
  "An alist matching modes to company backends. The backends for any mode is
built from this.")

;;;###autodef
(defun set-company-backend! (modes &rest backends)
  "Prepends BACKENDS (in order) to `company-backends' in MODES.

MODES should be one symbol or a list of them, representing major or minor modes.
This will overwrite backends for MODES on consecutive uses.

If the car of BACKENDS is nil, unset the backends for MODES.

Examples:

  (set-company-backend! 'js2-mode
    'company-tide 'company-yasnippet)

  (set-company-backend! 'sh-mode
    '(company-shell :with company-yasnippet))

  (set-company-backend! '(c-mode c++-mode)
    '(:separate company-irony-c-headers company-irony))

  (set-company-backend! 'sh-mode nil)  ; unsets backends for sh-mode"
  (declare (indent defun))
  (dolist (mode (doom-enlist modes))
    (if (null (car backends))
        (setq +company-backend-alist
              (delq (assq mode +company-backend-alist)
                    +company-backend-alist))
      (setf (alist-get mode +company-backend-alist)
            backends))))


;;
;;; Library

(defun +company--backends ()
  (let (backends)
    (let ((mode major-mode)
          (modes (list major-mode)))
      (while (setq mode (get mode 'derived-mode-parent))
        (push mode modes))
      (dolist (mode modes)
        (dolist (backend (append (cdr (assq mode +company-backend-alist))
                                 (default-value 'company-backends)))
          (push backend backends)))
      (delete-dups
       (append (cl-loop for (mode . backends) in +company-backend-alist
                        if (or (eq major-mode mode)  ; major modes
                               (and (boundp mode)
                                    (symbol-value mode))) ; minor modes
                        append backends)
               (nreverse backends))))))


;;
;;; Hooks

;;;###autoload
(defun +company-init-backends-h ()
  "Set `company-backends' for the current buffer."
  (if (not company-mode)
      (remove-hook 'change-major-mode-after-body-hook #'+company-init-backends-h 'local)
    (unless (eq major-mode 'fundamental-mode)
      (setq-local company-backends (+company--backends)))
    (add-hook 'change-major-mode-after-body-hook #'+company-init-backends-h nil 'local)))

(put '+company-init-backends-h 'permanent-local-hook t)

;; ComPac
(use-package company
  :diminish company-mode
  :hook ((prog-mode LaTeX-mode) . company-mode)
  :init
  (company-tng-mode)
  (add-hook 'company-mode-hook #'+company-init-backends-h)
  :custom
  (company-files-chop-trailing-slash nil)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  ;; Don't use company in the following modes
  (company-global-modes '(not shell-mode))
  ;; Trigger completion immediately.
  (company-idle-delay 0)
  ;; Number the candidates (use M-1, M-2 etc to select completions).
  (company-show-numbers nil)
  ;; (company-tooltip-minimum-width 80)
  (company-quickelp-delay nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-frontend
                       company-echo-metadata-frontend))
  :config
  (setq company-backends '(company-files company-dabbrev))
  (global-company-mode 1)

  (general-define-key
   :keymaps '(company-active-map evil-insert-state-map)
   "C-k" 'smarter-yas-expand-next-field-complete)
  (general-def 'insert
    :prefix "C-x"
    "C-f" 'company-files)

  (with-eval-after-load 'orderless
    (defvar-local +company-completion-styles '(partial-completion))
    (defvar-local +completion-styles nil)
    (defun set-company-completion-style (backend)
      (setq +completion-styles completion-styles)
      (setq completion-styles +company-completion-styles))
    (defun restore-company-completion-style ()
      (when +completion-styles
        (setq completion-styles +completion-styles)
        (setq +completion-styles nil)))

    (add-hook 'company-completion-started-hook #'set-company-completion-style)
    ;; (add-hook 'company-completion-cancelled-hook #'restore-company-completion-style)
    ;; (add-hook 'company-completion-finished-hook #'restore-company-completion-style)
    (add-hook 'evil-normal-state-entry-hook #'restore-company-completion-style))
  )
;; -ComPac
(use-package company-prescient
  :init (company-prescient-mode 1))


(use-package company-box
  :after company
  :hook (company-mode . company-box-mode))

;; CompanyTabNinePac
(use-package company-tabnine
  :straight (:host github :repo "theFool32/company-tabnine" :depth 1)
  :defer 1
  :after company
  :custom
  (company-tabnine-max-num-results 3)
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :bind
  (:map company-mode-map
        ("C-x C-t" . company-tabnine))
  :init
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-lsp 2)
               (seq-take candidates-tabnine 2)
               (seq-drop candidates-lsp 2)
               (seq-drop candidates-tabnine 2)
               ))))

  :config
  (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  )
;; -CompanyTabNinePac

(provide 'init-company)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-company.el ends here
