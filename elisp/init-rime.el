;;; init-rime.el ---
;;
;; Filename: init-rime.el
;; Description: For rime
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Wed Apr 15 17:32:45 2020 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 88
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

;; karabiner:
;; shift for emacs-rime
;; {
;;     "description": "Shift for Emacs-Rime",
;;     "manipulators": [
;;         {
;;             "type": "basic",
;;             "from": {
;;                 "key_code": "left_shift",
;;                 "modifiers": {
;;                     "optional": [
;;                         "any"
;;                     ]
;;                 }
;;             },
;;             "to": [
;;                 {
;;                     "key_code": "left_shift",
;;                     "lazy": true
;;                 }
;;             ],
;;             "to_if_alone": [
;;                 {
;;                     "key_code": "backslash",
;;                     "modifiers": ["left_control"]
;;                 }
;;             ],
;;             "conditions": [
;;                 {
;;                 "type": "frontmost_application_if",
;;                 "bundle_identifiers": ["org\\.gnu\\.Emacs"]
;;                 }
;;             ]
;;         }
;;     ]
;; },

(use-package rime
  :if +self/use-rime
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist/"
        rime-user-data-dir "~/Library/Rime"
        default-input-method "rime")

  :custom
  (rime-show-candidate 'posframe)
  (rime-posframe-properties (list :font "sarasa ui sc"
                                  :internal-border-width 10))
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p
     rime-predicate-after-ascii-char-p
     rime-predicate-space-after-cc-p))

  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@28/include"))

  (defun +rime-sync ()
    ;; HACK: force emacs-rime to use userdb.
    ;; I am not sure if it is safe as the deploy may delete the old userdb.
    (interactive)
    (when rime--lib-loaded
      (let ((lock-name (concat rime-user-data-dir "/luna_pinyin.userdb/LOCK")))
        (when (file-exists-p lock-name)
          (delete-file lock-name)
          (rime-deploy)))))

  (defun activate-default-input-method ()
    (interactive)
    (activate-input-method default-input-method))
  ;; (add-hook 'text-mode-hook 'activate-default-input-method)
  ;; (add-hook 'org-mode-hook 'activate-default-input-method)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
