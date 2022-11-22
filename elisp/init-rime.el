;;; init-rime.el ---

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
  :defer t
  :if +self/use-rime
  :init
  (setq rime-librime-root "~/.emacs.d/librime/dist/"
        rime-user-data-dir "~/.emacs.d/Rime/"
        default-input-method "rime")

  :custom
  (rime-show-candidate 'posframe)
  (rime-posframe-properties (list :font "sarasa ui sc"
                                  :internal-border-width 2))
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     ;; rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p))

  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@29/include"))

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
