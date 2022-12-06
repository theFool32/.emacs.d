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
  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@29/include"))

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)

  (defun rime-predicate-tex-math-p ()
    (and (derived-mode-p 'tex-mode)
         (or (and (featurep 'tex-site)
                  (texmathp))
             (and rime--current-input-key
                  (or (= #x24 rime--current-input-key)
                      (= #x5c rime--current-input-key))
                  (or (= (point) (line-beginning-position))
                      (= #x20 (char-before))
                      (rime-predicate-after-ascii-char-p))))))

  (setq rime-disable-predicates
        '(rime-predicate-evil-mode-p
          rime-predicate-after-alphabet-char-p
          rime-predicate-prog-in-code-p
          rime-predicate-after-ascii-char-p
          rime-predicate-space-after-cc-p
          rime-predicate-tex-math-p))

  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
