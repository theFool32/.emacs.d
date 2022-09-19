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
        rime-user-data-dir "~/Library/Rime"
        default-input-method "rime")

  :custom
  (rime-show-candidate 'posframe)
  (rime-posframe-properties (list :font "sarasa ui sc"
                                  :internal-border-width 10))
  (rime-disable-predicates
   '(rime-predicate-evil-mode-p
     ;; rime-predicate-after-alphabet-char-p
     rime-predicate-prog-in-code-p))

  :config
  (unless rime-emacs-module-header-root
    (setq rime-emacs-module-header-root "/opt/homebrew/opt/emacs-plus@29/include"))

  (define-key rime-mode-map (kbd "M-j") 'rime-force-enable)
  (define-key rime-mode-map (kbd "M-k") 'rime-inline-ascii)

  (defun +my/rime-sync ()
    ;; HACK: force emacs-rime to use userdb.
    ;; I am not sure if it is safe as the deploy may delete the old userdb.
    (interactive)
    (when rime--lib-loaded
      (let ((lock-name (concat rime-user-data-dir "/luna_pinyin.userdb/LOCK")))
        (when (file-exists-p lock-name)
          (delete-file lock-name)
          (rime-deploy)))))

  ;; (defun activate-default-input-method ()
  ;;   (interactive)
  ;;   (activate-input-method default-input-method))
  ;; (add-hook 'text-mode-hook 'activate-default-input-method)
  ;; (add-hook 'org-mode-hook 'activate-default-input-method)
  ;; (add-hook 'prog-mode-hook 'activate-default-input-method)
  )

(provide 'init-rime)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-rime.el ends here
