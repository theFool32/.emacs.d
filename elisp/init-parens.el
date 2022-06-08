;;; init-parens.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-global-config))

;; Automatic parenthesis pairing
(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            #'(lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t
                                 (,electric-pair-inhibit-predicate c)))))))

(use-package grammatical-edit
  :straight (:host github :repo "manateelazycat/grammatical-edit")
  :hook (prog-mode . grammatical-edit-mode)
  :config
  (define-key grammatical-edit-mode-map (kbd "(") 'grammatical-edit-open-round)
  (define-key grammatical-edit-mode-map (kbd "[") 'grammatical-edit-open-bracket)
  (define-key grammatical-edit-mode-map (kbd "{") 'grammatical-edit-open-curly)
  (define-key grammatical-edit-mode-map (kbd ")") 'grammatical-edit-close-round)
  (define-key grammatical-edit-mode-map (kbd "]") 'grammatical-edit-close-bracket)
  (define-key grammatical-edit-mode-map (kbd "}") 'grammatical-edit-close-curly)
  (define-key grammatical-edit-mode-map (kbd "=") 'grammatical-edit-equal)

  (define-key grammatical-edit-mode-map (kbd "%") 'grammatical-edit-match-paren)
  (define-key grammatical-edit-mode-map (kbd "\"") 'grammatical-edit-double-quote)
  (define-key grammatical-edit-mode-map (kbd "'") 'grammatical-edit-single-quote)

  (define-key grammatical-edit-mode-map (kbd "SPC") 'grammatical-edit-space)
  (define-key grammatical-edit-mode-map (kbd "RET") 'grammatical-edit-newline)

  (define-key grammatical-edit-mode-map (kbd "M-o") 'grammatical-edit-backward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-d") 'grammatical-edit-forward-delete)
  (define-key grammatical-edit-mode-map (kbd "C-k") 'grammatical-edit-kill)

  (define-key grammatical-edit-mode-map (kbd "M-\"") 'grammatical-edit-wrap-double-quote)
  (define-key grammatical-edit-mode-map (kbd "M-'") 'grammatical-edit-wrap-single-quote)
  (define-key grammatical-edit-mode-map (kbd "M-[") 'grammatical-edit-wrap-bracket)
  (define-key grammatical-edit-mode-map (kbd "M-{") 'grammatical-edit-wrap-curly)
  (define-key grammatical-edit-mode-map (kbd "M-(") 'grammatical-edit-wrap-round)
  (define-key grammatical-edit-mode-map (kbd "M-)") 'grammatical-edit-unwrap)

  (define-key grammatical-edit-mode-map (kbd "M-p") 'grammatical-edit-jump-right)
  (define-key grammatical-edit-mode-map (kbd "M-n") 'grammatical-edit-jump-left)
  (define-key grammatical-edit-mode-map (kbd "M-:") 'grammatical-edit-jump-out-pair-and-newline)
  )

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
