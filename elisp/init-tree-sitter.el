;;; init-tree-sitter.el ---

(eval-when-compile
  (require 'init-const))

(use-package tree-sitter
  :defer t
  :commands tree-sitter-hl-mode
  :hook ((python-mode) . tree-sitter-hl-mode))

(use-package tree-sitter-langs
  :defer t
  :after tree-sitter)

;;  TODO: replace `tree-sitter'
(use-package treesit
  :if (treesit-available-p)
  :straight (:type built-in)
  :init
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (css . ("https://github.com/tree-sitter/tree-sitter-css"))
          (html . ("https://github.com/tree-sitter/tree-sitter-html"))
          (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
          ))
  :config
  (setq major-mode-remap-alist
        '((javascript-mode . js-ts-mode)
          (js-mode . js-ts-mode)
          ;; (python-mode . python-ts-mode)
          (js-json-mode . json-ts-mode)
          (sh-mode . bash-ts-mode)))
  )



(provide 'init-tree-sitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tree-sitter.el ends here
