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

(provide 'init-tree-sitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tree-sitter.el ends here
