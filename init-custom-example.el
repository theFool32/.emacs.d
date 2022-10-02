;;; init-custom-example.el ---
;;
;;   Copy this file to `init-custom.el', and customize options for yourself.


;; For ebib
(defconst +self/ebib-base-dir "")
;; For org-mode
(defconst +self/org-base-dir "")

(defconst +self/use-rc-to-sync t
  "Should we use `rc' to sync when saving")

(defconst +self/use-rime t
  "Should we use `rime'")

(defconst my-lsp 'lsp-bridge
  "Which language server to use, eglot or lsp-bridge")

(provide 'init-custom)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-custom-example.el ends here
