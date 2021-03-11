;;; private/prog/config.el -*- lexical-binding: t; -*-

;; Flycheck
(use-package! flycheck
  :config
  (setq-default flycheck-disabled-checkers
                '(python-pylint python-pycompile))
  (setq-default flycheck-temp-prefix ".flycheck")
  )


;; Company
(use-package! company
  :config
  (setq company-idle-delay 0
        company-tooltip-align-annotations t
        company-tooltip-limit 9
        company-minimum-prefix-length 1
        company-show-numbers t
        company-echo-delay (if (display-graphic-p) nil 0)
        company-require-match 'never
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-global-modes '(not erc-mode message-mode help-mode gud-mode eshell-mode shell-mode)
        )
  )

;; Company-Box
(use-package! company-box
  :diminish
  :defines company-box-icons-all-the-icons
  :hook (company-mode . company-box-mode)
  :init
  (setq company-box-backends-colors nil
        company-box-doc-delay 0.5)
  :config
  (delq! 'company-echo-metadata-frontend company-frontends)
  (add-to-list 'company-box-frame-parameters '(tab-bar-lines 0))
  (defadvice! +company-box-detect-deleted-frame-a (frame)
    :filter-return #'company-box--get-frame
    (if (frame-live-p frame) frame))
  (defadvice! +company-box-detect-deleted-doc-frame-a (_selection frame)
    :before #'company-box-doc
    (and company-box-doc-enable
         (frame-local-getq company-box-doc-frame frame)
         (not (frame-live-p (frame-local-getq company-box-doc-frame frame)))
         (frame-local-setq company-box-doc-frame nil frame)))
  (with-no-warnings
    ;; Prettify icons
    (defun my-company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))
    (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp))

  (declare-function all-the-icons-faicon 'all-the-icons)
  (declare-function all-the-icons-material 'all-the-icons)
  (declare-function all-the-icons-octicon 'all-the-icons)
  (setq company-box-icons-all-the-icons
        `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.8 :v-adjust -0.15))
          (Text . ,(all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.02))
          (Method . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Function . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Constructor . ,(all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.02 :face 'all-the-icons-purple))
          (Field . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Variable . ,(all-the-icons-octicon "tag" :height 0.85 :v-adjust 0 :face 'all-the-icons-lblue))
          (Class . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Interface . ,(all-the-icons-material "share" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Module . ,(all-the-icons-material "view_module" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Property . ,(all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.02))
          (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.8 :v-adjust -0.15))
          (Value . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-lblue))
          (Enum . ,(all-the-icons-material "storage" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.8 :v-adjust -0.15))
          (Snippet . ,(all-the-icons-material "format_align_center" :height 0.8 :v-adjust -0.15))
          (Color . ,(all-the-icons-material "palette" :height 0.8 :v-adjust -0.15))
          (File . ,(all-the-icons-faicon "file-o" :height 0.8 :v-adjust -0.02))
          (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.8 :v-adjust -0.15))
          (Folder . ,(all-the-icons-faicon "folder-open" :height 0.8 :v-adjust -0.02))
          (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.8 :v-adjust -0.15))
          (Constant . ,(all-the-icons-faicon "square-o" :height 0.8 :v-adjust -0.1))
          (Struct . ,(all-the-icons-material "settings_input_component" :height 0.8 :v-adjust -0.15 :face 'all-the-icons-orange))
          (Event . ,(all-the-icons-octicon "zap" :height 0.8 :v-adjust 0 :face 'all-the-icons-orange))
          (Operator . ,(all-the-icons-material "control_point" :height 0.8 :v-adjust -0.15))
          (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.02))
          (Template . ,(all-the-icons-material "format_align_left" :height 0.8 :v-adjust -0.15)))
        company-box-icons-alist 'company-box-icons-all-the-icons))

(use-package! company-tabnine
  :disabled
  :after company
  :config
  ;; (cl-pushnew 'company-tabnine (default-value 'company-backends))
  (defun company//sort-by-tabnine (candidates)
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backend))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-1
            candidates-2)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-2))
            (push candidate candidates-1)
            (puthash candidate t candidates-table)))
        (setq candidates-1 (nreverse candidates-1))
        (setq candidates-2 (nreverse candidates-2))
        (nconc (seq-take candidates-1 2)
               (seq-take candidates-2 2)
               (seq-drop candidates-1 2)
               (seq-drop candidates-2 2)))))

  (add-to-list 'company-transformers 'company//sort-by-tabnine t)
  ;; `:separate`  使得不同 backend 分开排序
  ;; (add-to-list 'company-backends '(company-capf :with company-tabnine :separate))
  (setq +lsp-company-backends '(company-capf :with company-tabnine :separate))
  ;; (set-company-backend! 'prog-mode 'company-tabnine 'company-capf 'company-yasnippet)
  )
