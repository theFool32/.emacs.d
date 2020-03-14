;;; init-site.el --- Exports org to site.
;;; Commentary:
;;; Code:

(require 'init-org)

(use-package htmlize
  :after org
  :ensure t
  ;; :config
  ;; (setq htmlize-output-type 'font)
  )


(use-package ox-html
  :after org
  :ensure nil
  :config
  (setq
   ;; org-html-doctype "html5"
   ;; org-export-default-language "ch"
   user-full-name "Jie Li"))


(progn
  "Settings of `org-export'."
  (setq org-export-in-background t
        ;; Hide html built-in style and script.
        org-html-htmlize-output-type 'inline-css
        org-html-head-include-default-style nil
        org-html-head-include-scripts nil
        ))

;; FIXME: site-dir not works.
(defvar site-dir "~/site")

(use-package ox-publish
  :after org
  :ensure nil
  :config

  ;; org-publish-project-alist
  ;; ("project-name" :property value :property value ...)
  ;; ("project-name" :components ("project-name" "project-name" ...))

  (setq org-publish-project-alist
        '(("orgfiles"
           ;; ; Sources and destinations for files.
           :base-directory "~/site"  ;; local dir
           :publishing-directory "~/site" ;; :publishing-directory
           ;; :preparation-function
           ;; :complete-function

           ;; ; Selecting files
           :base-extension "org"
           :exclude "README.org"     ;; regexp
           ;; :include
           :recursive t

           ;; ; Publishing action
           :publishing-function org-html-publish-to-html

           ;; :htmlized-source


           ;; ;;; Options for the exporters

           ;; ; Generic properties
           ;; :archived-trees	org-export-with-archived-trees
           ;; :exclude-tags	org-export-exclude-tags
           :headline-levels 4 ;; org-export-headline-levels
           ;; :language	org-export-default-language
           ;; :preserve-breaks	org-export-preserve-breaks
           :section-numbers nil	;; org-export-with-section-numbers
           ;; :select-tags	org-export-select-tags
           :with-author "Jie Li" ;; org-export-with-author
           ;; :with-broken-links	org-export-with-broken-links
           ;; :with-clocks	t ;; org-export-with-clocks
           ;; :with-creator nil ;; org-export-with-creator
           ;; :with-date org-export-with-date
           ;; :with-drawers	org-export-with-drawers
           ;; :with-email	org-export-with-email
           ;; :with-emphasize	org-export-with-emphasize
           ;; :with-fixed-width org-export-with-fixed-width
           ;; :with-footnotes	org-export-with-footnotes
           ;; :with-latex	org-export-with-latex
           ;; :with-planning	org-export-with-planning
           :with-priority t ;; org-export-with-priority ;
           ;; :with-properties	org-export-with-properties
           ;; :with-special-strings	org-export-with-special-strings
           ;; :with-sub-superscript	org-export-with-sub-superscripts
           ;; :with-tables	org-export-with-tables
           ;; :with-tags	org-export-with-tags
           ;; :with-tasks	org-export-with-tasks
           ;; :with-timestamps	org-export-with-timestamps
           ;; :with-title	org-export-with-title
           :with-toc t ;; org-export-with-toc
           ;; :with-todo-keywords	org-export-with-todo-keywords


           ;; ; HTML specific properties
           ;; :html-allow-name-attribute-in-anchors	org-html-allow-name-attribute-in-anchors
           ;; :html-checkbox-type	org-html-checkbox-type
           ;; :html-container	org-html-container-element
           ;; :html-divs	org-html-divs
           :html-doctype "html5" ;; org-html-doctype
           ;; :html-extension	org-html-extension
           ;; :html-footnote-format nil ;; org-html-footnote-format
           ;; :html-footnote-separator	org-html-footnote-separator
           ;; :html-footnotes-section	org-html-footnotes-section
           ;; :html-format-drawer-function	org-html-format-drawer-function
           ;; :html-format-headline-function	org-html-format-headline-function
           ;; :html-format-inlinetask-function	org-html-format-inlinetask-function
           ;; :html-head-extra	org-html-head-extra
           ;; :html-head-include-default-style	org-html-head-include-default-style
           ;; :html-head-include-scripts	org-html-head-include-scripts
           ;; :html-head	org-html-head
           ;; :html-home/up-format	org-html-home/up-format
           ;; :html-html5-fancy	org-html-html5-fancy
           ;; :html-indent	org-html-indent
           ;; :html-infojs-options	org-html-infojs-options
           ;; :html-infojs-template	org-html-infojs-template
           ;; :html-inline-image-rules	org-html-inline-image-rules
           ;; :html-inline-images	org-html-inline-images
           ;; :html-link-home	org-html-link-home
           ;; :html-link-org-files-as-html	org-html-link-org-files-as-html
           ;; :html-link-up	org-html-link-up
           ;; :html-link-use-abs-url	org-html-link-use-abs-url
           ;; :html-mathjax-options	org-html-mathjax-options
           ;; :html-mathjax-template	org-html-mathjax-template
           ;; :html-metadata-timestamp-format	org-html-metadata-timestamp-format
           ;; :html-postamble-format t ;; org-html-postamble-format
           ;; :html-postamble t ;; org-html-postamble
           ;; :html-preamble-format	org-html-preamble-format
           ;; :html-preamble nil ;; org-html-preamble
           ;; :html-self-link-headlines	org-html-self-link-headlines
           ;; :html-table-align-individual-field	de{org-html-table-align-individual-fields
           ;; :html-table-attributes	org-html-table-default-attributes
           ;; :html-table-caption-above	org-html-table-caption-above
           ;; :html-table-data-tags	org-html-table-data-tags
           ;; :html-table-header-tags	org-html-table-header-tags
           ;; :html-table-row-tags	org-html-table-row-tags
           ;; :html-table-use-header-tags-for-first-column	org-html-table-use-header-tags-for-first-column
           ;; :html-tag-class-prefix	org-html-tag-class-prefix
           ;; :html-text-markup-alist	org-html-text-markup-alist
           ;; :html-todo-kwd-class-prefix	org-html-todo-kwd-class-prefix
           ;; :html-toplevel-hlevel	org-html-toplevel-hlevel
           ;; :html-use-infojs	org-html-use-infojs
           ;; :html-viewport	org-html-viewport
           ;; :html-wrap-src-lines	org-html-wrap-src-lines
           ;; :html-xml-declaration	org-html-xml-declaration


           ;; ; Markdown specific properties
           ;; :md-footnote-format	org-md-footnote-format
           ;; :md-footnotes-section	org-md-footnotes-section
           ;; :md-headline-style	org-md-headline-style


           ;; ; Other options
           :table-of-contents t
           ;; :style "<link rel=\"stylesheet\" href=\"../other/mystyle.css\" type=\"text/css\" />"
           )
          )))


(defun save-and-publish-file ()
  "Save current buffer and publish."
  (interactive)
  (save-buffer t)
  (org-publish-current-file t))


(define-minor-mode auto-save-and-publish-file-mode
  "Toggle auto save and publish current file."
  :global nil
  :lighter ""
  (if auto-save-and-publish-file-mode
      ;; When the mode is enabled
      (progn
        (add-hook 'after-save-hook #'save-and-publish-file :append :local))
    ;; When the mode is disabled
    (remove-hook 'after-save-hook #'save-and-publish-file :local)))


(use-package auto-save-and-publish-file-mode
  :after org
  :ensure nil
  :hook (org-mode))


(use-package simple-httpd
  :ensure t
  :config
  (setq httpd-root "~/site"))


(defun preview-current-buffer-in-browser ()
  "Open current buffer as html."
  (interactive)
  (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-base (buffer-name)) ".html")))
    (save-and-publish-file)
    (unless (httpd-running-p) (httpd-start))
    (browse-url fileurl)))



(provide 'init-site)
;;; init-site.el ends here
