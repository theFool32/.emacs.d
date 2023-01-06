;;; init-prog.el ---

(use-package devdocs
  :straight (:host github :repo "astoff/devdocs.el")
  :commands (devdocs-lookup-at-point devdocs-search-at-point)
  :config
  (add-hook 'python-mode-hook
            (lambda() (setq-local devdocs-current-docs '("python~3.9" "PyTorch" "NumPy~1.20"))))
  (defun devdocs-lookup-at-point()
    (interactive)
    (devdocs-lookup devdocs-current-docs (thing-at-point 'symbol)))
  (defun devdocs-search-at-point()
    (interactive)
    (devdocs-search (thing-at-point 'symbol))))

(use-package markdown-mode
  :defer t
  :mode ("\\.md\\'" . markdown-mode))

(use-package project
  :straight nil
  :after-call +my/first-input-hook-fun
  :config
  (defun my/project-files-in-directory (dir)
    "Use `fd' to list files in DIR."
    (let* ((default-directory dir)
           (localdir (file-local-name (expand-file-name dir)))
           (command (format "fd -H -t f -0 . %s" localdir)))
      (project--remote-file-names
       (sort (split-string (shell-command-to-string command) "\0" t)
             #'string<))))

  (cl-defmethod project-files ((project (head local)) &optional dirs)
    "Override `project-files' to use `fd' in local projects."
    (mapcan #'my/project-files-in-directory
            (or dirs (list (project-root project)))))

  (cl-defmethod project-root ((project (head local)))
    (nth 1 project))
  (defun my/project-try-local (dir)
    "Determine if DIR is a non-Git project."
    (catch 'ret
      (let ((pr-flags '((".project" ".projectile" ".rc_config")
                        ("Makefile" "README.org" "README.md"))))
        (dolist (current-level pr-flags)
          (dolist (f current-level)
            (when-let ((root (locate-dominating-file dir f)))
              (throw 'ret (list 'local root))))))))

  (setq project-find-functions '(project-try-vc my/project-try-local))

  ;;  HACK: auto remember project
  (add-hook 'change-major-mode-hook (lambda ()
                                      (when (and (buffer-file-name)
                                                 (not (string-match-p "^/\\(?:ssh\\|scp\\|su\\|sudo\\)?:" (buffer-file-name)))
                                                 (not (string-match-p "straight/repos" (buffer-file-name)))
                                                 (fboundp 'project-current))
                                        (when-let ((root (+my/project-root)))
                                          (project-remember-project (project-current)))))))


(use-package format-all
  :diminish
  :commands format-all-buffer
  :hook ((prog-mode) . format-all-ensure-formatter)
  )

(provide 'init-prog)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-prog.el ends here
