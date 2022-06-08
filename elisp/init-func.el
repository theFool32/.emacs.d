;;; init-func.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-global-config))

(defun +my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun +my/rename-file()
  "Rename file while using current file as default."
  (interactive)
  (let ((file-from (read-file-name "Move from: " default-directory buffer-file-name))
        (file-to (read-file-name "Move to:" default-directory)))
    (rename-file file-from file-to)
    (when (string= (file-truename file-from) (file-truename (buffer-file-name)))
      (kill-buffer)
      (find-file file-to))))

(defun +my/copy-file()
  "Copy file while using current file as default."
  (interactive)
  (copy-file
   (read-file-name "Copy from: " default-directory buffer-file-name)
   (read-file-name "Copy to:" default-directory)))

(defun +my/delete-file()
  "Delete file while using current file as default."
  (interactive)
  (let ((file-name (read-file-name "Delete: " default-directory (buffer-file-name))))
    (cond
     ((file-directory-p file-name) (delete-directory file-name t))
     ((file-exists-p file-name) (delete-file file-name))
     (t (message "Not found!")))
    (unless (file-exists-p (buffer-file-name))
      (kill-current-buffer))))

(defun +my/imenu ()
  "Consult-outline in `org-mode' unless imenu."
  (interactive)
  (if (derived-mode-p 'org-mode)
      (consult-outline)
    (consult-imenu)))


(defun +my/open-recent ()
  "Open recent directory in Dired or file otherwise."
  (interactive)
  (unless recentf-mode (recentf-mode 1))
  (let* ((candidates (if (derived-mode-p 'dired-mode)
                         (delete-dups
                          (append (mapcar 'file-name-directory recentf-list)))
                       (mapcar #'abbreviate-file-name
                               (-filter (lambda (filename) (not (file-directory-p filename)))
                                        recentf-list)))))
    (find-file
     (consult--read
      candidates
      :prompt "Find recent file: "
      :sort nil
      :require-match t
      :category 'file
      :state (consult--file-preview)
      :history 'file-name-history
      ))))

(defun +my/project-root (&optional dir)
  "Return the project root of DIR."
  (when-let* ((default-directory (or dir default-directory))
              (project (project-current)))
    (expand-file-name (if (fboundp 'project-root)
                          (project-root project)
                        (cdr project)))))

(defun +my/save-file ()
  "Save files including org agenda"
  (interactive)
  (if (derived-mode-p 'org-agenda-mode)
      (org-save-all-org-buffers)
    (save-buffer)))

(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
