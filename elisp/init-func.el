;;; init-func.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-global-config))

(defun +my/kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

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
                          (append (mapcar 'file-name-directory recentf-list))
                          ;; (append (mapcar (lambda (fname) (string-join (butlast (string-split fname "/")) "/")) recentf-list))
                          )
                       (mapcar #'abbreviate-file-name
                               ;; (-filter (lambda (filename) (not (file-directory-p filename)))
                               (-filter (lambda (filename) (not (string= "/" (substring filename -1))))
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


(defun hexcolour-luminance (color)
  "Calculate the luminance of a color string (e.g. \"#ffaa00\", \"blue\").
  This is 0.3 red + 0.59 green + 0.11 blue and always between 0 and 255."
  (let* ((values (x-color-values color))
         (r (car values))
         (g (cadr values))
         (b (caddr values)))
    (floor (+ (* .3 r) (* .59 g) (* .11 b)) 256)))
(defun hexcolour-add-to-font-lock ()
  (interactive)
  (font-lock-add-keywords
   nil
   `((,(concat "#[0-9a-fA-F]\\{3\\}[0-9a-fA-F]\\{3\\}?\\|"
               (regexp-opt (x-defined-colors) 'words))
      (0 (let ((colour (match-string-no-properties 0)))
           (put-text-property
            (match-beginning 0) (match-end 0)
            'face `((:foreground ,(if (> 128.0 (hexcolour-luminance colour))
                                      "white" "black"))
                    (:background ,colour)))))))))


(defvar +my/profiler--started nil)
(defun +my/profiler-toggle ()
  "Start ro stop profiler."
  (interactive)
  (if +my/profiler--started
      (progn
        (profiler-stop)
        (profiler-report)
        (setq +my/profiler--started nil))
    (profiler-start 'cpu+mem)
    (setq +my/profiler--started t)))


(defun +my/google-it (&optional word)
  "Google it"
  (interactive (list
                (if (use-region-p)
                    (buffer-substring-no-properties (region-beginning)
                                                    (region-end))
                  (thing-at-point 'symbol))))
  (browse-url (concat "https://www.google.com/search?q=" word)))

(provide 'init-func)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-func.el ends here
