;;; init-magit.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))


;; MagitPac
(use-package magit
  :defer t
  :if *git*
  :config
  ;; (global-auto-revert-mode -1)
  ;; (magit-auto-revert-mode -1)
  (defvar +magit-open-windows-in-direction 'right
    "What direction to open new windows from the status buffer.
For example, diffs and log buffers. Accepts `left', `right', `up', and `down'.")
  (defun +magit-display-buffer-fn (buffer)
    "Same as `magit-display-buffer-traditional', except...
- If opened from a commit window, it will open below it.
- Magit process windows are always opened in small windows below the current.
- Everything else will reuse the same window."
    (let ((buffer-mode (buffer-local-value 'major-mode buffer)))
      (display-buffer
       buffer (cond
               ((and (eq buffer-mode 'magit-status-mode)
                     (get-buffer-window buffer))
                '(display-buffer-reuse-window))
               ;; Any magit buffers opened from a commit window should open below
               ;; it. Also open magit process windows below.
               ((or (bound-and-true-p git-commit-mode)
                    (eq buffer-mode 'magit-process-mode))
                (let ((size (if (eq buffer-mode 'magit-process-mode)
                                0.35
                              0.7)))
                  `(display-buffer-below-selected
                    . ((window-height . ,(truncate (* (window-height) size)))))))

               ;; Everything else should reuse the current window.
               ((or (not (derived-mode-p 'magit-mode))
                    (not (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode))))
                '(display-buffer-same-window))

               ('(+magit--display-buffer-in-direction))))))

  (defun +magit--display-buffer-in-direction (buffer alist)
    "`display-buffer-alist' handler that opens BUFFER in a direction.
This differs from `display-buffer-in-direction' in one way: it will try to use a
window that already exists in that direction. It will split otherwise."
    (let ((direction (or (alist-get 'direction alist)
                         +magit-open-windows-in-direction))
          (origin-window (selected-window)))
      (if-let (window (window-in-direction direction))
          (unless magit-display-buffer-noselect
            (select-window window))
        (if-let (window (and (not (one-window-p))
                             (window-in-direction
                              (pcase direction
                                (`right 'left)
                                (`left 'right)
                                ((or `up `above) 'down)
                                ((or `down `below) 'up)))))
            (unless magit-display-buffer-noselect
              (select-window window))
          (let ((window (split-window nil nil direction)))
            (when (and (not magit-display-buffer-noselect)
                       (memq direction '(right down below)))
              (select-window window))
            (display-buffer-record-window 'reuse window buffer)
            (set-window-buffer window buffer)
            (set-window-parameter window 'quit-restore (list 'window 'window origin-window buffer))
            (set-window-prev-buffers window nil))))
      (unless magit-display-buffer-noselect
        (switch-to-buffer buffer t t)
        (selected-window))))

;;;###autoload
  (defun +magit/quit (&optional kill-buffer)
    "Bury the current magit buffer.

If KILL-BUFFER, kill this buffer instead of burying it.
If the buried/killed magit buffer was the last magit buffer open for this repo,
kill all magit buffers for this repo."
    (interactive "P")
    (let ((topdir (magit-toplevel)))
      (funcall magit-bury-buffer-function kill-buffer)
      (or (cl-find-if (lambda (win)
                        (with-selected-window win
                          (and (derived-mode-p 'magit-mode)
                               (equal magit--default-directory topdir))))
                      (window-list))
          (+magit/quit-all))))

;;;###autoload
  (defun +magit/quit-all ()
    "Kill all magit buffers for the current repository."
    (interactive)
    (mapc #'+magit--kill-buffer (magit-mode-get-buffers)))

  (defun +magit--kill-buffer (buf)
    "TODO"
    (when (and (bufferp buf) (buffer-live-p buf))
      (let ((process (get-buffer-process buf)))
        (if (not (processp process))
            (kill-buffer buf)
          (with-current-buffer buf
            (if (process-live-p process)
                (run-with-timer 5 nil #'+magit--kill-buffer buf)
              (kill-process process)
              (kill-buffer buf)))))))
  (setq magit-display-buffer-function #'+magit-display-buffer-fn)
  (setq magit-diff-refine-hunk (quote all))

  (general-define-key :states '(normal)
                      :keymaps 'magit-mode-map
                      "q" #'+magit/quit
                      "Q" #'+magit/quit-all)


  (defun magit-open-repo ()
    "open remote repo URL"
    (interactive)
    (let ((url (magit-get "remote" "origin" "url")))
      (progn
        (browse-url (if (string-match "^http" url)
                        url
                      (replace-regexp-in-string "\\(.*\\)@\\(.*\\):\\(.*\\)\\(\\.git?\\)"
                                                "https://\\2/\\3"
                                                url)))
        (message "opening repo %s" url))))

  (defun aborn/simple-git-commit-push ()
    "Simple commit current git project and push to its upstream."
    (interactive)
    (when (and buffer-file-name (buffer-modified-p))
      (save-buffer))
    (magit-stage-modified)
    (magit-diff-staged)
    (setq msg (read-string "Commit Message: "))
    (when (length= msg 0)
      (setq msg (format-time-string "commit by magit in emacs@%Y-%m-%d %H:%M:%S"
                                    (current-time))))
    (magit-call-git "commit" "-m" msg)
    (when (magit-get "remote" "origin" "url")
      (magit-push-current-to-upstream nil)
      (message "now do async push to %s" (magit-get "remote" "origin" "url")))
    (magit-mode-bury-buffer))
  )
;; -MagitPac

(use-package magit-todos
  :after magit
  :init
  ;; HACK
  (defun magit--tramp-asserts (dir)
    "override `magit--tramp-asserts'"
    nil))

;; Walk through git revisions of a file
(use-package git-timemachine
  :after magit
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer"))))
  :config
  (with-eval-after-load 'evil
    (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

(use-package magit-gitflow
  :after magit
  :hook (magit-mode . turn-on-magit-gitflow)
  :bind (:map magit-status-mode-map
              ("%" . magit-gitflow-popup)))

(use-package smerge-mode
  :straight nil
  :commands smerge-mode
  :hook ((find-file . (lambda ()
                        (save-excursion
                          (goto-char (point-min))
                          (when (re-search-forward "^<<<<<<< " nil t)
                            (smerge-mode 1)))))
         (smerge-mode . evil-normalize-keymaps))
  :config
  (local-leader-def
    :keymaps 'smerge-mode-map
    "n" '(smerge-next :wk "Next conflict")
    "p" '(smerge-prev :wk "Previous conflict")
    "RET" '(smerge-keep-current :wk "Accept current")
    "l" '(smerge-keep-lower :wk "Keep lower")
    "u" '(smerge-keep-upper :wk "Keep upper")
    "m" '(smerge-keep-mine :wk "Keep mine")
    "A" '(smerge-keep-all :wk "Keep all")))

(use-package forge
  :disabled
  :after magit)

(defvar gitmoji--all-emoji
  '(("增加新特性" . "feat:")
    ("bug 修复" . "fix:")
    ("文档改动" . "docs:")
    ("功能、交互优化" . "improve:")
    ("格式改动（不影响代码运行的变动，例如加空格、换行、分号等）" . "style:")
    ("重构代码" . "refactor:")
    ("性能相关优化" . "perf:")
    ("测试代码" . "test:")
    ("构建过程或辅助工具变动" . "chore:")
    ("回滚" . "revert:")
    ("合并" . "merge:")
    ("上传资源文件" . "resource:")))

(defun gitmoji-picker ()
  "Choose a gitmoji."
  (interactive)
  (let* ((choices gitmoji--all-emoji)
         (candidates (mapcar (lambda (cell)
                               (cons (format "%s — %s" (cdr cell) (car cell)) (concat (cdr cell) " ")))
                             choices)))
    (insert (cdr (assoc (completing-read "Choose a gitmoji " candidates) candidates)))
    (evil-insert-state)))


(provide 'init-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
