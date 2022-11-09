;;; init-shell.el ---

(when (and module-file-suffix
           (executable-find "cmake")
           (executable-find "libtool")
           (executable-find "make"))
  (use-package vterm
    :commands (vterm--internal vterm-posframe-toggle)
    :init
    (setq vterm-always-compile-module t)
    ;; (setq vterm-shell "tmux")
    (setq vterm-timer-delay 0.001
          process-adaptive-read-buffering nil)
    :config
    (evil-define-key 'insert vterm-mode-map (kbd "C-c") 'vterm-send-C-c)
    (evil-define-key 'normal vterm-mode-map (kbd "<escape>") 'vterm-send-escape)


    ;; (advice-add 'vterm-send-return :before (lambda ()
    ;;                                          (vterm-send-string " && printf '\\033[6 q'")))

    ;; https://github.com/akermu/emacs-libvterm#how-can-i-get-the-directory-tracking-in-a-more-understandable-way
    ;; (add-to-list 'vterm-eval-cmds '("update-pwd" (lambda (path) (setq default-directory path))))

    (with-no-warnings
      (defvar vterm-posframe--frame nil)

      (defun vterm-posframe-hidehandler (_)
        "Hidehandler used by `vterm-posframe-toggle'."
        (not (eq (selected-frame) posframe--frame)))

      (defun get-vterm-buffer ()
        "Return vterm buffer."
        (let ((buffer (get-buffer "vterm")))
          (if buffer
              buffer
            (vterm--internal #'ignore))))

      (defun vterm-posframe-toggle ()
        "Toggle `vterm' child frame."
        (interactive)
        (let ((buffer (get-vterm-buffer))
              (width  (max 80 (/ (frame-width) 2)))
              (height (/ (frame-height) 2)))
          (if (and vterm-posframe--frame
                   (frame-live-p vterm-posframe--frame)
                   (frame-visible-p vterm-posframe--frame))
              (progn
                (posframe-hide buffer)
                ;; Focus the parent frame
                (select-frame-set-input-focus (frame-parent vterm-posframe--frame)))
            (setq vterm-posframe--frame
                  (posframe-show
                   buffer
                   :poshandler #'posframe-poshandler-frame-center
                   :hidehandler #'vterm-posframe-hidehandler
                   :left-fringe 8
                   :right-fringe 8
                   :width width
                   :height height
                   :min-width width
                   :min-height height
                   :internal-border-width 3
                   :internal-border-color (face-foreground 'font-lock-comment-face nil t)
                   :background-color (face-background 'tooltip nil t)
                   :override-parameters '((cursor-type . 't))
                   :accept-focus t))
            ;; Focus the child frame
            (select-frame-set-input-focus vterm-posframe--frame))))
      )))

(provide 'init-shell)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-shell.el ends here
