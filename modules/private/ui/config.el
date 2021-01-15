;;; private/ui/config.el -*- lexical-binding: t; -*-


(after! hl-todo
  (setq hl-todo-keyword-faces
        `(("TODO"  . ,(face-foreground 'warning))
          ("HACK"  . ,(face-foreground 'warning))
          ("TEMP"  . ,(face-foreground 'warning))
          ("DONE"  . ,(face-foreground 'success))
          ("NOTE"  . ,(face-foreground 'success))
          ("DONT"  . ,(face-foreground 'error))
          ("DEBUG"  . ,(face-foreground 'error))
          ("FAIL"  . ,(face-foreground 'error))
          ("FIXME" . ,(face-foreground 'error))
          ("XXX"   . ,(face-foreground 'error))
          ("XXXX"  . ,(face-foreground 'error)))))


;; line number
(setq display-line-numbers-type 'relative)

;; trailing whitespace
(setq show-trailing-whitespace t)

(use-package! doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  (doom-themes-treemacs-theme "doom-colors")
  :config
  (load-theme 'doom-one t)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  )

(use-package! doom-modeline
  :custom
  (inhibit-compacting-font-caches t)
  (doom-modeline-buffer-file-name-style 'file-name)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-height 15)
  (doom-modeline-buffer-modification-icon t)
  )

(when (display-graphic-p)
  (cond (IS-MAC
         (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 16)
               doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 22)
               doom-unicode-font (font-spec :family "Apple Color Emoji" :size 8)
               doom-modeline-height 32))
        (IS-LINUX
         (setq doom-font (font-spec :family "CaskaydiaCove Nerd Font" :size 18 :weight 'regular)
               doom-big-font (font-spec :family "CaskaydiaCove Nerd Font" :size 22)
               doom-unicode-font (font-spec :family "Sarasa Nerd" :size 8)
               doom-modeline-height 32)))
  ;; fullscreen
  (add-to-list 'default-frame-alist '(fullscreen . maximized)))



(when IS-MAC
  ;; enable ligatures support
  ;; details here: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (ignore-errors
    (mac-auto-operator-composition-mode)))

(after! ibuffer
  ;; set ibuffer name column width
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size)))))

  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 50 50 :left :nil) " "
                (size-h 9 -1 :right) " "
                (mode 16 16 :left :elide) " "
                filename-and-process))))

(add-hook! 'process-menu-mode-hook
  (setq-local tabulated-list-format [("Process" 30 t)
                                     ("PID"      7 t)
                                     ("Status"   7 t)
                                     ("Buffer"  15 t)
                                     ("TTY"     12 t)
                                     ("Command"  0 t)]))


(when (display-graphic-p)
  (use-package nyan-mode
    :custom
    (nyan-cat-face-number 4)
    (nyan-animate-nyancat t)
    :hook
    (doom-modeline-mode . nyan-mode))
  )
