;;; init-ui-config.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-const))

(setq idle-update-delay 1.0)
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

(setq fast-but-imprecise-scrolling t)
(setq redisplay-skip-fontification-on-input t)
(setq frame-resize-pixelwise t)

;; TitleBar
(setq-default frame-title-format '("EMACS" " - %b"))
;; -TitleBar

;; YorN
(fset 'yes-or-no-p 'y-or-n-p)
;; -YorN

;; StartupScreen
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)
;; -StartupScreen

;; DisLineNum
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'org-mode-hook #'(lambda () (display-line-numbers-mode 0)))
;; Display column numbers in modeline
(column-number-mode 1)
(setq display-line-numbers-type 'relative)
;; -DisLineNum

;; DisTimeBat
(setq display-time-format "%m-%d %I:%M"
      display-time-mail-string ""
      display-time-default-load-average nil)
(display-time-mode t)
;; (display-battery-mode 1)
;; -DisTimeBat

;;Font

(when (display-graphic-p)
  (add-to-list 'default-frame-alist '(font . "CaskaydiaCove Nerd Font-14"))
  (set-fontset-font t '(#x4e00 . #x9fff) "Sarasa Mono SC")
  )


;; (add-to-list 'initial-frame-alist '(fullscreen . fullboth))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(custom-set-variables '(x-select-enable-clipboard t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))

(setq split-width-threshold 0
      split-height-threshold nil)

;; ATIPac
(use-package all-the-icons
  :if (display-graphic-p)
  :init (unless (or *sys/win32*
                    (find-font (font-spec :name "all-the-icons")))
          (all-the-icons-install-fonts t))
  :config
  (defun all-the-icons-reset ()
    "Reset (unmemoize/memoize) the icons."
    (interactive)
    (dolist (f '(all-the-icons-icon-for-file
                 all-the-icons-icon-for-mode
                 all-the-icons-icon-for-url
                 all-the-icons-icon-family-for-file
                 all-the-icons-icon-family-for-mode
                 all-the-icons-icon-family))
      (ignore-errors
        (memoize-restore f)
        (memoize f)))
    (message "Reset all-the-icons"))

  (add-to-list 'all-the-icons-mode-icon-alist
               '(xwidget-webkit-mode all-the-icons-faicon "chrome" :v-adjust -0.1 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-playlist-mode all-the-icons-material "playlist_play" :height 1.2 :v-adjust -0.2 :face 'all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(bongo-library-mode all-the-icons-material "library_music" :height 1.1 :v-adjust -0.2 :face 'all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-group-mode all-the-icons-fileicon "gnu" :face 'all-the-icons-silver))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-summary-mode all-the-icons-octicon "inbox" :height 1.0 :v-adjust 0.0 :face 'all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gnus-article-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(message-mode all-the-icons-octicon "mail" :height 1.1 :v-adjust 0.0 :face 'all-the-icons-lblue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(diff-mode all-the-icons-octicon "git-compare" :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(flycheck-error-list-mode all-the-icons-octicon "checklist" :height 1.1 :v-adjust 0.0 :face all-the-icons-lred))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.[bB][iI][nN]$" all-the-icons-octicon "file-binary" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.c?make$" all-the-icons-fileicon "gnu" :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.conf$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(conf-space-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-yellow))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(forge-topic-mode all-the-icons-alltheicon "git" :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(helpful-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
  (add-to-list 'all-the-icons-icon-alist
               '("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
  (add-to-list 'all-the-icons-icon-alist
               '("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
  (add-to-list 'all-the-icons-icon-alist
               '(".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-lorange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-dorange))
  (add-to-list 'all-the-icons-icon-alist
               '("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
  (add-to-list 'all-the-icons-mode-icon-alist
               '(gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue)))

(use-package olivetti
  :straight (:host github :repo "rnkn/olivetti")
  :commands (olivetti-mode olivetti-shrink olivetti-expand olivetti-set-width)
  :custom
  (olivetti-body-width 120))
;; -ATIPac

(use-package popper
  :defines popper-echo-dispatch-actions
  :hook (after-init . popper-mode)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Agenda Commands\\*"
          "\\*eldoc\\*"
          "\\*Calendar\\*"

          bookmark-bmenu-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*eshell.*\\*$" eshell-mode
          "^\\*shell.*\\*$"  shell-mode
          "^\\*term.*\\*$"   term-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lspce-hover\\*"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          ;; "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))

  (setq popper-echo-dispatch-actions t)
  :config
  (popper-echo-mode 1)
  (with-no-warnings
    (defun popper-close-window-hack (&rest _)
      "Close popper window via `C-g'."
      ;; `C-g' can deactivate region
      (when (and (called-interactively-p 'interactive)
                 (not (region-active-p))
                 popper-open-popup-alist)
        (let ((window (caar popper-open-popup-alist)))
          (when (window-live-p window)
            (delete-window window)))))
    (advice-add #'keyboard-quit :before #'popper-close-window-hack)))


;; DoomThemes
(use-package doom-themes
  :disabled
  :custom-face
  (cursor ((t (:background "BlanchedAlmond"))))
  :config
  (doom-themes-org-config)
  (load-theme 'doom-one t)
  )
;; -DoomThemes

(use-package ef-themes
  :init
  (ef-themes-select 'ef-summer)
  :config
  (with-eval-after-load 'org
    (setq org-todo-keyword-faces
          `(("TODO" . (:foreground ,(ef-themes-with-colors red-cooler) :weight bold))
            ("INPROCESS"  . ,(ef-themes-with-colors blue-cooler))
            ("PROJ"  . ,(ef-themes-with-colors cyan-cooler))
            ("WAITING" . ,(ef-themes-with-colors green-faint))
            ("DONE" . (:foreground ,(ef-themes-with-colors fg-alt) :strike-through t))
            ("CANCELED" . (:foreground ,(ef-themes-with-colors fg-dim) :weight bold :strike-through t))))
    )
  )

;; DoomModeline
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom-face
  (doom-modeline-buffer-modified ((t (:inherit (error bold) :background unspecified))))
  :custom
  ;; Don't compact font caches during GC. Windows Laggy Issue
  (inhibit-compacting-font-caches t)
  (doom-modeline-unicode-fallback t)
  (doom-modeline-icon t)
  (doom-modeline-env-version t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-height 15)
  (doom-modeline-bar-width 1)
  (doom-modeline-time-icon nil)
  )
;; -DoomModeline

(provide 'init-ui-config)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ui-config.el ends here
