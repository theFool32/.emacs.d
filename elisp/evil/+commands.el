;;; editor/evil/+commands.el -*- lexical-binding: t; -*-

;;
;;; Custom commands
(evil-ex-define-cmd "git"         #'magit-status)         ; open magit status window
(evil-ex-define-cmd "gstage"      #'magit-stage)
(evil-ex-define-cmd "gunstage"    #'magit-unstage)
(evil-ex-define-cmd "gblame"      #'magit-blame)

;;; Dealing with buffers
(evil-ex-define-cmd "messages"    #'view-echo-area-messages)

;;; Project navigation
(evil-ex-define-cmd "a"           #'projectile-find-other-file)

;;; Project tools
(evil-ex-define-cmd "er[rors]"    #'flycheck-list-errors)

;;; Org-mode
(evil-ex-define-cmd "cap"         #'org-capture)

(provide 'evil/+commands)
