;;; init-parens.el --- -*- lexical-binding: t -*-

(eval-when-compile
  (require 'init-global-config))

(use-package paren
  :hook (after-init . show-paren-mode)
  :init (setq show-paren-when-point-inside-paren t
              show-paren-when-point-in-periphery t
              show-paren-style 'parenthesis
              show-paren-context-when-offscreen 'overlay))

;; Automatic parenthesis pairing
(use-package elec-pair
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
  :config
  ;; disable <> auto pairing in electric-pair-mode for org-mode
  (add-hook 'org-mode-hook
            #'(lambda ()
                (setq-local electric-pair-inhibit-predicate
                            `(lambda (c)
                               (if (char-equal c ?<) t
                                 (,electric-pair-inhibit-predicate c)))))))

(with-eval-after-load 'evil
  (defun my/evil-paren-range (count beg end type inclusive)
    "Get minimum range of paren text object.
COUNT, BEG, END, TYPE is used.  If INCLUSIVE is t, the text object is inclusive."
    (let* ((cur_point (point))
           (parens '("()" "[]" "{}" "<>"))
           (quotes '("\"" "'"))
           (pqs (append parens quotes))
           range
           found-range)
      ;;  HACK: ' is widely used in lisp
      (when (derived-mode-p 'emacs-lisp-mode)
        (setq pqs (butlast pqs)))
      (dolist (p pqs)
        (ignore-errors
          (if (member p parens)
              (setq range (evil-select-paren (aref p 0) (aref p 1) beg end type count inclusive))
            (setq range (evil-select-quote (aref p 0) beg end type count))))
        (when (and range
                   (<= (nth 0 range) cur_point)
                   (>= (nth 1 range) cur_point))
          (cond
           (found-range
            (when (and
                   (< (- (nth 1 range) (nth 0 range))
                      (- (nth 1 found-range) (nth 0 found-range))))
              (setf (nth 0 found-range) (nth 0 range))
              (setf (nth 1 found-range) (nth 1 range))))
           (t
            (setq found-range range)))))
      found-range))
  (evil-define-text-object my/evil-a-paren (count &optional beg end type)
    "Select a paren."
    :extend-selection t
    (my/evil-paren-range count beg end type t))

  (evil-define-text-object my/evil-inner-paren (count &optional beg end type)
    "Select 'inner' paren."
    :extend-selection nil
    (my/evil-paren-range count beg end type nil))
  (define-key evil-inner-text-objects-map "g" #'my/evil-inner-paren)
  (define-key evil-outer-text-objects-map "g" #'my/evil-a-paren)

  (defun my/edit-kill ()
    (interactive)
    (let* ((parens '("(" ")" "[" "]" "{" "}" "<" ">" "\""))
           (char (string (char-after))))
      ;;  HACK: ' is widely used in lisp
      (when (not (derived-mode-p 'emacs-lisp-mode))
        (push "'" parens))
      (setq unread-command-events
            (append (apply 'vconcat (mapcar 'kbd
                                            ;;  FIXME: not work on the right side of a string
                                            (if (and (not (nth 3 (syntax-ppss)))
                                                     (member char parens))
                                                `("d" "a" ,char)
                                              ;; '("d" "i" "g")
                                              ;;  HACK: Don't know why dig not works
                                              '("v" "i" "g" "x")
                                              ))) nil))))
  (with-eval-after-load 'general
    (general-define-key
     :keymaps '(evil-normal-state-map)
     "C-k" 'my/edit-kill)))

(provide 'init-parens)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-parens.el ends here
