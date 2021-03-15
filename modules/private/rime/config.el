;;; private/rime/config.el -*- lexical-binding: t; -*-

(use-package! rime
  :after-call after-find-file pre-command-hook
  :custom
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-librime-root "~/.emacs.d/librime/dist")
  (rime-user-data-dir "~/Library/Rime")
  :config
  (defadvice! +rime--posframe-display-result-a (args)
    "给 `rime--posframe-display-result' 传入的字符串加一个全角空
格，以解决 `posframe' 偶尔吃字的问题。"
    :filter-args #'rime--posframe-display-result
    (cl-destructuring-bind (result) args
      (let ((newresult (if (string-blank-p result)
                           result
                         (concat result "　"))))
        (list newresult))))

  (load! "+rime-probe-english"))
