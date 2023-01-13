;; (defvar mu-path (format "%s%s" (string-trim-right (shell-command-to-string "brew --prefix mu")) "/share/emacs/site-lisp/mu/mu4e"))
(defvar mu-path (format "%s%s" (getenv "MU_PATH") "/share/emacs/site-lisp/mu/mu4e"))
(use-package mu4e
  :straight nil
  :load-path mu-path
  :if (executable-find "mu")
  :commands mu4e
  :hook ((mu4e-headers-mode . hl-line-mode)
         (mu4e-compose-mode . (lambda ()
                                (electric-indent-local-mode nil))))
  :init
  (provide 'html2text)
  :config
  (setenv "XAPIAN_CJK_NGRAM" "true")

  (setq
   mu4e-change-filenames-when-moving t
   mu4e-hide-index-messages t
   mu4e-context-policy 'pick-first
   mu4e-compose-context-policy nil
   mail-user-agent 'mu4e-user-agent
   send-mail-function 'smtpmail-send-it
   message-send-mail-function 'smtpmail-send-it
   smtpmail-smtp-service 587
   smtpmail-starttls-credentials (expand-file-name "~/.authinfo.gpg")
   smtpmail-stream-type  'starttls
   ;; mu4e-get-mail-command "offlineimap"
   mu4e-get-mail-command "true"
   mu4e-update-interval nil
   ;; 回复邮件插入邮件引用信息
   message-citation-line-function 'message-insert-formatted-citation-line
   message-citation-line-format "On %a, %b %d %Y, %f wrote:\n"
   mu4e-view-show-images t
   mu4e-view-image-max-width 800
   mu4e-compose-signature-auto-include t
   mu4e-compose-dont-reply-to-self t
   mu4e-use-fancy-chars nil
   mu4e-headers-include-related t
   mu4e-headers-skip-duplicates t
   mu4e-completing-read-function 'completing-read
   message-kill-buffer-on-exit t
   mu4e-confirm-quit nil
   mu4e-compose-format-flowed t
   mu4e-view-show-addresses t

   ;; 根据 from 邮件头使用正确的账户上下文发送 Email.
   message-sendmail-envelope-from 'header

   mu4e-maildir "~/.mail"

   mu4e-headers-thread-single-orphan-prefix '("─>" . "─▶")
   mu4e-headers-thread-orphan-prefix        '("┬>" . "┬▶ ")
   mu4e-headers-thread-connection-prefix    '("│ " . "│ ")
   mu4e-headers-thread-first-child-prefix   '("├>" . "├▶")
   mu4e-headers-thread-child-prefix         '("├>" . "├▶")
   mu4e-headers-thread-last-child-prefix    '("└>" . "╰▶")
   )

  (setq mu4e-bookmarks
        '( ("flag:unread AND NOT flag:trashed"      "Unread messages"   ?u)
           ("m:/Gmail/Inbox or m:/Outlook/Inbox or m:/XMU/Inbox" "Inbox" ?i)
           ("date:today..now"  "Today's messages"   ?t)
           ("date:7d..now"  "Last 7 days"           ?w)
           ("mime:image/*"  "Messages with images"  ?p)))

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 4)
          (:maildir . 25)
          (:from . 22)
          (:subject)))


  ;; 该函数基于当前所在的 maildir 来判定所账户上下文。
  (defun mu4e-message-maildir-matches (msg rx)
    (when rx
      (if (listp rx)
          ;; If rx is a list, try each one for a match
          (or (mu4e-message-maildir-matches msg (car rx))
              (mu4e-message-maildir-matches msg (cdr rx)))
        ;; Not a list, check rx
        (string-match rx (mu4e-message-field msg :maildir)))))

  ;; 中文搜索
  (defun mu4e-goodies~break-cjk-word (word)
    "Break CJK word into list of bi-grams like: 我爱你 -> 我爱 爱你"
    (if (or (<= (length word) 2)
            (equal (length word) (string-bytes word))) ; only ascii chars
        word
      (let ((pos nil)
            (char-list nil)
            (br-word nil))
        (if (setq pos (string-match ":" word))     ; like: "s:abc"
            (concat (substring word 0 (+ 1 pos))
                    (mu4e-goodies~break-cjk-word (substring word (+ 1 pos))))
          (if (memq 'ascii (find-charset-string word)) ; ascii mixed with others like: abc你好
              word
            (progn
              (setq char-list (split-string word "" t))
              (while (cdr char-list)
                (setq br-word (concat br-word (concat (car char-list) (cadr char-list)) " "))
                (setq char-list (cdr char-list)))
              br-word))))))
  (defun mu4e-goodies~break-cjk-query (expr)
    "Break CJK strings into bi-grams in query."
    (let ((word-list (split-string expr " " t))
          (new ""))
      (dolist (word word-list new)
        (setq new (concat new (mu4e-goodies~break-cjk-word word) " ")))))
  (setq mu4e-query-rewrite-function 'mu4e-goodies~break-cjk-query)

  (defun open-mail-in-browser (&optional mail)
    (interactive)
    (let ((mails '(("Gmail". "https://www.gmail.com")
                   ("Outlook" . "https://www.outlook.com")
                   ("XMU" . "https://stu.xmu.edu.cn"))))
      (browse-url
       (cdr (assoc (completing-read "Mail:" (mapcar 'car mails)) mails)))))
  (general-define-key :states '(normal)
                      :keymaps 'mu4e-main-mode-map
                      "o" #'open-mail-in-browser)
  )

(provide 'init-mail)
