(use-package mu4e
  ;;  TODO: disable `index' when open `mu4e-main'
  :ensure nil
  :straight nil
  :if (executable-find "mu")
  :commands mu4e
  :hook ((after-init . (lambda ()
                         (run-with-idle-timer 2 nil
                                              (lambda ()
                                                (mu4e t)))))
         (mu4e-headers-mode . hl-line-mode)
         (mu4e-compose-mode . (lambda ()
                                (electric-indent-local-mode nil))))
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

   ;; 启用 inline iamge 显示并定义显示图片最大宽度
   mu4e-view-show-images t
   mu4e-view-image-max-width 800

   ;; 自动包含邮件签名
   mu4e-compose-signature-auto-include t

   ;; 禁止回复给自己
   mu4e-compose-dont-reply-to-self t

   ;; This enables unicode chars to be used for things like flags in the message index screens.
   ;; I've disabled it because the font I am using doesn't support this very well. With this
   ;; disabled, regular ascii characters are used instead.
   mu4e-use-fancy-chars nil

   ;; This enabled the thread like viewing of email similar to gmail's UI.
   mu4e-headers-include-related t
   mu4e-headers-skip-duplicates t

   mu4e-completing-read-function 'completing-read

   message-kill-buffer-on-exit t

   mu4e-confirm-quit nil

   ;; mu4e sets up visual-line-mode and also fill (M-q to do the right thing
   ;; each paragraph is a single long line; at sending, emacs will add the
   ;; special line continuation characters.
   mu4e-compose-format-flowed t


   ;; show full addresses in view message (instead of just names
   ;; toggle per name with M-RET
   mu4e-view-show-addresses t

   ;; 根据 from 邮件头使用正确的账户上下文发送 Email.
   message-sendmail-envelope-from 'header

   mu4e-maildir "~/.mail"
   )

  ;; Sometimes html email is just not readable in a text based client, this lets me open the
  ;; email in my browser.
  (add-to-list 'mu4e-view-actions '("View in browser" . mu4e-action-view-in-browser) t)
  ;; 使用 shr 渲染当前 buffer
  (require 'shr)
  (defun shr-render-current-buffer ()
    "Render the selected region."
    (shr-render-region (point-min) (point-max)))
  (setq mu4e-html2text-command 'shr-render-current-buffer)


  (setq mu4e-bookmarks
        '( ("flag:unread AND NOT flag:trashed"      "Unread messages"   ?u)
           ("m:/Gmail/Inbox or m:/Outlook/Inbox or m:/XMU/Inbox" "Inbox" ?i)
           ("date:today..now"  "Today's messages"   ?t)
           ("date:7d..now"  "Last 7 days"           ?w)
           ("mime:image/*"  "Messages with images"  ?p)))

  ;; (setq mu4e-maildir-shortcuts
  ;;       '( ("/Gmail/Inbox" . ?g)
  ;;          ("/Gmail/存档" . ?G)
  ;;          ("/Outlook/Inbox" . ?k)
  ;;          ("/Outlook/存档" . ?K)
  ;;          ))

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 6)
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

  (require 'org-mu4e)
  ;;store link to message if in header view, not to header query
  (setq org-mu4e-link-query-in-headers-mode nil)

  (use-package mu4e-alert
    :disabled
    :config
    (mu4e-alert-set-default-style 'notifier)
    ;; (mu4e-alert-enable-notifications)
    (mu4e-alert-enable-mode-line-display)
    ;; (setq mu4e-alert-email-notification-types '(count subject))
    )


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
