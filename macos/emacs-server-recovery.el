;;; emacs-server-recovery.el --- Recover deleted daemon sockets -*- lexical-binding: t -*-

;;; Commentary:
;; A temporary-directory cleanup can unlink the socket while Emacs still
;; listens on it.  Recreate only that listener, preserving connected clients.

;;; Code:

(require 'server)

(defvar +my/desktop-server-recovery-timer nil
  "Timer checking that the daemon's local socket still exists.")

(defun +my/desktop-recover-server-socket ()
  "Restore a missing local server socket without closing clients or buffers."
  (interactive)
  (when (and (daemonp)
             (not server-use-tcp)
             (processp server-process)
             (eq (process-status server-process) 'listen)
             (not (file-exists-p (expand-file-name server-name server-socket-dir))))
    ;; `server-start' normally disconnects every entry in `server-clients'.
    ;; Hide that list dynamically so only the listening process is replaced.
    (let ((server-clients nil))
      (server-start))
    (message "Restored Emacs server socket: %s"
             (expand-file-name server-name server-socket-dir))
    t))

(defun +my/desktop-check-server-socket ()
  "Check the daemon socket, reporting recovery errors without stopping Emacs."
  (condition-case error-data
      (+my/desktop-recover-server-socket)
    (error (message "Emacs server socket recovery failed: %s"
                    (error-message-string error-data)))))

(when (daemonp)
  (when (timerp +my/desktop-server-recovery-timer)
    (cancel-timer +my/desktop-server-recovery-timer))
  (setq +my/desktop-server-recovery-timer
        (run-with-timer 15 15 #'+my/desktop-check-server-socket))
  (+my/desktop-check-server-socket))

(provide 'emacs-server-recovery)
;;; emacs-server-recovery.el ends here
