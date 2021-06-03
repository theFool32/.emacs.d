;;; org/+screenshot.el --- -*- lexical-binding: t -*-
;;
;; Filename: +screenshot.el
;; Description: screenshot for org-mode
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2019 theFool32
;; Created: Fri Mar  6 19:52:14 2020 (+0800)
;; Last-Updated:
;;           By:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:
;; https://pengpengxp.github.io/archive/before-2018-11-10/2017-02-23-org-mode-screenshot-management.html
(defcustom peng-org-screenshot-dir-name  "img/"
  "default image directory name for org screenshot"
  :type 'string
  )

(defun peng-org-screenshot ()
  (interactive)
  "Take a screenshot into a user specified file in the current
       buffer file directory and insert a link to this file."
  (let* ((img-dir peng-org-screenshot-dir-name))
    (progn
      (if (file-exists-p img-dir)
          (print "yes")
        (mkdir img-dir))
      (let ((temp-name (read-file-name "please selete a image name"
                                 img-dir)))
        (setq filename (concat img-dir "/" (file-name-base temp-name) ".png"))
        (call-process-shell-command "screencapture" nil nil nil nil "-i" (concat
                                                                          "\"" filename "\"" ))
        (insert (concat "[[./" filename "]]"))))))

(defun peng-find-org-link-begin-and-end (plist string)
  "find link from plist whose link is equal to string, return a
  list just like `((name begin-position end-position))'"
  (let ((return-list '()))
    (progn
      (while plist
        (progn
          (if (string-equal (car (car plist))
                            string)
              (add-to-list 'return-list (cdr (car plist))))
          (setq plist (cdr plist))))
      return-list)))

(defun peng-do-delete-link-function (be-list)
  "goto the begining of link and delete it, be-list is a list
  just like `((name begin-position end-position))'"
  (while be-list
    (progn
      (goto-char (car (car be-list)))
      (delete-char (- (car (cdr (car be-list)))
                      (car (car be-list))))
      (setq be-list (cdr be-list)))))

(defun peng-delete-org-screenshot-image-file-and-link ()
  (interactive)
  (let* ((link-list (org-element-map (org-element-parse-buffer) 'link
                                     (lambda (link)
                                       (when (string= (org-element-property :type link) "file")
                                         (list (org-element-property :path link)
                                               (org-element-property :begin link)
                                               (org-element-property :end link))))))
         (img-dir peng-org-screenshot-dir-name)
         (temp-name (read-file-name "please selete a image name you want to delete"
                                      img-dir))
         (begin-end-list (peng-find-org-link-begin-and-end link-list temp-name)))
    (progn
      (if (yes-or-no-p "Do you really want to delete the image file? This can't be revert!!")
          (delete-file temp-name))
      (if (yes-or-no-p "Do you also want to delete the image links?")
          (peng-do-delete-link-function begin-end-list)))))
;;; capture ends

(provide 'org/+screenshot)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +screenshot.el ends here
