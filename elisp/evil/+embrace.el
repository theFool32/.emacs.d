;;; evil/+embrace.el --- -*- lexical-binding: t -*-
;;
;; Filename: +embrace.el
;; Description: Evil embrace
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

;;;###autoload
(defun +evil--embrace-get-pair (char)
  (if-let* ((pair (cdr-safe (assoc (string-to-char char) evil-surround-pairs-alist))))
      pair
    (if-let* ((pair (assoc-default char embrace--pairs-list)))
        (if-let* ((real-pair (and (functionp (embrace-pair-struct-read-function pair))
                                  (funcall (embrace-pair-struct-read-function pair)))))
            real-pair
          (cons (embrace-pair-struct-left pair) (embrace-pair-struct-right pair)))
      (cons char char))))

;;;###autoload
(defun +evil--embrace-escaped ()
  "Backslash-escaped surround character support for embrace."
  (let ((char (read-char "\\")))
    (if (eq char 27)
        (cons "" "")
      (let ((pair (+evil--embrace-get-pair (string char)))
            (text (if (sp-point-in-string) "\\\\%s" "\\%s")))
        (cons (format text (car pair))
              (format text (cdr pair)))))))

;;;###autoload
(defun +evil--embrace-latex ()
  "LaTeX command support for embrace."
  (cons (format "\\%s{" (read-string "\\")) "}"))

;;;###autoload
(defun +evil--embrace-elisp-fn ()
  "Elisp function support for embrace."
  (cons (format "(%s " (or (read-string "(") "")) ")"))

;;;###autoload
(defun +evil--embrace-angle-brackets ()
  "Type/generic angle brackets."
  (cons (format "%s<" (or (read-string "") ""))
        ">"))

(provide 'evil/+embrace)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; +embrace.el ends here
