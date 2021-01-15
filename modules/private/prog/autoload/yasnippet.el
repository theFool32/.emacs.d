;;; private/prog/autoload/yasnippet.el -*- lexical-binding: t; -*-

;;;###autoload
(defun smarter-yas-expand-next-field-complete ()
  "Try to `yas-expand' and `yas-next-field' at current cursor position.
If failed try to complete the common part with `company-complete-common'"
  (interactive)
  (if yas-minor-mode
      (let ((old-point (point))
            (old-tick (buffer-chars-modified-tick)))
        (yas-expand)
        (when (and (eq old-point (point))
                   (eq old-tick (buffer-chars-modified-tick)))
          (ignore-errors (yas-next-field))
          (when (and (eq old-point (point))
                     (eq old-tick (buffer-chars-modified-tick)))
            (progn
              (call-interactively 'company-abort)
              (call-interactively 'company-yasnippet))
            )
          ))
    ;; FIXME: c-k tab c-k
    (company-complete-common)
    )
  )
