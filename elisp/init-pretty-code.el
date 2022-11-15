;;; init-pretty-code.el ---

;; PreSym
;; (global-prettify-symbols-mode 1)
;; -PreSym

;;;###autoload
(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")


(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.
  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.
This function accepts one special property:
  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.
For example, the rule for emacs-lisp-mode is very simple:
  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")
This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.
Pretty symbols can be unset for emacs-lisp-mode with:
  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (doom-enlist modes))
        (assq-delete-all mode +pretty-code-symbols-alist))
    (let (results)
      (while plist
        (let ((key (pop plist)))
          (if (eq key :alist)
              (prependq! results (pop plist))
            (when-let (char (plist-get +pretty-code-symbols key))
              (push (cons (pop plist) char) results)))))
      (dolist (mode (doom-enlist modes))
        (setf (alist-get mode +pretty-code-symbols-alist)
              (if-let (old-results (alist-get mode +pretty-code-symbols-alist))
                  (dolist (cell results old-results)
                    (setf (alist-get (car cell) old-results) (cdr cell)))
                results))))))

(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :<=  8804
    :>=  8805
    :tuple         "⨂"
    :dot           "•"
    :checkbox      "□"
    :pending       "■"
    :checkedbox    "☑"
    :list_property "∷"
    :ellipses      "…"
    :arrow_right   "→"
    :arrow_left    "←"
    :title         "❤"
    :subtitle      "𝙩"
    :author        "✍"
    :date          "⚓"
    :property      "☸"
    :options       "⌥"
    :latex_class   "🄲"
    :latex_header  "⇥"
    :beamer_header "↠"
    :attr_latex    "🄛"
    :attr_html     "🄗"
    :begin_quote   "❮"
    :end_quote     "❯"
    :caption       "☰"
    :header        "›"
    :results       "🍌"
    :begin_export  "⏩"
    :end_export    "⏪"
    :properties    "⚙"
    :end           "∎"
    :priority_a   "🄰"
    :priority_b   "🄱"
    :priority_c   "🄲"
    :priority_d   "🄳"
    ;; :priority_c   "🅲"
    )
  "Options plist for `set-pretty-symbols!'.
This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defun +pretty-code--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.
This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

(defvar +pretty-code-enabled-modes '(org-mode)
  "List of major modes in which `prettify-symbols-mode' should be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun +pretty-code-init-pretty-symbols-h ()
  "Enable `prettify-symbols-mode'.
If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.
Otherwise it builds `prettify-code-symbols-alist' according to
`+pretty-code-symbols-alist' for the current major-mode."
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (when (or (eq +pretty-code-enabled-modes t)
              (if (eq (car +pretty-code-enabled-modes) 'not)
                  (not (memq major-mode (cdr +pretty-code-enabled-modes)))
                (memq major-mode +pretty-code-enabled-modes)))
      (setq prettify-symbols-alist
            (append (cdr (assq major-mode +pretty-code-symbols-alist))
                    (default-value 'prettify-symbols-alist)))
      (when prettify-symbols-mode
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))

(add-hook 'after-change-major-mode-hook #'+pretty-code-init-pretty-symbols-h)

;; for Python-mode
;; (set-pretty-symbols! 'python-mode
;;   :>= ">="
;;   :<= "<="
;;   ;; Functional
;;   :def "def"
;;   :lambda "lambda"
;;   ;; Types
;;   :null "None"
;;   :true "True"
;;   :false "False"
;;   :int "int"
;;   :str "str"
;;   :float "float"
;;   :bool "bool"
;;   :tuple "tuple"
;;   ;; Flow
;;   :not "not"
;;   :in "in" :not-in "not in"
;;   :and "and" :or "or"
;;   :for "for"
;;   :return "return" :yield "yield")

(set-pretty-symbols! 'emacs-lisp-mode
  :lambda "lambda")

(set-pretty-symbols! 'org-mode
  :name "#+NAME:"
  :src_block "#+begin_src"
  :src_block_end "#+end_src"
  :src_block "#+begin_latex"
  :src_block_end "#+end_latex"
  ;; :checkbox      "[ ]"
  ;; :pending       "[-]"
  ;; :checkedbox    "[X]"
  :list_property "::"
  :em_dash       "---"
  :ellipsis      "..."
  :arrow_right   "->"
  :arrow_left    "<-"
  :title         "#+title:"
  :subtitle      "#+subtitle:"
  :author        "#+author:"
  :date          "#+date:"
  :property      "#+property:"
  :options       "#+options:"
  :latex_class   "#+latex_class:"
  :latex_header  "#+latex_header:"
  :beamer_header "#+beamer_header:"
  :attr_latex    "#+attr_latex:"
  :attr_html     "#+attr_latex:"
  :begin_quote   "#+begin_quote"
  :end_quote     "#+end_quote"
  :caption       "#+caption:"
  :header        "#+header:"
  :begin_export  "#+begin_export"
  :end_export    "#+end_export"
  :results       "#+RESULTS:"
  :property      ":PROPERTIES:"
  :end           ":END:"
  ;; :priority_a    "[#A]"
  ;; :priority_b    "[#B]"
  ;; :priority_c    "[#C]"
  ;; :priority_d    "[#D]"
  )

(provide 'init-pretty-code)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-pretty-code.el ends here
