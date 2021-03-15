;; -*- no-byte-compile: t; -*-
;;; private/rime/packages.el

;; (package! liberime
;;   :recipe (:host github :repo "DogLooksGood/liberime" :files ("CMakeLists.txt" "Makefile" "src" "*.el")))
(package! rime
  :recipe (:host github :repo "DogLooksGood/emacs-rime" :files ("*.el" "Makefile" "lib.c")))
