;; -*- no-byte-compile: t; -*-
;;; private/reference/packages.el

;; (package! ebib :recipe (:repo "thefool32/ebib" :host github))
(package! ebib :recipe (:local-repo "/Users/lijie/dev/ebib" :build (:not native-compile)))
