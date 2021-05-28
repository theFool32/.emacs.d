;;; init-tree-sitter.el ---
;;
;; Filename: init-tree-sitter.el
;; Description:
;; Author: theFool32
;; Maintainer:
;; Copyright (C) 2020 theFool32
;; Created: Thu May  6 17:00:53 2021 (+0800)
;; Version:
;; Package-Requires: ()
;; Last-Updated:
;;           By:
;;     Update #: 9
;; URL:
;; Doc URL:
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Change Log:
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

(eval-when-compile
  (require 'init-const))

;; https://github.com/ubolonton/emacs-tree-sitter/issues/88#issuecomment-849338234
(use-package tsc
  :straight `(:pre-build ,(when (and (memq window-system '(mac ns))
                                     (string-match-p (rx string-start "arm-")
                                                     system-configuration))
                                         ;; required for tree-sitter
                            (unless (and (executable-find "cargo")
                                         ;; required for building bindings
                                         (executable-find "cask")
                                         (executable-find "git")
                                         ;; required for tree-sitter to generate
                                         (executable-find "npm")
                                         ;; required for bindings
                                         (executable-find "llvm-gcc"))
                              (warn "tree-sitter build will fail"))
                              ;; get the latest tree-sitter
                            '(("sh" "-c" "test -d rust-tree-sitter || git clone https://github.com/tree-sitter/tree-sitter rust-tree-sitter; cd rust-tree-sitter && git pull")
                              ("sh" "-c" "cd rust-tree-sitter/cli && cargo install --path .")
                              ;; rebuild bindings
                              ("sh" "-c" "EMACS=emacs ./bin/setup && EMACS=emacs ./bin/build")
                              ;; ensure all language definitions
                              ("find" "langs/repos" "-type" "f" "-name" "grammar.js" "-not" "-path" "\\*/node_modules/\\*" "-exec" "sh" "-c" "grammar_path=\"${1%/*}\"; EMACS=emacs make \"ensure/${grammar_path##*/}\"" "sh" "{}" ";")
                              ;; needed or it will download x86_64 dylibs over the arm64 ones we just built
                              ("sh" "-c" "printf LOCAL >core/DYN-VERSION")))
                         :files ("core/DYN-VERSION" "core/tsc-dyn.*" "core/*.el")))

(provide 'init-tree-sitter)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-tree-sitter.el ends here
