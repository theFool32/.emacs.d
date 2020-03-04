;;; init-magit.el --- -*- lexical-binding: t -*-
;;
;; Filename: init-magit.el
;; Description: Initialize Magit
;; Author: Mingde (Matthew) Zeng
;; Copyright (C) 2019 Mingde (Matthew) Zeng
;; Created: Fri Mar 15 08:40:27 2019 (-0400)
;; Version: 2.0.0
;; Last-Updated: Sat Feb 22 10:56:35 2020 (+0800)
;;           By: Mingde (Matthew) Zeng
;; URL: https://github.com/MatthewZMD/.emacs.d
;; Keywords: M-EMACS .emacs.d magit
;; Compatibility: emacs-version >= 26.1
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; This initializes magit
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

;; MagitPac
(use-package magit
  :if *git*
  :bind ("C-x g" . magit-status)
  :config
  (global-auto-revert-mode -1)
  (magit-auto-revert-mode -1)
  )
;; -MagitPac

(defvar gitmoji--all-emoji
  '(("Improving structure / format of the code." . ":art:")
    ("Improving performance." . ":zap:")
    ("Removing code or files." . ":fire:")
    ("Fixing a bug." . ":bug:")
    ("Critical hotfix." . ":ambulance:")
    ("Introducing new features." . ":sparkles:")
    ("Writing docs." . ":memo:")
    ("Deploying stuff." . ":rocket:")
    ("Updating the UI and style files." . ":lipstick:")
    ("Initial commit." . ":tada:")
    ("Updating tests." . ":white_check_mark:")
    ("Fixing security issues." . ":lock:")
    ("Fixing something on macOS." . ":apple:")
    ("Fixing something on Linux." . ":penguin:")
    ("Fixing something on Windows." . ":checkered_flag:")
    ("Fixing something on Android." . ":robot:")
    ("Fixing something on iOS." . ":green_apple:")
    ("Releasing / Version tags." . ":bookmark:")
    ("Removing linter warnings." . ":rotating_light:")
    ("Work in progress." . ":construction:")
    ("Fixing CI Build." . ":green_heart:")
    ("Downgrading dependencies." . ":arrow_down:")
    ("Upgrading dependencies." . ":arrow_up:")
    ("Pinning dependencies to specific versions." . ":pushpin:")
    ("Adding CI build system." . ":construction_worker:")
    ("Adding analytics or tracking code." . ":chart_with_upwards_trend:")
    ("Refactoring code." . ":recycle:")
    ("Work about Docker." . ":whale:")
    ("Adding a dependency." . ":heavy_plus_sign:")
    ("Removing a dependency." . ":heavy_minus_sign:")
    ("Changing configuration files." . ":wrench:")
    ("Internationalization and localization." . ":globe_with_meridians:")
    ("Fixing typos." . ":pencil2:")
    ("Writing bad code that needs to be improved." . ":hankey:")
    ("Reverting changes." . ":rewind:")
    ("Merging branches." . ":twisted_rightwards_arrows:")
    ("Updating compiled files or packages." . ":package:")
    ("Updating code due to external API changes." . ":alien:")
    ("Moving or renaming files." . ":truck:")
    ("Adding or updating license." . ":page_facing_up:")
    ("Introducing breaking changes." . ":boom:")
    ("Adding or updating assets." . ":bento:")
    ("Updating code due to code review changes." . ":ok_hand:")
    ("Improving accessibility." . ":wheelchair:")
    ("Documenting source code." . ":bulb:")
    ("Writing code drunkenly." . ":beers:")
    ("Updating text and literals." . ":speech_balloon:")
    ("Performing database related changes." . ":card_file_box:")
    ("Adding logs." . ":loud_sound:")
    ("Removing logs." . ":mute:")
    ("Adding contributor(s)." . ":busts_in_silhouette:")
    ("Improving user experience / usability." . ":children_crossing:")
    ("Making architectural changes." . ":building_construction:")
    ("Working on responsive design." . ":iphone:")
    ("Mocking things." . ":clown_face:")
    ("Adding an easter egg." . ":egg:")
    ("Adding or updating a .gitignore file" . ":see_no_evil:")
    ("Adding or updating snapshots" . ":camera_flash:")
    ("Experimenting new things" . ":alembic:")
    ("Improving SEO" . ":mag:")
    ("Work about Kubernetes" . ":wheel_of_dharma:")
    ("Adding or updating types (Flow, TypeScript)" . ":label:")))

(defun gitmoji-picker ()
  "Choose a gitmoji."
  (interactive)
  (ivy-read "Choose a gitmoji: "
            (mapcar (lambda (x)
                      (cons
                       (concat (cdr x) " — " (car x))
                       x))
                    gitmoji--all-emoji)
            :action (lambda (x)
                      (insert (cdr (cdr x)))
                      (insert " "))))

(use-package magit-todos)

(require 'init-evil)
(use-package evil-magit
  :if (executable-find "git"))

;; Walk through git revisions of a file
(use-package git-timemachine
  :custom-face
  (git-timemachine-minibuffer-author-face ((t (:inherit success))))
  (git-timemachine-minibuffer-detail-face ((t (:inherit warning))))
  :bind (:map vc-prefix-map
              ("t" . git-timemachine))
  :hook (before-revert . (lambda ()
                           (when (bound-and-true-p git-timemachine-mode)
                             (user-error "Cannot revert the timemachine buffer")))))


(provide 'init-magit)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-magit.el ends here
