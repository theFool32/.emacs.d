.PHONY: env custom init	install

install_emacs:
	@brew tap jimeh/emacs-builds
	@brew install --cask emacs-app-monthly
	@curl https://raw.githubusercontent.com/theFool32/build-emacs-macos/icon/materials/emacs-big-sur.icns -o /Applications/Emacs.App/Contents/Resources/Emacs.icns
	@touch /Applications/Emacs.app
	@sudo killall Finder && sudo killall Finder

env:
	@printenv > ~/.emacs.d/env
	@echo MU_PATH=`brew --prefix mu` >> ~/.emacs.d/env

custom:
	@test -f init-custom.el || cp init-custom-example.el init-custom.el

# init: env custom
# 	@emacs -Q -batch -eval '(progn (find-file "init.org")(org-babel-tangle))'

install:
	@brew install ripgrep fd rga terminal-notifier
