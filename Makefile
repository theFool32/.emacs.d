.PHONY: env custom init	install
env:
	@printenv > ~/.emacs.d/env
	@echo MU_PATH=`brew --prefix mu` >> ~/.emacs.d/env

custom:
	@test -f init-custom.el || cp init-custom-example.el init-custom.el

init: env custom
	@emacs -Q -batch -eval '(progn (find-file "init.org")(org-babel-tangle))'

install:
	@brew install ripgrep fd rga terminal-notifier
