env:
	@printenv > ~/.emacs.d/env
	@echo MU_PATH=`brew --prefix mu` >> ~/.emacs.d/env

custom:
	@test -f init-custom1.el || echo "exist"

init: env custom
	@emacs -Q -batch -eval '(progn (find-file "init.org")(org-babel-tangle))'
