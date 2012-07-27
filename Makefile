build:

rebuild:
	make clean
	make build

rebuild-xbgf:
	cd topics/transformation/xbgf; make rebuild

test:
	cd topics/transformation/xbgf/tests && make testfull
	cd topics/convergence/xbgf/ebnf && make test
	cd topics/convergence/xbgf/fl && make test
	cd topics/convergence/xbgf/java && make test
	cd topics/convergence/exbgf/java && make test
	cd topics/convergence/sliced/fl && make test
	cd topics/convergence/sliced/java && make test
	cd topics/grammars && make diff

clean:
	rm -f *~
