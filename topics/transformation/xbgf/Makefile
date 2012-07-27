build:
	cd tests && make build
	cd apps && make build
	cd ../../convergence/xbgf/fl && make build

rebuild:
	make clean
	make build

test:
	cd apps && make test
	cd tests && make test
	cd ../../convergence/xbgf/fl && make test

clean:
	rm -f *~
	cd tests && make clean
	cd apps && make clean
	cd ../../convergence/xbgf/fl && make clean

