build:
	cd topics/transformation/xbgf; make build

rebuild:
	make clean
	make build

test:
	make build
	cd topics/transformation/xbgf; make test

clean:
	rm -f *~
	cd topics/transformation/xbgf; make clean
