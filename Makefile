build:
	cd topics/transformation/xbgf; make build

rebuild:
	make clean
	make build

test:
	make build

clean:
	rm -f *~
	cd topics/transformation/xbgf; make clean
