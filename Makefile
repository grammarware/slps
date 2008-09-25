build:

rebuild:
	make clean
	make build

rebuild-xbgf:
	cd topics/transformation/xbgf; make rebuild

test:
	make build

clean:
	rm -f *~
