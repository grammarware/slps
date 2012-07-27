build:

rebuild:
	make clean
	make build

rebuild-xbgf:
	cd topics/transformation/xbgf; make rebuild

test:
	cd topics/transformation/xbgf/tests && make testfull

clean:
	rm -f *~
