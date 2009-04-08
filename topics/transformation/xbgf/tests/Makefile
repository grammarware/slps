build:
	@cd ../apps/xbgf; make build

test:
	ls -1 *.xbgf | xargs -n1 ./testperform

testfull:
	make build
	ls -1 *.xbgf | xargs -n1 ./testthorough

complete:
	@ls -1 *.xbgf | xargs -n1 ./quiettest

rebuild:
	make clean
	@cd ../apps/xbgf; make rebuild

%.test:
	./testperform $*

%.quiet:
	./testthorough $*

clean:
	rm -f *.out
	rm -f *.log
	rm -f *~
