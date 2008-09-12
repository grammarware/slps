tools = ../../../../shared/tools

build:

test:
	make inject.test
	make deyaccify.test
	make doubles.test
	make extract.test
	make extract2.test
	make remove.test

rebuild:
	make clean
	make test

%.test:
	${tools}/checkxml bgf $*.bgf
	${tools}/checkxml xbgf $*.xbgf
	${tools}/xbgf $*.xbgf $*.bgf $*.out
	${tools}/gdt $*.out $*.baseline

clean:
	rm -f *.out
	rm -f *~
