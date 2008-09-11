tools = ../../../../shared/tools

build:

test:
	make remove.test

%.test: 
	${tools}/checkxml bgf $*.bgf
	${tools}/checkxml xbgf $*.xbgf
	${tools}/xbgf $*.xbgf $*.bgf $*.out
	${tools}/gdt $*.out $*.baseline

clean:
	rm -f *.out
	rm -f *~
