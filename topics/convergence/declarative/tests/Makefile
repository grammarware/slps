test:
	ls -1 *.mbgf | xargs -n1 ./testperform

quiet:
	ls -1 *.mbgf | xargs -n1 ./quiettest

build:

rebuild:
	make clean
	make build

%.test:
	./testperform $*

%.quiet:
	./quiettest $*

clean:
	rm -f *.out
	rm -f *.log
	rm -f *~
