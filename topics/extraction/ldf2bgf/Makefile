all:
	make clean
	make test

build:

test:
	find ../../.. -name "*.ldf" | xargs -n1 ../../../shared/tools/ldf2bgf
	ls -1 *.bgf | xargs -n1 ../../../shared/tools/validate bgf

clean:
	rm -f *.bgf
