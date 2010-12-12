all:
	make clean
	make test

build:

test:
	make test-ldf2bgf
	make test-ldf2set

test-ldf2bgf:
	find ../../.. -name "*.ldf" | xargs -n1 ./testperform

test-ldf2set:
	../../../shared/tools/ldf2set ../../fl/ldf/fl.ldf samples

clean:
	rm -rf *.bgf tests/*.bgf samples/* samples.xml *.pyc
