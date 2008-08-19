source = ../../fl/prolog1/Parser.pro
target = tests/fl

build:

test:
	make clean-tests
	../../../shared/tools/dcg2bgf ${source} ${target}.bgf
	../../../shared/tools/showbgf ${target}.bgf > ${target}.out
	diff ${target}.out ${target}.baseline

clean:
	make clean-tests
	rm -f *~

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
