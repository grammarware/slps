source = ../../fl/txl/FL.Txl
target = tests/fl

test:
	make clean
	../../../shared/tools/txl2bgf ${source} ${target}.bgf
	../../../shared/tools/bgf2bnf ${target}.bgf ${target}.out
	diff ${target}.out ${target}.baseline

clean:
	rm -f tests/*.bgf tests/*.out
