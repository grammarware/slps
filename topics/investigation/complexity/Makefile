test:
	cp ../../convergence/fl/snapshot/*.bgf tests/
	cp ../../convergence/java/snapshot/*.bgf tests/
	ls -1 tests/*.bgf | xargs -n1 ./testperform

check:
	bgf2bnf tests/antlr.bgf
	./mcc.py tests/antlr.bgf
	xbgf horizontalize.xbgf tests/antlr.bgf antlrh.bgf
	bgf2bnf antlrh.bgf
	./mcc.py antlrh.bgf
clean:
	rm -f tests/*.bgf tests/*.out