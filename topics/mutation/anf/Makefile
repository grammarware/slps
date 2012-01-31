test:
	cp ../../convergence/fl/snapshot/*.bgf tests/
	rsc2bgf ../../fl/rascal1/FL.rsc tests/rascal.bgf
	ls -1 tests/*.bgf | xargs -n1 ./testperform
	rm normal/*.almost.bgf

clean:
	rm -f tests/*.bgf xbgf/*.xbgf normal/*
