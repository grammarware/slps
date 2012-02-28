test:
	make clean
	cp ../../convergence/fl/snapshot/*.bgf tests/
	ls -1 tests/*.bgf | xargs -n1 ./testperform
	rm *.xbgf

show:
	ls -1 tests/*.ready.bgf | xargs -n1 ../../../shared/tools/showrootprods

clean:
	rm -f *.xbgf tests/*.xbgf tests/*.ready.* tests/*.root.*
