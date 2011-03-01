test:
	cp ../../convergence/fl/snapshot/*.bgf tests/
	cp ../../convergence/java/snapshot/*.bgf tests/
	ls -1 tests/*.bgf | xargs -n1 ./testperform

clean:
	rm -f tests/*.bgf tests/*.out