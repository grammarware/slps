test:
	../../../shared/tools/xsd2bgf ../../fl/xsd/fl.xsd fl.bgf
	ls -1 tests/ | xargs -n1 -I _ basename _ .baseline.bgf | xargs -n1 ./testperform

clean:
	rm -f *.bgf