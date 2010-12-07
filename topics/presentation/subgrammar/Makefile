test:
	../../../shared/tools/xsd2bgf ../../fl/xsd/fl.xsd fl.bgf
	echo 'Executing test cases...'
	ls -1 tests/ | xargs -n1 -I _ basename _ .baseline.bgf | xargs -n1 -I _ ./subgrammar.py fl.bgf _ fl._.bgf
	echo 'Syntax checking test results...'
	ls -1 *.bgf | xargs -n1 checkxml bgf
	echo 'Validating test results...'
	ls -1 tests/ | xargs -n1 -I _ basename _ .baseline.bgf | xargs -n1 -I _ ../../../shared/tools/gdt fl._.bgf tests/_.baseline.bgf

clean:
	rm -f *.bgf