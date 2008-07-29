fl = ../../fl/shared/fl.xsd
bgf = ../../../shared/xsd/bgf.xsd
xbgf = ../../../shared/xsd/xbgf.xsd

build:

test:
	make clean-tests
# Test fl sample
	../../../shared/tools/xsd2bgf ${fl} tests/fl.bgf
	../../../shared/tools/showbgf tests/fl.bgf > tests/fl.out
	diff tests/fl.out tests/fl.baseline
# Test bgf
	../../../shared/tools/xsd2bgf ${bgf} tests/bgf.bgf
	../../../shared/tools/showbgf tests/bgf.bgf > tests/bgf.out
	diff tests/bgf.out tests/bgf.baseline
# Test xbgf
	../../../shared/tools/xsd2bgf ${xbgf} tests/xbgf.bgf
	../../../shared/tools/showbgf tests/xbgf.bgf > tests/xbgf.out
	diff tests/xbgf.out tests/xbgf.baseline

clean:
	make clean-tests
	rm -f *~

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
