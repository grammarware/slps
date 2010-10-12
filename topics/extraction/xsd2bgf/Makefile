fl = ../../fl/xsd/fl.xsd
bgf = ../../../shared/xsd/bgf.xsd
xbgf = ../../../shared/xsd/xbgf.xsd
xsd2bgf = ../../../shared/tools/xsd2bgf
showbgf = ../../../shared/tools/showbgf
validate = ../../../shared/tools/validate

build:

test:
	make clean-tests
# Test fl sample
	${xsd2bgf} ${fl} tests/fl.bgf
	${validate} bgf tests/fl.bgf
	${showbgf} tests/fl.bgf > tests/fl.out
	diff tests/fl.out tests/fl.baseline
# Test bgf
	${xsd2bgf} ${bgf} tests/bgf.bgf
	${validate} bgf tests/bgf.bgf
	${showbgf} tests/bgf.bgf > tests/bgf.out
	diff tests/bgf.out tests/bgf.baseline
# Test xbgf
	${xsd2bgf} ${xbgf} tests/xbgf.bgf
	${validate} bgf tests/xbgf.bgf
	${showbgf} tests/xbgf.bgf > tests/xbgf.out
	diff tests/xbgf.out tests/xbgf.baseline

clean:
	make clean-tests
	rm -f *~

clean-tests:
	rm -f tests/*.bgf
	rm -f tests/*.out
