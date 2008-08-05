fl = ../../../../slps/topics/fl
xsd = ${fl}/xsd/fl.xsd
factorial = ${fl}/xsd/factorial.xml
bgf = ../../../shared/xsd/bgf.xsd
xbgf = ../../../shared/xsd/xbgf.xsd
btf = ../../../shared/xsd/btf.xsd
apps = ../../transformation/xbgf/apps
validator = ../../../download/msv/msv.jar
xsd2btf = ../../../shared/tools/xsd2btf

build:

test:
	${xsd2btf} ${xsd} ${factorial} tests/factorial.btf
	${xsd2btf} ${xbgf} ${apps}/bgf/bgf1.xbgf tests/bgf1.btf
	${xsd2btf} ${xbgf} ${apps}/bgf/bgf2.xbgf tests/bgf2.btf
	${xsd2btf} ${xbgf} ${apps}/xbgf/xbgf1.xbgf tests/xbgf1.btf
	${xsd2btf} ${xbgf} ${apps}/xbgf/xbgf2.xbgf tests/xbgf2.btf
	make check

check:
	ls tests/*.btf | xargs java -jar ${validator} ${btf}

clean:
	make clean-tests
	rm -f *~

clean-tests:
	rm -f tests/*.btf
