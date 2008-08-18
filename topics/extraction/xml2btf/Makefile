fl = ../../../../slps/topics/fl
xsd = ${fl}/xsd/fl.xsd
factorial = ${fl}/xsd/factorial.xml
bgf = ../../../shared/xsd/bgf.xsd
xbgf = ../../../shared/xsd/xbgf.xsd
btf = ../../../shared/xsd/btf.xsd
apps = ../../transformation/xbgf/apps
validator = ../../../download/msv/msv.jar
xml2btf = ../../../shared/tools/xml2btf

build:

test:
	${xml2btf} ${xsd} ${factorial} tests/factorial.btf
	${xml2btf} ${xbgf} ${apps}/bgf/append.xbgf tests/xbgf1.btf
	${xml2btf} ${xbgf} ${apps}/xbgf/append.xbgf tests/xbgf2.btf
	make check

check:
	ls tests/*.btf | xargs java -jar ${validator} ${btf}

clean:
	make clean-tests
	rm -f *~

clean-tests:
	rm -f tests/*.btf
