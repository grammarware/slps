fl = ..
xsd = ${fl}/xsd/fl.xsd
dcg = ${fl}/prolog/Parser.pro
java1base = ${fl}/java1
java1pkg = types
java3base = ${fl}/java3
java3pkg = fl
antlr = ${fl}/java1/FL.g
bgf = ../../../shared/xsd/bgf.xsd
validator = ../../../download/msv/msv.jar
tools = ../../../shared/tools

build:

test:
	make extract
	make check1
	make derive
	make unerase
	make abstract
	make check2
	make diff

check: check1 check2

check1:
	ls bgf/extract/*.bgf | xargs java -jar ${validator} ${bgf}

check2:
	ls bgf/derive/*.bgf | xargs java -jar ${validator} ${bgf}

extract:
	make xsd.extract
	make java.extract
	make antlr.extract
	make dcg.extract

xsd.extract:
	${tools}/xsd2bgf ${xsd} bgf/extract/xsd.bgf

java.extract:
	${tools}/java2bgf ${java1base} ${java1pkg} bgf/extract/java1.bgf
	${tools}/java2bgf ${java3base} ${java3pkg} bgf/extract/java3.bgf

antlr.extract:
	${tools}/antlr2bgf ${antlr} bgf/extract/antlr.bgf

dcg.extract:
	${tools}/dcg2bgf ${dcg} bgf/extract/dcg.bgf

derive:
	make xsd.derive
	make java1.derive
	make java3.derive
	make dcg.derive
	make antlr.derive

%.derive:
	${tools}/xbgf bgf/extract/$*.bgf xbgf/$*.xbgf bgf/derive/$*.bgf


unerase:
	${tools}/xbgf \
		bgf/derive/java1.bgf \
		xbgf/unerase.xbgf \
		bgf/derive/java.bgf

abstract:
	${tools}/xbgf \
		bgf/derive/xsd.bgf \
		xbgf/abstract1.xbgf \
		bgf/derive/abstract1.bgf
	${tools}/xbgf \
		bgf/derive/dcg.bgf \
		xbgf/abstract2.xbgf \
		bgf/derive/abstract2.bgf

diff:
	${tools}/bdt bgf/derive/java1.bgf bgf/derive/java3.bgf 
	${tools}/bdt bgf/derive/xsd.bgf bgf/derive/java.bgf
	${tools}/bdt bgf/derive/dcg.bgf bgf/derive/antlr.bgf
	${tools}/bdt bgf/derive/abstract1.bgf bgf/derive/abstract2.bgf

clean:

clean:
	rm -f *~
	rm -f bgf/extract/*.bgf
	rm -f bgf/derive/*.bgf
	rm -rf bin
	rm -rf obj
	rm -rf *.user
	rm -rf *.suo
