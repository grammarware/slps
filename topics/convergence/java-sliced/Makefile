validator = ../../../shared/tools/validate

build:
	time make slice
	time make test
	time make csv
	
slice:
	../../../shared/python/sliceXbgfs.py ../java/java.lcf .

test:
	../../../shared/tools/lci java.lcf architecture

csv:
	../../../shared/python/lcf2csv.py java.lcf ../../../shared/tools/gdt csv

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *~
	rm -f bgf/* xbgf/* *.txt
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log
