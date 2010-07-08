validator = ../../../shared/tools/checkxml

build:
	time make slice
	time make test
	time make csv
	
slice:
	../../../shared/python/sliceXbgfs.py ../fl/fl.lcf .

test:
	../../../shared/tools/lci fl.lcf architecture
	
csv:
	../../../shared/python/lcf2csv.py fl.lcf ../../../shared/tools/gdt csv

check:
	${validator} lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *~
	rm -f bgf/* xbgf/* *.txt TMP-res
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f fl.log

