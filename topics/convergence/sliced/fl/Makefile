validator = ../../../../shared/tools/validate

build:
	time make slice
	time make test
	time make csv

slice:
	../../../../shared/python/sliceXbgfs.py ../../xbgf/fl/fl.lcf .

test:
	../../../../shared/tools/lci fl.lcf architecture

csv:
	../../../../shared/python/lcf2csv.py fl.lcf ../../../../shared/tools/gdt csv

check:
	${validator} lcf fl.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

clean:
	rm -f *~ *.user *.suo *.log *.txt TMP-res architecture* bgf/*
	rm -rf bin obj
