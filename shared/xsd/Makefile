validator = /usr/bin/xmllint --noout --schema
mapper = ../tools/xsd2bgf

build:

test:
	make testbgf
	make testxbgf
	make testbtf
	make testxbtf
	#make testldf
	make testxldf
	make testlcf
	#make testedd
	#make testrgf
	make check

testbgf:
	${validator} bgf.xsd ../../topics/fl/lci/snapshot/*.bgf

testxbgf:
	${validator} xbgf.xsd ../../topics/fl/lci/xbgf/*.xbgf

testbtf:
	${validator} btf.xsd ../../topics/fl/lci/xmlsamples/*.btf

testxbtf:

testlcf:
	${validator} lcf.xsd ../../topics/*/*/*.lcf

testldf:
	${validator} ldf.xsd ../../topics/languedoc/*/*.ldf

testxldf:
	${validator} xldf.xsd ../../topics/languedoc/*/xldf/*.xldf

testedd:
	${validator} edd.xsd ../edd/*.edd

check:
	ls -1 *.xsd | xargs -n1 -I _ ${mapper} _ tmp.bgf

clean:
	rm -rf bin obj
	rm -f *.user
	rm -f *.suo

