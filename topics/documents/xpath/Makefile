build:
	../../../shared/tools/spec2bgf xpath1.xml xpath1.bgf
	../../../shared/tools/validate bgf xpath1.bgf
	/usr/local/bin/xsltproc --stringparam grammar `pwd`'/xpath1.bgf' ../../extraction/w3c/spec2ldf.xslt xpath1.xml > xpath1.ldf
	../../../shared/tools/validate ldf xpath1.ldf

test:
	../../../shared/tools/validate ldf xpath1.ldf
	
get:
	curl -k http://www.w3.org/TR/1999/REC-xpath-19991116.xml > xpath1.xml
	curl -k http://www.w3.org/TR/2007/REC-xpath20-20070123/xpath20.xml > xpath2.xml
	curl -k http://www.w3.org/XML/1998/06/xmlspec-v20.dtd > xmlspec-v20.dtd
	perl -pi -w -e 's/\/XML\/1998\/06\/xmlspec-v20.dtd/xmlspec-v20.dtd/g;' xpath1.xml

clean:
	rm -f *~ *.bgf
