all:

build:
	../../shared/tools/spec2bgf ../../topics/documents/xpath/xpath1.xml xpath1.bgf
	xsltproc --stringparam grammar `pwd`'/xpath1.bgf' ../../topics/extraction/w3c/spec2ldf.xslt ../../topics/documents/xpath/xpath1.xml > xpath1.ldf

test:
	../../shared/tools/validate bgf xpath1.bgf
	../../shared/tools/validate ldf xpath1.ldf
