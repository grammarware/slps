build:
	/usr/local/bin/xsltproc spec2ldf.xslt xpath-pp.xml > xpath1.ldf

get:
	curl -k http://www.w3.org/TR/1999/REC-xpath-19991116.xml >xpath1.xml
	curl -k http://www.w3.org/TR/2007/REC-xpath20-20070123/xpath20.xml >xpath2.xml
	curl -k http://www.w3.org/XML/1998/06/xmlspec-v20.dtd > xmlspec-v20.dtd
	
test:
	../../../shared/tools/checkxml ldf xpath1.ldf

clean:
	*~
