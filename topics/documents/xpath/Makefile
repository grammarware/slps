bgf:
	/usr/local/bin/xsltproc spec2bgf.xslt xpath-pp.xml > xpath1.bgf
	perl -pi -w -e 's/<unknown>\)\*<\/unknown>/<unknown>\)<\/unknown><postfix-star\/>/g;' xpath1.bgf
	perl -pi -w -e 's/<unknown>\)\?<\/unknown>/<unknown>\)<\/unknown><postfix-optional\/>/g;' xpath1.bgf
	perl -pi -w -e 's/<unknown>\(<\/unknown>/<sequence><bgf:expression>/g;' xpath1.bgf
	perl -pi -w -e 's/<unknown>\)<\/unknown>/<\/bgf:expression><\/sequence>/g;' xpath1.bgf
	/usr/local/bin/xsltproc postfix2confix.xslt xpath1.bgf > xpath1a.bgf
	/usr/local/bin/xsltproc ununknow.xslt xpath1a.bgf > xpath1b.bgf

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
