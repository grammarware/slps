build:
	xmllint --noout --schema related.xsd related.xml
	xsltproc links2html.xslt related.xml > related.html

clean:
	rm -f related.html

