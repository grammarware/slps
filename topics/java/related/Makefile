build:
	xmllint --noout --schema related.xsd related.xml
	xsltproc links2html.xslt related.xml | python ../../presentation/ldf2pdf/closemeta.py > related.html

clean:
	rm -f related.html

