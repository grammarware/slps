test:
	xsltproc ../../../shared/xsl/ldf2samples.xslt ../../fl/ldf/fl.ldf > samples.xml
	python xml2set.py samples.xml samples

clean:
	rm -rf samples/* samples.xml *.pyc
