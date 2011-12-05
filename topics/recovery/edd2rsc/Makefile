all:

test:
	xsltproc ../../../shared/xsl/edd-export.xslt ../../../shared/edd/lll1.edd > lll1.txt.edd
	java -jar ../../../download/rascal.jar Transformer.rsc lll1.txt.edd LLL1
	xsltproc ../../../shared/xsl/edd-export.xslt ../../../shared/edd/lll2.edd > lll2.txt.edd
	java -jar ../../../download/rascal.jar Transformer.rsc lll2.txt.edd LLL2

rsc:
	java -jar ../../../download/rascal.jar

clean:
	rm -f *~ *.txt.edd LLL?.rsc