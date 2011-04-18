all:

upload:
	scp index.html rlaemmel,slps@web.sourceforge.net:htdocs

sync:
	rsync -avz --exclude=".svn" --exclude="Makefile" --exclude="_dev" * sspider,slps@web.sourceforge.net:htdocs

clean:
	rm -f *~ zoo/*/*

build:
	make zooprj
	make tmprj

zooprj:
	make -f _dev/Makefile.c.zoo
	make -f _dev/Makefile.cpp.zoo
	make -f _dev/Makefile.csharp.zoo
	make -f _dev/Makefile.java.zoo
	make -f _dev/Makefile.xpath.zoo
	xsltproc --stringparam date `date +"%d/%m/%Y"` _dev/zoo2xhtml.xslt _dev/config.zoo > zoo/index.html
	xsltproc _dev/links2html.xslt _dev/java-grammars.xml | python ../topics/export/hypertext/closemeta.py > zoo/java/links.html

tmprj:
	ls -1 ../topics/testing/gbtf/tests/java/*.bgf   | xargs -n1 _dev/conv java
	ls -1 ../topics/testing/gbtf/tests/tescol/*.bgf | xargs -n1 _dev/conv tescol
	@#xsltproc ../shared/xsl/links2html.xslt _dev/testmatch.xml | python ../shared/python/closemeta.py > testmatch/index.html

test:
	xmllint --noout --schema ../shared/xsd/links.xsd _dev/*.xml
