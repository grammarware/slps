all:

upload:
	scp index.html rlaemmel,slps@web.sourceforge.net:htdocs

sync:
	rsync -avz --exclude=".svn" --exclude="Makefile" --exclude="_dev" * sspider,slps@web.sourceforge.net:htdocs

clean:
	rm -f *~ zoo/*/*

build:
	make -f _dev/Makefile.c.zoo
	make -f _dev/Makefile.cpp.zoo
	make -f _dev/Makefile.csharp.zoo
	make -f _dev/Makefile.java.zoo
	xsltproc --stringparam date `date +"%d/%m/%Y"` ../shared/xsl/zoo2xhtml.xslt _dev/config.zoo > zoo/index.html
	xsltproc ../shared/xsl/links2html.xslt _dev/java-grammars.xml | python ../shared/python/closemeta.py > zoo/java/links.html

test:
	xmllint --noout --schema ../shared/xsd/links.xsd _dev/java-grammars.xml
