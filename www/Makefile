all:

upload:
	scp index.html rlaemmel,slps@web.sourceforge.net:htdocs

sync:
	rsync -avz --exclude=".svn" --exclude="Makefile" --exclude="*.zoo" * sspider,slps@web.sourceforge.net:htdocs

clean:
	rm -f *~ zoo/*/*

build:
	make -f Makefile.c.zoo
	make -f Makefile.cpp.zoo
	make -f Makefile.csharp.zoo
	make -f Makefile.java.zoo
	xsltproc ../shared/xsl/zoo2xhtml.xslt config.zoo > zoo/index.html
