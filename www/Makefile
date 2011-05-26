all:

upload:
	scp index.html rlaemmel,slps@web.sourceforge.net:htdocs

sync:
	rm -f tmp.xml
	rsync -avz --delete --exclude=".svn" --exclude="Makefile" --exclude="_dev" * sspider,slps@web.sourceforge.net:htdocs

clean:
	rm -f *~ zoo/*/*

build:
	make zooprj
	make tankprj
	make tmprj
	xsltproc _dev/links2html.xslt _dev/java-grammars.xml | python ../topics/export/hypertext/closemeta.py > zoo/java/links.html

zooprj:
	rm -f zoo/*/*
	xsltproc _dev/list2makefile.xslt _dev/zoo.xml > _dev/Makefile.x
	make -f _dev/Makefile.x
	xsltproc --stringparam date `date +"%d/%m/%Y"` _dev/list2xhtml.xslt _dev/zoo.xml  >  zoo/index.html

tankprj:
	rm -f tank/*/*
	cp ../topics/convergence/fl/snapshot/*.bgf tank/fl/
	cp ../topics/testing/gbtf/tests/tescol/*.bgf tank/tescol/
	chmod 644 tank/*/*.bgf
	xsltproc _dev/list2makefile.xslt _dev/tank.xml | grep -v 'add tank fl' | grep -v 'add tank tescol' > _dev/Makefile.y
	make -f _dev/Makefile.y
	xsltproc --stringparam date `date +"%d/%m/%Y"` _dev/list2xhtml.xslt _dev/tank.xml > tank/index.html

tmprj:
	ls -1 ../topics/testing/gbtf/tests/java/*.bgf   | xargs -n1 _dev/conv java
	ls -1 ../topics/testing/gbtf/tests/tescol/*.bgf | xargs -n1 _dev/conv tescol
	@#xsltproc ../shared/xsl/links2html.xslt _dev/testmatch.xml | python ../shared/python/closemeta.py > testmatch/index.html

test:
	xmllint --noout --schema ../shared/xsd/links.xsd _dev/java-grammars.xml
