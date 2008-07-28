install: install-fop install-msv install-et install-pp

install-et:
	curl -k http://effbot.org/media/downloads/elementtree-1.2.7-20070827-preview.zip > elementtree.zip
	unzip elementtree.zip
	rm -f elementtree.zip
	sudo sh -c 'cd ${PWD}/elementtree-1.2.7-20070827-preview; python setup.py install; rm -rf build'
	make clean-temps

install-pp:
	curl -k http://heanet.dl.sourceforge.net/sourceforge/pyparsing/pyparsing-1.5.0.tar.gz >pyparsing.tar.gz
	gunzip pyparsing.tar.gz
	tar xfv pyparsing.tar
	rm -f pyparsing.tar
	sh -c 'cd pyparsing-1.5.0 ; python setup.py install'
	make clean-temps

install-fop:
	curl -k http://apache.mirroring.de/xmlgraphics/fop/binaries/fop-0.94-bin-jdk1.3.tar.gz > fop.tar.gz
	gunzip fop.tar.gz
	tar -xvf fop.tar
	mv fop-0.94 fop
	make clean-temps

install-msv:
	curl -k https://msv.dev.java.net/files/documents/61/85936/msv.20080213.zip > msv.zip
	unzip msv.zip
	mkdir -p msv
	mv msv-20080213/*.jar msv
	make clean-temps

clean-temps:
	rm -f fop.tar
	rm -f msv.zip
	rm -rf msv-*
	rm -fr elementtree-1.2.7-20070827-preview
	rm -rf pyparsing-1.5.0

clean:
	rm -f *~
	make clean-temps
	rm -rf fop
	rm -rf msv

