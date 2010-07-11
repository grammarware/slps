all:

install: install-antlr install-fop install-msv install-et install-pp install-generateDS install-gdk

install-xig:
	#curl -k http://labsewiki.isti.cnr.it/data/media/projects/taxi/taxi.exe
	curl -k http://downloads.sourceforge.net/project/xml-xig/xml-xig/xml-xig%200.1/xml-xig-0.1.1.jar?use_mirror=citylan&ts=1278861268 >xml-xig-0.1.1.jar

install-gdk:
	curl -k http://gdk.sourceforge.net/gdk-1.4.4.tar.gz > gdk-1.4.4.tar.gz
	gzip -d gdk-1.4.4.tar.gz
	tar -xf gdk-1.4.4.tar
	cd gdk-1.4.4 && ./configure
	cd gdk-1.4.4 && make
	cd gdk-1.4.4 && make check
	cd gdk-1.4.4 && sudo make install

install-jastadd:
	curl -k http://builds.jastadd.org/JastAdd/2009-06-10/jastadd2.jar > jastadd2.jar

install-antlr:
	curl -k http://www.antlr.org/download/antlrworks-1.1.7.jar > antlrworks-1.1.7.jar

install-et:
	curl -k http://effbot.org/media/downloads/elementtree-1.2.7-20070827-preview.zip > elementtree.zip
	unzip elementtree.zip
	rm -f elementtree.zip
	sudo sh -c 'cd elementtree-1.2.7-20070827-preview; python setup.py install; rm -rf build'
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

install-generateDS:
	svn co http://codespeak.net/svn/lxml/trunk lxml
	python setup.py build --static-deps	
	curl -k http://www.rexx.com/~dkuhlman/generateDS-1.17b.tar.gz > generateDS-1.17b.tar.gz
	tar xzvf generateDS-1.17b.tar.gz
	easy_install lxml
	cd generateDS-1.17b ; python setup.py build
	cd generateDS-1.17b ; python setup.py install

clean-temps:
	rm -rf fop.tar msv.zip generateDS-1.17b.tar* msv-*
	rm -fr elementtree-1.2.7-20070827-preview
	rm -rf pyparsing-1.5.0

clean:
	rm -f *~
	make clean-temps
	rm -f antlrworks-1.1.7.jar
	rm -rf fop
	rm -rf msv

