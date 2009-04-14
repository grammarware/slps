version = ${shell cat VERSION}
distdir = grk-${version}
cvsroot = flits.cs.vu.nl:/home/ralf/cvsroot
project = cvsprojects/grk

all:
	cd grammars/vscobolii; make
	chmod -fR go+rX .

test: all
	cd grammars/vscobolii; make test


clean:
	cd lib;      make clean
	cd tools;    make clean
	cd grammars; make clean
	rm -f *~
	rm -rf temp
	rm -rf ${distdir}*

# To be used by developer at cs.vu.nl
dist:
	rm -rf temp
	rm -rf ${distdir}*
	mkdir temp
	cd temp; cvs -d ${cvsroot} checkout ${project}
	mv temp/${project} ${distdir}
	rm -rf temp
	cd ${distdir}; find . -depth -name CVS -exec rm -rf \{\} \;
	tar -cvf ${distdir}.tar ${distdir}
	gzip ${distdir}.tar
	mv ${distdir}.tar.gz ${HOME}/archive
	chmod go+rX ${HOME}/archive/${distdir}.tar.gz
	rm -rf ${distdir}*
	cd ${HOME}/www/grammarware; \
	rm -f ${distdir}.tar.gz; \
	ln -s ${HOME}/archive/${distdir}.tar.gz
