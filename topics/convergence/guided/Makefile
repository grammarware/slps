all:
	make test

build:
	cp ../../mutation/anf/normal/*.normal.bgf base/
	xbgf prepare/dcg.xbgf base/dcg.normal.bgf base/dcg.prepared.bgf
	xbgf prepare/antlr.xbgf base/antlr.normal.bgf base/antlr.prepared.bgf
	xbgf prepare/ecore.fix.xbgf base/ecore.normal.bgf base/ecore.fixed.bgf
	xbgf prepare/ecore.xbgf base/ecore.fixed.bgf base/ecore.prepared.bgf

test:
	./converge.py master.bgf good/

clean:
	rm -f good/*.bgf *.xbgf master.bgf.*
