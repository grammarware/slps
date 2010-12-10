all:
	./extract.py RascalRascal.rsc rascal.bgf
	../../../shared/tools/validate bgf rascal.bgf
	../../../shared/tools/bgf2bnf rascal.bgf rascal.bnf
	../../../shared/tools/xbgf horizontalize.xbgf rascal.bgf rascal.hor.bgf
	../../../shared/tools/validate bgf rascal.hor.bgf
	../../../shared/tools/bgf2bnf rascal.hor.bgf rascal.hor.bnf

clean:
	rm -f *.bgf *.bnf
