all:

test:
	./extract.py ../../topics/grammars/ RascalRascal.rsc rascal.bgf
	../../../shared/tools/validate bgf rascal.bgf

clean:
	rm -f *.bgf *.bnf *.html
