all:

test:
	../../../shared/tools/rscs2bgf ../../fl/rascal/src/Concrete.rsc rsc1.bgf
	../../../shared/tools/rscd2bgf ../../fl/rascal/src/Abstract.rsc rsc2.bgf
	../../../shared/tools/rsc2bgf  ../../fl/rascal/src/Concrete.rsc rsc3.bgf
	ls -1 *.bgf | xargs -n1 ../../../shared/tools/validate bgf

clean:
	rm -f *.bgf *.bnf *.html
