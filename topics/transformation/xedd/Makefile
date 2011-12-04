all:

try:
	../../recovery/hunter/hunter.py lll1t.doc.lll1t lll1t.edd lll1n.doc.bgf
	../../../shared/tools/cbgf2xbgf lll1.spec2doc.cbgf /dev/null lll1.reverse.xbgf
	../../../shared/tools/xbgf lll1.reverse.xbgf lll1n.doc.bgf lll1n.spec.bgf
	#../../../shared/tools/cbgf lll1t.coupled.cbgf 
	gdt lll1.spec.bgf lll1t.spec.bgf

specprop:
	# This scenario propagates changes from the notation to its parser, to its grammar, to its beautified grammar and to its definition "in itself"
	./xedd.py lll1.try.xedd lll1.edd lll1t.edd lll1t.coupled.cbgf
	# need to learn to run Rascal from command line
	cp backup/lll1t.rsc .
	../../../shared/tools/rsc2bgf lll1t.rsc lll1t.raw.bgf
	../../../shared/tools/normbgf lll1t.raw.bgf lll1t.spec.bgf
	../../../shared/tools/cbgf lll1t.coupled.cbgf lll1.spec.bgf lll1t.spec.bgf
	../../../shared/tools/cbgf2xbgf lll1.spec2doc.cbgf lll1.forward.xbgf /dev/null
	../../../shared/tools/xbgf lll1.forward.xbgf lll1t.spec.bgf lll1t.doc.bgf
	../../../shared/tools/reformat lll1t.doc.bgf lll1t.edd lll1t.doc.lll1t

edd:
	cp ../../../shared/edd/lll1.edd lll1.edd
	cp ../../../shared/edd/lll2.edd lll2.edd

rsc: edd
	# need to learn to run Rascal from command line
	cp ../../../../personal/work/gnf/src/bgf2rsc/GeneratedLLL.rsc .
	cp backup/lll?.rsc .

bgfspec: rsc
	../../../shared/tools/rsc2bgf lll1.rsc lll1.raw.bgf
	../../../shared/tools/normbgf lll1.raw.bgf lll1.spec.bgf
	../../../shared/tools/rsc2bgf lll2.rsc lll2.raw.bgf
	../../../shared/tools/normbgf lll2.raw.bgf lll2.spec.bgf

bgfdoc:
	cp ../../grammars/metasyntax/lll-ldta/grammar.bgf lll1.doc.bgf
	../../../shared/tools/reformat lll1.doc.bgf lll1.edd lll1.doc.lll1
	cp ../../grammars/metasyntax/lll-kort/grammar.bgf lll2.doc.bgf
	../../../shared/tools/reformat lll2.doc.bgf lll2.edd lll2.doc.lll2

bgfspecndoc: bgfspec bgfdoc
	../../../shared/tools/cbgf lll1.spec2doc.cbgf lll1.spec.bgf lll1.doc.bgf

get:
	cp ../../grammars/metasyntax/lll-kort/src.manually.fixed.txt lll-gdkref.lll
	cp ../../grammars/metasyntax/lll-ldta/src.ldta.2002.txt lll-ldta.lll
	cd ../../grammars/metasyntax/lll1-edd && make extract
	cd ../../grammars/metasyntax/lll2-edd && make extract
	../../../shared/tools/reformat ../../grammars/metasyntax/lll1-edd/grammar.bgf lll1.edd lll1.lll
	../../../shared/tools/reformat ../../grammars/metasyntax/lll2-edd/grammar.bgf lll2.edd lll2.lll

clean:
	rm -f lll?.edd GeneratedLLL.rsc lll?.rsc lll?.raw.bgf lll?.spec.bgf lll?.doc.bgf *.xbgf *.lll?? *.res
