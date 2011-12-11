all:

m:
	# suppose we have lll2gen.spec.bgf; let's rerun xedd just to get coupled
	./xedd.py lll1to2.xedd LLL1.edd LLL2gen.edd lll1to2.coupled.cbgf lll1to2.reformat.cbgf

m2:
	# This scenario propagates changes from the notation to its parser, to its grammar, to its beautified grammar and to its definition "in itself"
	# Changes from LLL1 to LLL2
	./xedd.py lll1to2.xedd LLL1.edd LLL2gen.edd lll1to2.coupled.cbgf lll1to2.reformat.cbgf
	../../../shared/tools/edd2rsc LLL2gen.edd LLL2gen
	../../../shared/tools/rsc2bgf LLL2gen.rsc lll2gen.raw.bgf SL
	../../../shared/tools/normbgf lll2gen.raw.bgf lll2gen.spec.bgf
	../../../shared/tools/cbgf lll1to2.coupled.cbgf lll1.spec.bgf lll2gen.spec.bgf
	../../../shared/tools/cbgf2 lll1to2.coupled.cbgf lll1.spec2doc.cbgf lll2gen.spec2doc.cbgf
	../../../shared/tools/cbgf2xbgf lll2gen.spec2doc.cbgf lll2gen.forward.xbgf /dev/null
	../../../shared/tools/xbgf lll2gen.forward.xbgf lll2gen.spec.bgf lll2gen.doc.bgf
	../../../shared/tools/bgfpp lll2gen.doc.bgf LLL2.edd lll2gen.doc.lll2

lll:
	for f in ../../../shared/bgf/*.bgf ; do ../../../shared/tools/bgfpp "$$f" LLL1.edd llls/`basename "$$f" .bgf`.lll ; done

try:
	# This scenario propagates changes from the definition "in itself" to the grammar, to the unbeautified grammar
	# should also propagate farther: to the parser, to the notation and finally to the (calculated) notation delta
	../../recovery/hunter/hunter.py lll1t.doc.lll1t LLL1t.edd lll1n.doc.bgf
	# in this simple case, spec2doc.xbgf is still okay
	../../../shared/tools/cbgf2xbgf lll1.spec2doc.cbgf /dev/null lll1.reverse.xbgf
	../../../shared/tools/xbgf lll1.reverse.xbgf lll1n.doc.bgf lll1n.spec.bgf
	# could not detect starting symbols from LLL notation, using an automated mutation
	../../../shared/generators/reroot2top lll1n.spec.bgf lll1n.reroot.xbgf
	../../../shared/tools/xbgf lll1n.reroot.xbgf lll1n.spec.bgf lll1n.spec2.bgf
	# should be equal
	../../../shared/tools/gdt lll1t.spec.bgf lll1n.spec2.bgf
	../../../shared/tools/bgf2rsc lll1n.spec.bgf lll1n.rsc

migrate1:
	rm -f mllls/*
	ls -1 llls | xargs -n1 -I {} make {}.migrate1

migrate2:
	rm -f mllls/*
	ls -1 llls | xargs -n1 -I {} make {}.migrate2

%.migrate1:
	../../../shared/tools/bgfreformat llls/$* LLL1.edd LLL1t.edd mllls/`basename $* .lll`.lll

%.migrate2:
	../../../shared/tools/bgfreformat llls/$* LLL1.edd LLL1t.edd mllls/`basename $* .lll`.lll lll1to2.reformat.cbgf

specprop1:
	# This scenario propagates changes from the notation to its parser, to its grammar, to its beautified grammar and to its definition "in itself"
	./xedd.py lll1.try.xedd LLL1.edd LLL1t.edd lll1t.coupled.cbgf lll1t.reformat.cbgf
	# running Rascal from command line
	../../../shared/tools/edd2rsc LLL1t.edd LLL1t
	#cp backup/lll1t.rsc .
	../../../shared/tools/rsc2bgf LLL1t.rsc lll1t.raw.bgf SL
	../../../shared/tools/normbgf lll1t.raw.bgf lll1t.spec.bgf
	../../../shared/tools/cbgf lll1t.coupled.cbgf lll1.spec.bgf lll1t.spec.bgf
	../../../shared/tools/cbgf2 lll1t.coupled.cbgf lll1.spec2doc.cbgf lll1t.spec2doc.cbgf
	../../../shared/tools/cbgf2xbgf lll1t.spec2doc.cbgf lll1t.forward.xbgf /dev/null
	../../../shared/tools/xbgf lll1t.forward.xbgf lll1t.spec.bgf lll1t.doc.bgf
	../../../shared/tools/bgfpp lll1t.doc.bgf LLL1t.edd lll1t.doc.lll1t

edd:
	#cp ../../../shared/edd/LLL1.edd LLL1.edd
	#cp ../../../shared/edd/LLL2.edd LLL2.edd
	echo Copied

rsc: edd
	# running Rascal from command line
	../../../shared/tools/edd2rsc LLL1.edd LLL1
	../../../shared/tools/edd2rsc LLL2.edd LLL2
	#cp ../../../../personal/work/gnf/src/bgf2rsc/GeneratedLLL.rsc .
	#cp backup/lll?.rsc .

bgfspec: rsc
	../../../shared/tools/rsc2bgf LLL1.rsc lll1.raw.bgf SL
	../../../shared/tools/normbgf lll1.raw.bgf lll1.spec.bgf
	../../../shared/tools/rsc2bgf LLL2.rsc lll2.raw.bgf SL
	../../../shared/tools/normbgf lll2.raw.bgf lll2.spec.bgf

bgfdoc:
	cp ../../grammars/metasyntax/lll-ldta/grammar.bgf lll1.doc.bgf
	../../../shared/tools/bgfpp lll1.doc.bgf LLL1.edd lll1.doc.lll1
	# need to rerun hunter since otherwise it's without seplists (legacy?)
	../../recovery/hunter/hunter.py ../../grammars/metasyntax/lll-kort/src.manually.fixed.txt ../../../shared/edd/LLL2.edd lll2.doc.bgf
	../../../shared/tools/bgfpp lll2.doc.bgf LLL2.edd lll2.doc.lll2

bgfspecndoc: bgfspec bgfdoc
	../../../shared/tools/cbgf lll1.spec2doc.cbgf lll1.spec.bgf lll1.doc.bgf
	../../../shared/tools/cbgf lll2.spec2doc.cbgf lll2.spec.bgf lll2.doc.bgf

get:
	cp ../../grammars/metasyntax/lll-kort/src.manually.fixed.txt lll-gdkref.lll
	cp ../../grammars/metasyntax/lll-ldta/src.ldta.2002.txt lll-ldta.lll
	cd ../../grammars/metasyntax/lll1-edd && make extract
	cd ../../grammars/metasyntax/lll2-edd && make extract
	../../../shared/tools/bgfpp ../../grammars/metasyntax/lll1-edd/grammar.bgf LLL1.edd lll1.lll
	../../../shared/tools/bgfpp ../../grammars/metasyntax/lll2-edd/grammar.bgf LLL2.edd lll2.lll

clean:
	rm -f LLL*.edd GeneratedLLL.rsc lll?.rsc lll?.raw.bgf lll?.spec.bgf lll?.doc.bgf *.xbgf *.lll?? *.res
