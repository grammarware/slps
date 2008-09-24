log = 
redirect = >> $*.log 2>&1
tools = ../../../../shared/tools

build:

test:
	@make add.quiet
	@make chain.quiet
	@make define.quiet
	@make designate.quiet
	@make deyaccify1.quiet
	@make deyaccify2.quiet
	@make deyaccify3.quiet
	@make distribute1.quiet
	@make eliminate.quiet
	@make extract1.quiet
	@make extract2.quiet
	@make extract3.quiet
	@make extract4.quiet
	@make factor1.quiet
	@make factor2.quiet
	@make factor3.quiet
	@make factor4.quiet
	@make fold1.quiet
	@make fold2.quiet
	@make fold3.quiet
	@make horizontal1.quiet
	@make horizontal2.quiet
	@make inject.quiet
	@make inline.quiet
	@make introduce.quiet
	@make lassoc1.quiet
	@make lassoc2.quiet
	@make massage1.quiet
	@make massage2.quiet
	@make massage3.quiet
	@make massage4.quiet
	@make narrow1.quiet
	@make narrow2.quiet
	@make narrow3.quiet
	@make permute.quiet
	@make project.quiet
	@make rassoc1.quiet
	@make rassoc2.quiet
	@make remove.quiet
	@make rename1.quiet
	@make rename2.quiet
	@make rename3.quiet
	@make rename4.quiet
	@make rename5.quiet
	@make replace1.quiet
	@make replace2.quiet
	@make replace3.quiet
	@make replace4.quiet
	@make reroot.quiet
	@make skip.quiet
	@make strip1.quiet
	@make strip2.quiet
	@make strip3.quiet
	@make strip4.quiet
	@make strip5.quiet
	@make strip6.quiet
	@make unchain.quiet
	@make undefine.quiet
	@make unfold1.quiet
	@make unfold2.quiet
	@make unfold3.quiet
	@make unite.quiet
	@make vertical1.quiet
	@make vertical2.quiet
	@make widen1.quiet
	@make widen2.quiet
	@make widen3.quiet
	@make yaccify.quiet

rebuild:
	make clean
	make test

%.test:
	@echo [Test XBGF] $*
	@rm -f $*.log
	@touch $*.log
	@${tools}/checkxml bgf $*.bgf ${log}
	@${tools}/showbgf $*.bgf ${log}
	@${tools}/checkxml xbgf $*.xbgf ${log}
	@${tools}/showxbgf $*.xbgf ${log}
	@${tools}/xbgf $*.xbgf $*.bgf $*.out ${log}
	@${tools}/showbgf $*.out ${log}
	@${tools}/gdt $*.out $*.baseline ${log}

%.quiet:
	@make $*.test log="${redirect}"

clean:
	rm -f *.out
	rm -f *.log
	rm -f *~
