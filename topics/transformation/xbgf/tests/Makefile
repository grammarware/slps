tools = ../../../../shared/tools

build:

test:
	make add.test
	make chain.test
	make define.test
	make designate.test
	make deyaccify1.test
	make deyaccify2.test
	make eliminate.test
	make extract1.test
	make extract2.test
	make extract3.test
	make extract4.test
	make fold1.test
	make fold2.test
	make fold3.test
	make horizontal1.test
	make horizontal2.test
	make inject.test
	make inline.test
	make introduce.test
	make lassoc1.test
	make lassoc2.test
	make massage1.test
	make massage2.test
	make massage3.test
	make massage4.test
	make narrow1.test
	make narrow2.test
	make narrow3.test
	make permute.test
	make project.test
	make rassoc1.test
	make rassoc2.test
	make remove.test
	make rename1.test
	make rename2.test
	make rename3.test
	make rename4.test
	make rename5.test
	make replace1.test
	make replace2.test
	make replace3.test
	make reroot.test
	make skip.test
	make strip1.test
	make strip2.test
	make strip3.test
	make strip4.test
	make strip5.test
	make strip6.test
	make unchain.test
	make undefine.test
	make unfold1.test
	make unfold2.test
	make unfold3.test
	make unite.test
	make vertical1.test
	make vertical2.test
	make widen1.test
	make widen2.test
	make widen3.test
	make yaccify.test

rebuild:
	make clean
	make test

%.test:
	${tools}/checkxml bgf $*.bgf
	${tools}/checkxml xbgf $*.xbgf
	${tools}/xbgf $*.xbgf $*.bgf $*.out
	${tools}/gdt $*.out $*.baseline

clean:
	rm -f *.out
	rm -f *~
