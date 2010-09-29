bgfdir = ../../storage/bgf/tests
tooldir = ../../../shared/tools

test:
	ls -1 ${bgfdir} | xargs -n1 ./bgf2txl2bgf ${tooldir} ${bgfdir}

fl:
	cp ../../fl/txl/FL.Txl FL0.txl
	${tooldir}/txl2bgf FL0.txl fl1.bgf
	${tooldir}/bgf2txl fl1.bgf FL2.txl
	${tooldir}/txl2bgf FL2.txl fl3.bgf
	${tooldir}/gdt fl1.bgf fl3.bgf

clean:
	rm -f *~ *.bgf *.txl *.bnf
