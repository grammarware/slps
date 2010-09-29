bgfdir = ../../storage/bgf/tests
tooldir = ../../../shared/tools

test:
	ls -1 ${bgfdir} | xargs -n1 ./bgf2txl2bgf ${tooldir} ${bgfdir}

clean:
	rm -f *~ *.bgf *.txl *.bnf
