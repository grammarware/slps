bgfdir = ../../../storage/bgf/tests.small
tooldir = ../../../../shared/tools

test:
	ls -1 ${bgfdir} | xargs -n1 ./runtestcase ${tooldir} ${bgfdir}

clean:
	rm -f *~ tmp.*.bgf tmp.xbgf
