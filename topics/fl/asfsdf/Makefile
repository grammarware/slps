metaenv = ~/Tools/Meta

all:
	make build
	make test

test:
	cat suboptimal.trm
	sglr -p FL.tbl -i suboptimal.trm | asfe -e FL.eqs | unparsePT

build:
	echo definition > FL.def
	cat *.sdf syntax/*.sdf optimise/*.sdf >> FL.def
	cat ${metaenv}/share/sdf-library/library/basic/*.sdf >> FL.def
	cat ${metaenv}/share/asf-library/library/basic/*.sdf >> FL.def
	sdf2table -m FL FL.def

clean:
	rm -f reduct.out
	rm -f FL.def

