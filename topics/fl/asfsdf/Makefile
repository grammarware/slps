metaenv = `which sdf2table | xargs dirname`/../share

all:
	make build
	make test

test:
	cat suboptimal.fl
	cat suboptimal.fl | python pre.py | sglr -p FL.tbl | asfe -e FL.eqs | restorebrackets -p FL.tbl | unparsePT | python post.py

build:
	echo definition > FL.def
	cat *.sdf syntax/*.sdf optimise/*.sdf >> FL.def
	cat ${metaenv}/sdf-library/library/basic/*.sdf >> FL.def
	cat ${metaenv}/asf-library/library/basic/*.sdf >> FL.def
	sdf2table -m FL FL.def

clean:
	rm -f reduct.out
	rm -f FL.def

