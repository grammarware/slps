metaenv = `which sdf2table | xargs dirname`/../share
sdfl = ${metaenv}/sdf-library/library/basic
asfl = ${metaenv}/asf-library/library/basic

all:
	make build
	make test

test:
	cat suboptimal.fl
	cat suboptimal.fl | python pre.py | sglr -p FL.tbl | asfe -e FL.eqs | restorebrackets -p FL.tbl | unparsePT | python post.py

build:
	echo definition > FL.def
	cat *.sdf syntax/*.sdf optimise/*.sdf >> FL.def
	cat ${sdfl}/Whitespace.sdf ${sdfl}/Comments.sdf >> FL.def
	cat ${sdfl}/NatCon.sdf ${sdfl}/IntCon.sdf ${sdfl}/BoolCon.sdf >> FL.def
	cat ${asfl}/Integers.sdf ${asfl}/Booleans.sdf >>FL.def
	sdf2table -m FL FL.def

clean:
	rm -f reduct.out
	rm -f FL.def

