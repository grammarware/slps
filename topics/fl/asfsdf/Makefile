metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library

all:
	if [ -r /usr/local/bin/pack-sdf ]; then \
		make strategobuild ; \
	else \
		make build ; \
	fi
	make test

test:
	cat suboptimal.fl
	cat suboptimal.fl | python pre.py | sglr -p FL.tbl | asfe -e FL.eqs | restorebrackets -p FL.tbl | unparsePT | python post.py

build:
	echo definition > FL.def
	cat *.sdf syntax/*.sdf optimise/*.sdf >> FL.def
	cat ${sdfl}/basic/Whitespace.sdf ${sdfl}/basic/Comments.sdf >> FL.def
	cat ${sdfl}/basic/NatCon.sdf ${sdfl}/basic/IntCon.sdf ${sdfl}/basic/BoolCon.sdf >> FL.def
	cat ${asfl}/basic/Integers.sdf ${asfl}/basic/Booleans.sdf >>FL.def
	sdf2table -m FL FL.def

strategobuild:
	@cp ${sdfl}/basic/*.sdf basic
	@cp ${asfl}/basic/*.sdf basic
	pack-sdf -i FL.sdf -o FL.def
	sdf2table -m FL FL.def

clean:
	rm -f reduct.out
	rm -f FL.def
	rm -rf basic/*
