metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library
testcase = ../shared/factorial.txt

all: FL.tbl FL.eqs test

FL.eqs: optimise/FL.asf syntax/Numbers.asf
	eqs-dump -m syntax/Numbers -o 1.eqs
	eqs-dump -m optimise/FL -o 2.eqs
	@LDIR=${PWD} ; cd ${asfl} ; eqs-dump -m basic/Integers -o $$LDIR/3.eqs
	concat-asf -o FL.eqs 1.eqs 2.eqs 3.eqs
	rm -f 1.eqs 2.eqs 3.eqs

FL.tbl: FL.sdf syntax/FL.sdf optimise/FL.sdf
	@if [ -r /usr/local/bin/pack-sdf ]; then \
		make packbuild ; \
	else \
		make catbuild ; \
	fi

test: test-parser

test-parser: FL.tbl
	@echo Parsing factorial:
	@echo --------------------------------------------------------
	@cat ${testcase}
	@echo --------------------------------------------------------
	@cat ${testcase} | python pre.py | sglr -p FL.tbl -A | unparsePT | python post.py

test-optimizer:
	@echo Optimising example:
	@echo --------------------------------------------------------
	@cat suboptimal.fl
	@echo --------------------------------------------------------
	@cat suboptimal.fl | python pre.py | sglr -p FL.tbl -A | asfe -e FL.eqs | restorebrackets -p FL.tbl | unparsePT | python post.py

catbuild:
	echo definition > FL.def
	@cat *.sdf syntax/*.sdf optimise/*.sdf >> FL.def
	@cat ${sdfl}/basic/Whitespace.sdf ${sdfl}/basic/Comments.sdf >> FL.def
	@cat ${sdfl}/basic/NatCon.sdf ${sdfl}/basic/IntCon.sdf ${sdfl}/basic/BoolCon.sdf >> FL.def
	@cat ${asfl}/basic/Integers.sdf ${asfl}/basic/Booleans.sdf >>FL.def
	sdf2table -m FL FL.def

packbuild:
	@cp ${sdfl}/basic/*.sdf basic
	@cp ${asfl}/basic/*.sdf basic
	pack-sdf -i FL.sdf -o FL.def
	sdf2table -m FL FL.def

clean:
	rm -f ?.eqs
	rm -f reduct.out
	rm -f FL.def FL.tbl FL.eqs
	rm -rf basic/*

