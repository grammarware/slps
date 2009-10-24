metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library
testcase = ../shared/factorial.txt

all: FL.tbl FL.eqs test

FL.eqs: Optimiser.asf Numbers.asf
	eqs-dump -m Optimiser -o Optimiser.eqs
	eqs-dump -m Numbers -o Numbers.eqs
	@LDIR=${PWD} ; cd ${asfl} ; eqs-dump -m basic/Integers -o $$LDIR/Integers.eqs
	concat-asf -o FL.eqs Optimiser.eqs Integers.eqs Numbers.eqs

FL.tbl: FL.sdf Syntax.sdf Numbers.sdf Optimiser.sdf
	@if [ -r /usr/local/bin/pack-sdf ]; then \
		make packbuild ; \
	else \
		make catbuild ; \
	fi

test: test-parser test-optimizer

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
	@cat *.sdf >> FL.def
	@AA=${PWD};cd ${sdfl};cat $${AA}/sdflib.list | xargs -n1 -J % cat % >>$${AA}/FL.def
	@AA=${PWD};cd ${asfl};cat $${AA}/asflib.list | xargs -n1 -J % cat % >>$${AA}/FL.def
	sdf2table -m FL -i FL.def -o FL.tbl

packbuild:
	@cp ${sdfl}/basic/*.sdf basic
	@cp ${asfl}/basic/*.sdf basic
	pack-sdf -i FL.sdf -o FL.def
	sdf2table -m FL -i FL.def -o FL.tbl

clean:
	rm -f reduct.out
	rm -f FL.def FL.tbl *.eqs
	rm -rf basic/*

