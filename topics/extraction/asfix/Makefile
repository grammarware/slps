metaenv = /opt/meta
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library

build: Main.tbl Main.eqs

Main.eqs: *.sdf *.asf
	eqs-dump -m Main -o Main.eqs

Main.tbl: *.sdf
	cp ../sdf/Tokens.* .
	@if [ -r /usr/local/bin/pack-sdf ]; then \
		make tblpack; \
	else \
		make tblcat; \
	fi
	sdf2table -m Main -i Main.def -o Main.tbl

tblpack:
	pack-sdf -i Main.sdf -I ${sdfl} -o Main.def

tblcat:
	@echo "definition" > Main.def
	@AA=${PWD};cd ${sdfl};cat $${AA}/sdflib.list | xargs -n1 -J % cat % >>$${AA}/Main.def
	@cat Main.sdf Tokens.sdf AsFixTokens.sdf AsFixToBGF.sdf >> Main.def

test:
	@echo "main(" > asfix.trm
	@cat small.fl | python ../../fl/asfsdf/pre.py | sglr -p ../../fl/asfsdf/FL.tbl -t >> asfix.trm
	@echo ")" >> asfix.trm
	@echo "main(" > fac.trm
	@cat ../../fl/shared/factorial.txt | python ../../fl/asfsdf/pre.py | sglr -p ../../fl/asfsdf/FL.tbl -t >> fac.trm
	@echo ")" >> fac.trm
	cat asfix.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > small.btf
	cat fac.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > myfactorial1.btf
	xsltproc ../../../shared/xsl/btf2btf.xslt myfactorial1.btf > myfactorial.btf
	cp ../xml2btf/tests/factorial.btf .
	ls -1 *.btf | xargs -n1 validate btf
 
clean:
	rm -f reduct.out
	rm -f Main.def Main.tbl *.eqs
	rm -f FL.sdf fl.bgf
	rm -f asfix.trm fac.trm small.btf myfactorial.btf

