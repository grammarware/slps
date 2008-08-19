metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library

build: Main.tbl Main.eqs

Main.eqs: *.sdf *.asf
	eqs-dump -m Main -o Main.eqs

Main.tbl: *.sdf
	cp ../sdf2bgf/Tokens.* .
	@if [ -r /usr/local/bin/pack-sdf ]; then \
		make tblpack; \
	else \
		make tblcat; \
	fi

tblpack:
	pack-sdf -i Main.sdf -I ${sdfl} -o Main.def
	sdf2table -m Main Main.def

tblcat:
	@echo "definition" > Main.def
	@cat ${sdfl}/languages/aterm/syntax/*.sdf >> Main.def
	@cat ${sdfl}/languages/asfix/syntax/*.sdf >> Main.def
	@cat ${sdfl}/languages/sdf2/syntax/*.sdf >> Main.def
	@cat ${sdfl}/languages/xml/syntax/XML.sdf >> Main.def
	@cat ${sdfl}/basic/Whitespace.sdf >> Main.def
	@cat ${sdfl}/basic/NatCon.sdf >> Main.def
	@cat ${sdfl}/basic/StrCon.sdf >> Main.def
	@cat ${sdfl}/basic/IdentifierCon.sdf >> Main.def
	@cat ${sdfl}/basic/Comments.sdf >> Main.def
	@cat Main.sdf Tokens.sdf AsFixTokens.sdf AsFixToBGF.sdf >> Main.def
	sdf2table -m Main Main.def

test:
	@echo "main(" > asfix.trm
	@cat small.fl | python ../../fl/asfsdf/pre.py | sglr -p ../../fl/asfsdf/FL.tbl -t >> asfix.trm
	@echo ")" >> asfix.trm
	@echo "main(" > fac.trm
	@cat ../../fl/shared/factorial.txt | python ../../fl/asfsdf/pre.py | sglr -p ../../fl/asfsdf/FL.tbl -t >> fac.trm
	@echo ")" >> fac.trm
	cat asfix.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > small.btf
	cat fac.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > myfactorial.btf
	cp ../xml2btf/tests/factorial.btf .
	ls -1 *.btf | xargs -n1 checkxml btf
 
clean:
	rm -f reduct.out
	rm -f Main.def Main.tbl *.eqs
	rm -f FL.sdf fl.bgf
	rm -f asfix.trm fac.trm small.btf myfactorial.btf

