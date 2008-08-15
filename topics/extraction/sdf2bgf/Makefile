metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library
schemata = ../../../shared/xsd

all: build test

build: Main.tbl Main.eqs

Main.eqs:
	eqs-dump -m Main -o 1.eqs
	eqs-dump -m Tokens -o 2.eqs
	concat-asf -o Main.eqs 1.eqs 2.eqs
	rm -f 1.eqs 2.eqs

Main.tbl:
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
	@cat ${sdfl}/languages/sdf2/syntax/*.sdf >> Main.def
	@cat ${sdfl}/languages/xml/syntax/XML.sdf >> Main.def
	@cat ${sdfl}/basic/Whitespace.sdf >> Main.def
	@cat ${sdfl}/basic/NatCon.sdf >> Main.def
	@cat ${sdfl}/basic/StrCon.sdf >> Main.def
	@cat ${sdfl}/basic/IdentifierCon.sdf >> Main.def
	@cat ${sdfl}/basic/Comments.sdf >> Main.def
	@cat Main.sdf Tokens.sdf >> Main.def
	sdf2table -m Main Main.def

test:
	@echo "main(" > FL.trm
	@cat ../../fl/asfsdf/syntax/FL.sdf >> FL.trm
	@echo ")" >>FL.trm
	cat FL.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > fl.bgf
	xmllint --noout --schema ${schemata}/bgf.xsd fl.bgf
 
clean:
	rm -f reduct.out
	rm -f Main.def Main.tbl *.eqs
	rm -f FL.trm fl.bgf

