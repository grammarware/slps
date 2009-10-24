metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library
flsdf = ../../fl/asfsdf/

build: Main.tbl Main.eqs

Main.eqs: *.asf *.sdf
	eqs-dump -m Main -o Main.eqs

Main.tbl: *.sdf
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
	@cat Main.sdf Tokens.sdf >> Main.def

test:
	@echo Building FL.trm from ${flsdf} ...
	@echo "main(definition" > FL.trm
	@cat ${flsdf}/*.sdf >> FL.trm
	@echo ")" >>FL.trm
	cat FL.trm | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > fl.bgf
	../../../shared/tools/checkxml bgf fl.bgf
 
clean:
	rm -f reduct.out
	rm -f Main.def Main.tbl *.eqs
	rm -f FL.trm fl.bgf

