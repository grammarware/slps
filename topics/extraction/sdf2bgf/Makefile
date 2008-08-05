validator = ../../../download/msv/msv.jar
metaenv = `which asfe | xargs dirname`/..
sdfl = ${metaenv}/share/sdf-library/library
asfl = ${metaenv}/share/asf-library/library
schemata = ../../../shared/xsd

all: Main.tbl Main.eqs test

Main.eqs:
	eqs-dump -m Main -o 1.eqs
	eqs-dump -m Tokens -o 2.eqs
	concat-asf -o Main.eqs 1.eqs 2.eqs
	rm -f 1.eqs 2.eqs

Main.tbl:
	pack-sdf -i Main.sdf -I ${sdfl} -o Main.def
	sdf2table -m Main Main.def

test:
	@echo "main(" > FL.sdf
	@cat ../../fl/asfsdf/syntax/FL.sdf >> FL.sdf
	@echo ")" >>FL.sdf
	cat FL.sdf | sglr -p Main.tbl | asfe -e Main.eqs | unparsePT > fl.bgf
	java -jar ${validator} -warning ${schemata}/bgf.xsd fl.bgf
 
clean:
	rm -f reduct.out
	rm -f Main.def Main.tbl *.eqs
	rm -f FL.sdf fl.bgf

