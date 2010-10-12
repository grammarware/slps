fl = ..
tools = ../../../shared/tools
xbgf = ${fl}/lci/xbgf
sdf = ${fl}/asfsdf

build: limit.bgf limit.btf

test:
	swipl -q -f TestEvaluator.pro -t main -- 120 limit.btf fac 5

append.xbgf:
	${tools}/appendxbgf append.xbgf \
		${xbgf}/renameSdf.xbgf \
		${xbgf}/defineLex.xbgf \
		${xbgf}/stripTerminals.xbgf \
		${xbgf}/permuteArgs.xbgf \
		${xbgf}/reroot.xbgf
	${tools}/validate xbgf append.xbgf

limit.bgf: append.xbgf
	${tools}/sdf2bgf ${sdf}/*.sdf source.bgf
	${tools}/validate bgf source.bgf
	${tools}/xbgf append.xbgf source.bgf limit.bgf
	${tools}/validate bgf limit.bgf

limit.btf: append.xbgf
	${tools}/asfix2btf ${sdf} ${fl}/shared/factorial.txt source.btf
	${tools}/validate btf source.btf
	${tools}/xbtf append.xbgf source.btf limit.btf
	${tools}/validate btf limit.btf

rebuild:
	make clean
	make build

test: build

check:
	make build
	cd ../prolog2; make build
	tdt ../prolog2/limit.btf limit.btf

show: build
	${tools}/showbgf limit.bgf
	${tools}/showpnf limit.btf

clean:
	rm -f *~
	rm -f *.bgf
	rm -f *.btf
	rm -f *.xbgf
