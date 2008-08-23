fl = ..
tools = ../../../shared/tools
xbgf = ${fl}/lci/xbgf
sdf = ${fl}/asfsdf/syntax

build: limit.bgf limit.btf

test:
	swipl -q -f TestEvaluator.pro -t main -- 120 limit.btf fac 5

append.xbgf:
	appendxbgf append.xbgf \
		${xbgf}/downcase.xbgf \
		${xbgf}/defineLex.xbgf \
		${xbgf}/stripTerminals.xbgf \
		${xbgf}/permuteArgs.xbgf \
		${xbgf}/reroot.xbgf

limit.bgf: append.xbgf
	${tools}/sdf2bgf ${sdf}/FL.sdf ${sdf}/Numbers.sdf source.bgf
	${tools}/xbgf append.xbgf source.bgf limit.bgf

limit.btf: append.xbgf
	${tools}/asfix2btf ${sdf} ${fl}/shared/factorial.txt source.btf
	${tools}/xbtf append.xbgf source.btf limit.btf

rebuild:
	make clean
	make build

test: build

show: build
	${tools}/showbgf limit.bgf
	${tools}/showpnf limit.btf

clean:
	rm -f *~
	rm -f *.bgf
	rm -f *.btf
	rm -f *.xbgf
