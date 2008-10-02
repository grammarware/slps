validator = ../../../shared/tools/checkxml

build:

diff2: test
	gdt bgf/jls1app.prepare1.refactorStatements.refactorTypes.structure1.addFeatures1to2.cropFeatures1.bgf bgf/jls2.prepare2.bgf

diff:
	../../../shared/tools/xbgf xbgf/refactorClassDeclarations3.xbgf bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.deyaccifyExpressions3.inlineStatements2doc.inlineExpressions2doc.bgf bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.deyaccifyExpressions3.inlineStatements2doc.inlineExpressions2doc.refactorClassDeclarations3.bgf
	../../../shared/tools/gdt bgf/jls3syntax.fixErrors3.prepare3.bgf bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.deyaccifyExpressions3.inlineStatements2doc.inlineExpressions2doc.refactorClassDeclarations3.bgf


test:
	python ../../convergence/lci/lci.py java.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

debug:
	../../../shared/tools/html2bgf ../jls1/syntax.kw ../jls1/syntax.html 1.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls1/collect.kw ../jls1/collected.html q.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls2/syntax.html 2.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls2/collect.kw ../jls2/collected.html w.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls3/syntax.html 3.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls3/collect.kw ../jls3/collected.html e.bnf -bnf >/dev/null
clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log

