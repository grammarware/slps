validator = ../../../shared/tools/checkxml

build:

diff:
	python ../../convergence/lci/lci.py java3.lcf architecture
	gdts bgf/app3.fixErrors3.prepare3.allowMultipleLabels.bgf bgf/doc3.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.unlayerExpressions3.inlineStatements2doc.inlineExpressions3doc.refactorClassDeclarations3.refactorInterfaceDeclarations3.refactorMiscDeclarations3.breakSemantics3doc.bgf

test:
	python ../../convergence/lci/lci.py java.lcf architecture
	@rm -f architecture*.dot

check:
	${validator} lcf java.lcf
	ls -1 xbgf/*.xbgf | xargs -n1 ${validator} xbgf
	ls -1  bgf/*.bgf  | xargs -n1 ${validator} bgf

debug:
	../../../shared/tools/html2bgf ../jls1/syntax.kw ../jls1/app.html 1.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls1/collect.kw ../jls1/doc.html q.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls2/app.html 2.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls2/collect.kw ../jls2/doc.html w.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls3/app.html 3.bnf -bnf >/dev/null
	../../../shared/tools/html2bgf ../jls3/collect.kw ../jls3/doc.html e.bnf -bnf >/dev/null
clean:
	rm -f *~
	rm -f bgf/*
	rm -rf bin obj
	rm -f *.user *.suo
	rm -f architecture*
	rm -f java.log

