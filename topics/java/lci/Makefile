validator = ../../../shared/tools/checkxml

build:

diff2:
	xbgf xbgf/unlayerExpressions3.xbgf bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.bgf foo
	gdt xbgf.log bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.unlayerExpressions3.bgf | grep -v Ok

diff:
	gdt bgf/jls3app.fixErrors3.prepare3.bgf bgf/jls3doc.fixErrors3doc.unifyModifiers.unifyNames.deyaccify2doc.refactorTypes3.deyaccifyExpressions3.inlineStatements2doc.inlineExpressions2doc.refactorClassDeclarations3.refactorEnumDeclarations.refactorInterfaceDeclarations3.breakSemantics3doc.bgf

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

