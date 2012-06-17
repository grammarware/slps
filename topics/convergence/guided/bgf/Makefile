build:
	../../../../shared/tools/antlr2bgf ../../../fl/java1/FL.g antlr.bgf
	../../../../shared/tools/dcg2bgf ../../../fl/prolog1/Parser.pro dcg.bgf
	../../../../shared/tools/sdf2bgf ../../../fl/asfsdf/Syntax.sdf sdf.bgf
	../../../../shared/tools/xsd2bgf ../../../fl/xsd/fl.xsd xsd.bgf
	../../../../shared/tools/java2bgf ../../../fl/java1 types om.bgf
	../../../../shared/tools/java2bgf ../../../fl/java3 fl jaxb.bgf
	../../../../shared/tools/ecore2bgf ../../../fl/emf1/fl.ecore ecore.bgf
	../../../../shared/tools/ecore2bgf ../../../fl/emf2/model/fl.ecore emf.bgf
	../../../../shared/tools/py2bgf ../../../fl/python/parser2.py python.bgf
	../../../../shared/tools/rscs2bgf ../../../fl/rascal/src/Concrete.rsc rascal-c.bgf
	../../../../shared/tools/rscd2bgf ../../../fl/rascal/src/Abstract.rsc rascal-a.bgf
	../../../../shared/tools/txl2bgf ../../../fl/txl/FL.Txl txl.bgf

test:
	ls -1 *.bgf | xargs -n1 ../../../../shared/tools/validate bgf

clean:
	rm -rf *.bgf

