all:

test:
	../../../shared/tools/cbgf tests/om-jaxb.cbgf tests/bgf/om.bgf tests/bgf/jaxb.bgf
	../../../shared/tools/cbgf tests/emf-xsd.cbgf tests/bgf/ecore.bgf tests/bgf/xsd.bgf

todo:
	../../../shared/tools/cbgf tests/antlr-dcg.cbgf tests/bgf/antlr.bgf tests/bgf/dcg.bgf

prep:
	cp ../../convergence/fl/snapshot/*.bgf tests/bgf

clean:
	rm -f *~ tests/*.forward.* tests/*.reverse.* tests/bgf/*.res
