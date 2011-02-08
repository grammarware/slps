tdir = ../../shared/tools
gdir = ../../topics/grammars
cdir = ../../topics/convergence

all:
	make clean
	make fl
	make c
	make cpp
	make csharp
	make java

fl:
	cd ${cdir}/fl && make test
	cp ${cdir}/fl/snapshot/antlr.bgf          fl.antlr.ext.bgf
	cp ${cdir}/fl/snapshot/dcg.bgf            fl.dcg.ext.bgf
	cp ${cdir}/fl/snapshot/ecore.bgf          fl.ecore.ext.bgf
	cp ${cdir}/fl/snapshot/ecore2.bgf         fl.xsd.ecore.ext.bgf
	cp ${cdir}/fl/snapshot/jaxb.bgf           fl.xsd.java.ext.bgf
	cp ${cdir}/fl/snapshot/om.bgf             fl.java.ext.bgf
	cp ${cdir}/fl/snapshot/sdf.bgf            fl.sdf.ext.bgf
	cp ${cdir}/fl/snapshot/txl.bgf            fl.txl.ext.bgf
	${tdir}/subgrammar fl.antlr.ext.bgf       program       fl.antlr.rec.bgf
	${tdir}/subgrammar fl.dcg.ext.bgf         program       fl.dcg.rec.bgf
	${tdir}/subgrammar fl.ecore.ext.bgf       Program       fl.ecore.rec.bgf
	${tdir}/subgrammar fl.xsd.ecore.ext.bgf   ProgramType   fl.xsd.ecore.rec.bgf
	${tdir}/subgrammar fl.xsd.java.ext.bgf    Function      fl.xsd.java.rec.bgf
	${tdir}/subgrammar fl.java.ext.bgf        Function      fl.java.rec.bgf
	${tdir}/subgrammar fl.sdf.ext.bgf         Program       fl.sdf.rec.bgf
	${tdir}/subgrammar fl.txl.ext.bgf         program       fl.txl.rec.bgf

csharp:
	cd ${gdir}/csharp && make build
	cp ${gdir}/csharp/ecma-334-4/extracted-grammar.bgf   csharp.ecma-334.ext.bgf
	cp ${gdir}/csharp/ecma-334-4/recovered-grammar.bgf   csharp.ecma-334.rec.bgf
	cp ${gdir}/csharp/msft-ls-1.0/extracted-grammar.bgf  csharp.microsoft-1.ext.bgf
	cp ${gdir}/csharp/msft-ls-1.0/recovered-grammar.bgf  csharp.microsoft-1.rec.bgf
	cp ${gdir}/csharp/msft-ls-3.0/extracted-grammar.bgf  csharp.microsoft-3.ext.bgf
	cp ${gdir}/csharp/msft-ls-3.0/recovered-grammar.bgf  csharp.microsoft-3.rec.bgf
	cp ${gdir}/csharp/msft-ls-4.0/extracted-grammar.bgf  csharp.microsoft-4.ext.bgf
	cp ${gdir}/csharp/msft-ls-4.0/recovered-grammar.bgf  csharp.microsoft-4.rec.bgf

cpp:
	cd ${gdir}/cpp && make build
	cp ${gdir}/cpp/iso-14882-1998/extracted-grammar.bgf  cpp.iso-14882-1998.ext.bgf
	cp ${gdir}/cpp/iso-14882-1998/recovered-grammar.bgf  cpp.iso-14882-1998.rec.bgf
	cp ${gdir}/cpp/iso-n2723/extracted-grammar.bgf       cpp.iso-n2723.ext.bgf
	cp ${gdir}/cpp/iso-n2723/recovered-grammar.bgf       cpp.iso-n2723.rec.bgf

c:
	cd ${gdir}/c && make build
	cp ${gdir}/c/iso-9899-1999/extracted-grammar.bgf     c.iso-9899-1999.ext.bgf
	cp ${gdir}/c/iso-9899-1999/recovered-grammar.bgf     c.iso-9899-1999.rec.bgf
	cp ${gdir}/c/iso-9899-tc2/extracted-grammar.bgf      c.iso-9899-tc2.ext.bgf
	cp ${gdir}/c/iso-9899-tc2/recovered-grammar.bgf      c.iso-9899-tc2.rec.bgf
	cp ${gdir}/c/iso-9899-tc3/extracted-grammar.bgf      c.iso-9899-tc3.ext.bgf
	cp ${gdir}/c/iso-9899-tc3/recovered-grammar.bgf      c.iso-9899-tc3.rec.bgf

java:
	cd ${gdir}/java && make build
	cp ${gdir}/java/jls1/extracted-readable-grammar.bgf      java.jdk-1.read.ext.bgf
	cp ${gdir}/java/jls2/extracted-readable-grammar.bgf      java.j2se-1.2.read.ext.bgf
	cp ${gdir}/java/jls3/extracted-readable-grammar.bgf      java.j2se-5.0.read.ext.bgf
	cp ${gdir}/java/jls1/extracted-implementable-grammar.bgf java.jdk-1.impl.ext.bgf
	cp ${gdir}/java/jls2/extracted-implementable-grammar.bgf java.j2se-1.2.impl.ext.bgf
	cp ${gdir}/java/jls3/extracted-implementable-grammar.bgf java.j2se-5.0.impl.ext.bgf
	cp ${gdir}/java/jls1/recovered-readable-grammar.bgf      java.jdk-1.read.rec.bgf
	cp ${gdir}/java/jls2/recovered-readable-grammar.bgf      java.j2se-1.2.read.rec.bgf
	cp ${gdir}/java/jls3/recovered-readable-grammar.bgf      java.j2se-5.0.read.rec.bgf
	cp ${gdir}/java/jls1/recovered-implementable-grammar.bgf java.jdk-1.impl.rec.bgf
	cp ${gdir}/java/jls2/recovered-implementable-grammar.bgf java.j2se-1.2.impl.rec.bgf
	cp ${gdir}/java/jls3/recovered-implementable-grammar.bgf java.j2se-5.0.impl.rec.bgf

clean:
	rm -f *~ *.bgf

test:
	ls -1 *.bgf | xargs -n1 ${tdir}/validate bgf
