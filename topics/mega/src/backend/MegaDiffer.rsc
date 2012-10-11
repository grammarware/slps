@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaDiffer

import structure::MegaADT;
import backend::MegaHack;

import List;
import IO;

// TODO: everything.

public bool megadiff(AMegaModel m1, AMegaModel m2)
	{
		for (d <- m1.decls, d notin m2.decls)
			println("Left megamodel has entity <d.id>");
		for (d <- m2.decls, d notin m1.decls)
			println("Right megamodel has entity <d.id>");
		for (r <- m1.rels, r notin m2.rels)
			println("Left megamodel has relationship <r>");
		for (r <- m2.rels, r notin m1.rels)
			println("Right megamodel has entity <r>");
		return toSet(m1.decls)==toSet(m2.decls) && toSet(m1.rels)==toSet(m2.rels) && m1.name==m2.name;
	}

public bool megadiffnonames(AMegaModel m1, AMegaModel m2)
	{
		map[str,str] typeOf1 = (), typeOf2 = ();
		set[tuple[str,str,str]] rdf1 = {}, rdf2 = {};
		list[MegaRelation] rs = m2.rels;
		for (d <- m1.decls)
			typeOf1[d.id] = backend::MegaHack::nameOf(d);
		for (d <- m2.decls)
			typeOf2[d.id] = backend::MegaHack::nameOf(d);
		for (r <- m1.rels)
			rdf1 += <typeOf1[r.x],backend::MegaHack::nameOf(r),typeOf1[r.y]>;
		for (r <- m2.rels)
			rdf2 += <typeOf2[r.x],backend::MegaHack::nameOf(r),typeOf2[r.y]>;
		//for (r <- m1.rels, r notin m2.rels)
		//	println("Left megamodel has relationship <r>");
		//for (r <- m2.rels, r notin m1.rels)
		//	println("Right megamodel has entity <r>");
		return rdf1==rdf2;
	}
