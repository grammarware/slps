@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Complete

import structure::MegaADT;
import structure::MegaGrammar;
import String;
import List;

import io::MegaL;
import IO;

public void main()
{
	AMegaModel ast = readAST(|project://megal/examples/guided.megal|);
	list[MegaDeclaration] ddecls = [];
	list[MegaRelation] drels = [];
	<ast,ddecls,drels> = complete(ast);
	writeDot(|project://megal/examples/guided.dot|,ast,ddecls,drels);
}

public AMegaModel completed(AMegaModel m)
{
	<ast,ddecls,drels> = complete(m);
	if (megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels) := ast)
		return megamodel(name, desc, incs, decls+ddecls, rels+drels);
	else
		// should never happen
		return ast;
}

public tuple[AMegaModel,list[MegaDeclaration],list[MegaRelation]] complete(AMegaModel m)
{
	list[MegaDeclaration] ds = [];
	list[MegaRelation] rs = [];
	int cx = 0;
	// AMegaModel = megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels);
	rels = toSet(m.rels);
	decls = toSet(m.decls);
	for(f <- {y | r <- m.rels, inputOf(str x, str y, str comment) := r})
		if ([M1*,hasOutput(f, str z, str comment),M2*] !:= m.rels)
		{
			println("Found a function <f> with no output!");
			cx += 1;
			rs += hasOutput(f, "OUT<cx>", "generated");
			ds += artifact(nomod(), "OUT<cx>", false, "generated");
		}
	for(f <- {x | r <- m.rels, hasOutput(str x, str y, str comment) := r})
		if ([M1*,inputOf(str z, f, str comment),M2*] !:= m.rels)
		{
			println("Found a function <f> with no input!");
			cx += 1;
			rs += inputOf("IN<cx>", f, "generated");
			ds += artifact(nomod(), "IN<cx>", false, "generated");
		}
	//for(f <- {id | d <- m.decls, functionapp(MegaMod m, str id, bool plus, str comment) := d})
	////for({functionapp(MegaMod _, str f, bool _, str _), D*} := decls)
	//	if ([M1*,elementOf(f, str y, str _),M2*] !:= m.rels || [M3*,function(MegaMod m, y, bool _, str _),M4*] !:= m.decls)
	//	{
	//		println("Found a function application <f> without a base function!");
	//		cx += 1;
	//		rs += elementOf(f,"FUN<cx>","generated");
	//		ds += function(nomod(), "FUN<cx>", false, "generated");
	//	}
	for([M1*,inputOf(str x, str y, str comment),M2*,hasOutput(y, str z, str _),M3*] := m.rels)
	{
		str f = "", fun = "";
		if([M4*,functionapp(MegaMod m, y, bool _, str _),M5*] := m.decls)
			f = y;
		else
		{
			println("Found a missing function application declaration of <y>.");
			cx += 1;
			f = "FUN<cx>";
			ds += function(nomod(), f, false, "generated");
		}
		if ([M6*,elementOf(f, str a, str _),M7*] := m.rels)
			fun = a;
		else//if([M8*,function(MegaMod m, y, bool _, str _),M4*] !:= m.decls)
		{
			fun = "FUN<cx>";
			println("Found a function application <f> without a base function!");
			cx += 1;
			rs += elementOf(f,fun,"generated");
			ds += function(nomod(), fun, false, "generated");
		}
		//for([M1*,inputOf(str x, str y, str comment),M2*,hasOutput(y, str z, str _),M3*] := m.rels)
	}
	return <m,ds,rs>;
}
