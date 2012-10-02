@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaImplode

import MegaADT;
import MegaGrammar;
import IO;
import ParseTree;
import String;

public void main() = main([]);

public void main(list[str] argv)
{
	// iprintln(mapmegal(parse(#MegaModel,trim(readFile(|cwd:///../tests/technology.megal|)))));
	iprintln(mapmegal(parse(#MegaModel,trim(readFile(|cwd:///../tests/read.megal|)))));
}

AMegaModel mapmegal((MegaModel)`<MegaHeader h><MegaInclude* ins><MegaDecl+ ds>`) = makemegal("<h.name>","",ins,ds);
AMegaModel mapmegal((MegaModel)`<MegaDesc d><MegaHeader h><MegaInclude* ins><MegaDecl+ ds>`) = makemegal("<h.name>",trim("<d.s>"),ins,ds);
default AMegaModel mapmegal(MegaModel m) = megamodel("","",[],[],[]);

AMegaModel makemegal(str name, str desc, MegaInclude* ins, MegaDecl+ ds)
	= megamodel(name,desc,collectIncludes(ins),collectDecls(ds),collectRels(ds));

list[str] collectIncludes(MegaInclude* ins) = ["<i.name>" | MegaInclude i <- ins];
// list[str] collectIncludes(MegaInclude* ins) = [];
list[MegaDeclaration] collectDecls(MegaDecl+ ds) = [];
list[MegaRelation] collectRels(MegaDecl+ ds) = [];

// data AMegaModel = megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels);
// data MegaDeclaration
// 	= artifact(MegaMod m, str id, bool plus)
// 	| file(MegaMod m, str id, bool plus)
// 	| language(MegaMod m, str id, bool plus)
// 	| technology(MegaMod m, str id, bool plus)
// 	| fragment(MegaMod m, str id, bool plus)
// 	| objectgraph(MegaMod m, str id, bool plus)
// 	| program(MegaMod m, str id, bool plus)
// 	| library(MegaMod m, str id, bool plus)
// 	| function(MegaMod m, str id, bool plus)
// 	;
// data MegaMod = local() | variable() | nomod();
// data MegaRelation
// 	= subsetOf(str x, str y)
//     | elementOf(str x, str y)
//     | partOf(str x, str y)
//     | correspondsTo(str x, str y)
//     | dependsOn(str x, str y)
// 	| refersTo(str x, str y)
//     | conformsTo(str x, str y)
//     | realizationOf(str x, str y)
// 	| descriptionOf(str x, str y)
// 	| definitionOf(str x, str y)
// 	| mapsTo(str f, str x, str y)
// 	;
