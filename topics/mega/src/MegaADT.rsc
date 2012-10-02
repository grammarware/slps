@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaADT

data AMegaModel = megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels);
data MegaDeclaration
	= artifact(MegaMod m, str id, bool plus)
	| file(MegaMod m, str id, bool plus)
	| language(MegaMod m, str id, bool plus)
	| technology(MegaMod m, str id, bool plus)
	| fragment(MegaMod m, str id, bool plus)
	| objectgraph(MegaMod m, str id, bool plus)
	| program(MegaMod m, str id, bool plus)
	| library(MegaMod m, str id, bool plus)
	| function(MegaMod m, str id, bool plus)
	;
data MegaMod = local() | variable() | nomod();
data MegaRelation
	= subsetOf(str x, str y)
    | elementOf(str x, str y)
    | partOf(str x, str y)
    | correspondsTo(str x, str y)
    | dependsOn(str x, str y)
	| refersTo(str x, str y)
    | conformsTo(str x, str y)
    | realizationOf(str x, str y)
	| descriptionOf(str x, str y)
	| definitionOf(str x, str y)
	| mapsTo(str f, str x, str y)
	;
