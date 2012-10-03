@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module structure::MegaADT

data AMegaModel = megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels);
data MegaDeclaration
	= artifact(MegaMod m, str id, bool plus, str comment)
	| file(MegaMod m, str id, bool plus, str comment)
	| language(MegaMod m, str id, bool plus, str comment)
	| technology(MegaMod m, str id, bool plus, str comment)
	| fragment(MegaMod m, str id, bool plus, str comment)
	| objectGraph(MegaMod m, str id, bool plus, str comment)
	| program(MegaMod m, str id, bool plus, str comment)
	| library(MegaMod m, str id, bool plus, str comment)
	| function(MegaMod m, str id, bool plus, str comment)
	;
data MegaMod = local() | variable() | nomod();
data MegaRelation
	= subsetOf(str x, str y, str comment)
	| elementOf(str x, str y, str comment)
	| partOf(str x, str y, str comment)
	| correspondsTo(str x, str y, str comment)
	| dependsOn(str x, str y, str comment)
	| refersTo(str x, str y, str comment)
	| conformsTo(str x, str y, str comment)
	| realizationOf(str x, str y, str comment)
	| descriptionOf(str x, str y, str comment)
	| definitionOf(str x, str y, str comment)
	| inputOf(str x, str y, str comment)
	| hasOutput(str x, str y, str comment)
	| domainOf(str x, str y, str comment)
	| hasRange(str x, str y, str comment)
	;
