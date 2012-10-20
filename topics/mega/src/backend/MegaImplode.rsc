@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaImplode

import structure::MegaADT;
import structure::MegaGrammar;
import String;

AMegaModel mapmegal((MegaModel)`<MegaHeader h><MegaInclude* ins><MegaDecl* ds>`) = makemegal("<h.name>","",ins,ds);
AMegaModel mapmegal((MegaModel)`<MegaDesc d><MegaHeader h><MegaInclude* ins><MegaDecl* ds>`) = makemegal("<h.name>",trim("<d.s>"),ins,ds);
default AMegaModel mapmegal(MegaModel m) = megamodel("","",[],[],[]);

AMegaModel makemegal(str name, str desc, MegaInclude* ins, MegaDecl* ds)
	= megamodel(name,desc,collectIncludes(ins),collectDecls(ds),collectRels(ds));

list[str] collectIncludes(MegaInclude* ins) = ["<i.name>" | MegaInclude i <- ins];
list[MegaDeclaration] collectDecls(MegaDecl* ds) = [*mapdecl(d) | MegaDecl d <- ds];
list[MegaRelation] collectRels(MegaDecl* ds) = [*maprel(d) | MegaDecl d <- ds];

list[MegaDeclaration] mapdecl((MegaDecl)`<MegaModifier? mm><MegaEntity e><MegaDot d>`) = [map1decl(e,mapmod(mm),"<e.id>",getComment(d))];
//list[MegaDeclaration] mapdecl((MegaDecl)`<MegaModifier? mm><MegaEntity e><MegaDot d>`) = [map2decl(mapmod(mm),"<e.id>")];
default list[MegaDeclaration] mapdecl(MegaDecl d) = [];

list[MegaRelation] maprel((MegaDecl)`<MegaModifier? _><MegaArtifact _><MaybePlus plus><ID x><MegaBin b><ID y><MegaDot d>`) = [map1rel("<b>","<x>","<y>",getComment(d))];
list[MegaRelation] maprel((MegaDecl)`<MegaModifier? _>Function<MaybePlus plus><ID x>:<ID y>-><ID z><MegaDot d>`) = [domainOf("<y>","<x>",getComment(d)),hasRange("<x>","<z>",getComment(d))];
list[MegaRelation] maprel((MegaDecl)`<ID x><MegaBin b><ID y><MegaDot d>`) = [map1rel("<b>","<x>","<y>",getComment(d))];
list[MegaRelation] maprel((MegaDecl)`<ID x>:<ID y>-><ID z><MegaDot d>`) = [domainOf("<y>","<x>",getComment(d)),hasRange("<x>","<z>",getComment(d))];
list[MegaRelation] maprel((MegaDecl)`<ID x>(<ID y>)|-><ID z><MegaDot d>`) = [inputOf("<y>","<x>",getComment(d)),hasOutput("<x>","<z>",getComment(d))];
default list[MegaRelation] maprel(MegaDecl d) = [];

str getComment(MegaDot d) = replaceFirst("<d.c>","-- ","");

MegaMod mapmod(MegaModifier? m)
{
	switch("<m>")
	{
		case "local": return local();
		case "variable": return variable();
		default: return nomod();
	}
}
//MegaMod mapmod((MegaModifier?)`local`) = local();
//MegaMod mapmod((MegaModifier?)`variable`) = variable();
//MegaMod mapmod((MegaModifier?)``) = nomod();

bool isplus((MaybePlus)`+`) = true;
bool isplus((MaybePlus)``) = false;
default bool isplus(MaybePlus _) = false; // insurance

MegaRelation map1rel("\<", str x, str y, str comment) = subsetOf(x,y,comment);
MegaRelation map1rel("subsetOf", str x, str y, str comment) = subsetOf(x,y,comment);
MegaRelation map1rel(":", str x, str y, str comment) = elementOf(x,y,comment);
MegaRelation map1rel("elementOf", str x, str y, str comment) = elementOf(x,y,comment);
MegaRelation map1rel("@", str x, str y, str comment) = partOf(x,y,comment);
MegaRelation map1rel("partOf", str x, str y, str comment) = partOf(x,y,comment);
MegaRelation map1rel("=", str x, str y, str comment) = correspondsTo(x,y,comment);
MegaRelation map1rel("correspondsTo", str x, str y, str comment) = correspondsTo(x,y,comment);
MegaRelation map1rel("~\>", str x, str y, str comment) = refersTo(x,y,comment);
MegaRelation map1rel("dependsOn", str x, str y, str comment) = dependsOn(x,y,comment);
MegaRelation map1rel("refersTo", str x, str y, str comment) = refersTo(x,y,comment);
MegaRelation map1rel("-|", str x, str y, str comment) = conformsTo(x,y,comment);
MegaRelation map1rel("conformsTo", str x, str y, str comment) = conformsTo(x,y,comment);
MegaRelation map1rel("=\>", str x, str y, str comment) = definitionOf(x,y,comment);
MegaRelation map1rel("realizationOf", str x, str y, str comment) = realizationOf(x,y,comment);
MegaRelation map1rel("descriptionOf", str x, str y, str comment) = descriptionOf(x,y,comment);
MegaRelation map1rel("definitionOf", str x, str y, str comment) = definitionOf(x,y,comment);
MegaRelation map1rel("inputOf", str x, str y, str comment) = inputOf(x,y,comment);
MegaRelation map1rel("hasOutput", str x, str y, str comment) = hasOutput(x,y,comment);
MegaRelation map1rel("domainOf", str x, str y, str comment) = domainOf(x,y,comment);
MegaRelation map1rel("hasRange", str x, str y, str comment) = hasRange(x,y,comment);
default MegaRelation map1rel(str b, str x, str y, str comment) = subsetOf(x,y,comment); // no error report

MegaDeclaration map1decl((MegaEntity)`<MegaArtifact a><MaybePlus plus><ID x><MegaBin _><ID _>`,MegaMod m, str id, str comment)
	= map1decl((MegaEntity)`<a><plus><x>`, m, id, comment);

MegaDeclaration map1decl((MegaEntity)`<MegaArtifact a><MaybePlus plus><ID _>`,MegaMod m, str id, str comment)
{
	switch("<a>")
	{
		case "Artifact": return artifact(m,id,isplus(plus),comment);
		case "File": return file(m,id,isplus(plus),comment);
		case "Language": return language(m,id,isplus(plus),comment);
		case "Technology": return technology(m,id,isplus(plus),comment);
		case "Fragment": return fragment(m,id,isplus(plus),comment);
		case "FunctionApp": return functionapp(m,id,isplus(plus),comment);
		case "ObjectGraph": return objectGraph(m,id,isplus(plus),comment);
		case "Program": return program(m,id,isplus(plus),comment);
		case "Library": return library(m,id,isplus(plus),comment);
		default: println("ERROR in <a>.");
	}
}
MegaDeclaration map1decl((MegaEntity)`Function<MaybePlus plus><ID _><MegaFun? _>`,MegaMod m, str id, str comment) = function(m,id,isplus(plus),comment); //"
default MegaDeclaration map1decl(MegaEntity e,MegaMod m, str id) = println("ERROR in <e>");
