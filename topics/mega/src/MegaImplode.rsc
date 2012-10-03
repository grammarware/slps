@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module MegaImplode

import structure::MegaADT;
import structure::MegaGrammar;
import IO;
import ParseTree;
import String;

public void main() = main([]);

public void main(list[str] argv)
{
	// iprintln(mapmegal(parse(#MegaModel,trim(readFile(|cwd:///../tests/technology.megal|)))));
	//iprintln(mapmegal(parse(#MegaModel,trim(readFile(|cwd:///../tests/read.megal|)))));
	iprintln(mapmegal(parse(#MegaModel,trim(readFile(|project://megal/tests/deserialization.megal|)))));
}

AMegaModel mapmegal((MegaModel)`<MegaHeader h><MegaInclude* ins><MegaDecl+ ds>`) = makemegal("<h.name>","",ins,ds);
AMegaModel mapmegal((MegaModel)`<MegaDesc d><MegaHeader h><MegaInclude* ins><MegaDecl+ ds>`) = makemegal("<h.name>",trim("<d.s>"),ins,ds);
default AMegaModel mapmegal(MegaModel m) = megamodel("","",[],[],[]);

AMegaModel makemegal(str name, str desc, MegaInclude* ins, MegaDecl+ ds)
	= megamodel(name,desc,collectIncludes(ins),collectDecls(ds),collectRels(ds));

list[str] collectIncludes(MegaInclude* ins) = ["<i.name>" | MegaInclude i <- ins];
list[MegaDeclaration] collectDecls(MegaDecl+ ds) = [*mapdecl(d) | MegaDecl d <- ds];
list[MegaRelation] collectRels(MegaDecl+ ds) = [*maprel(d) | MegaDecl d <- ds];

list[MegaDeclaration] mapdecl((MegaDecl)`<MegaModifier? mm><MegaEntity e><MegaDot d>`) = [map1decl(e,mapmod(mm),"<e.id>")];
//list[MegaDeclaration] mapdecl((MegaDecl)`<MegaModifier? mm><MegaEntity e><MegaDot d>`) = [map2decl(mapmod(mm),"<e.id>")];
default list[MegaDeclaration] mapdecl(MegaDecl d) = [];

list[MegaRelation] maprel((MegaDecl)`<MegaModifier? _><MegaArtifact _><MaybePlus plus><ID x><MegaBin b><ID y><MegaDot d>`) = [map1rel("<b>","<x>","<y>")];
list[MegaRelation] maprel((MegaDecl)`<MegaModifier? _>Function<MaybePlus plus><ID x>:<ID y>-><ID z><MegaDot d>`) = [domainOf("<y>","<x>"),hasRange("<x>","<z>")];
list[MegaRelation] maprel((MegaDecl)`<ID x><MegaBin b><ID y><MegaDot d>`) = [map1rel("<b>","<x>","<y>")];
list[MegaRelation] maprel((MegaDecl)`<ID x>:<ID y>-><ID z><MegaDot d>`) = [domainOf("<y>","<x>"),hasRange("<x>","<z>")];
list[MegaRelation] maprel((MegaDecl)`<ID x>(<ID y>)|-><ID z><MegaDot d>`) = [inputOf("<y>","<x>"),hasOutput("<x>","<z>")];
default list[MegaRelation] maprel(MegaDecl d) = [];

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

MegaRelation map1rel("\<", str x, str y) = subsetOf(x,y);
MegaRelation map1rel("subsetOf", str x, str y) = subsetOf(x,y);
MegaRelation map1rel(":", str x, str y) = elementOf(x,y);
MegaRelation map1rel("elementOf", str x, str y) = elementOf(x,y);
MegaRelation map1rel("@", str x, str y) = partOf(x,y);
MegaRelation map1rel("partOf", str x, str y) = partOf(x,y);
MegaRelation map1rel("=", str x, str y) = correspondsTo(x,y);
MegaRelation map1rel("correspondsTo", str x, str y) = correspondsTo(x,y);
MegaRelation map1rel("~\>", str x, str y) = refersTo(x,y);
MegaRelation map1rel("dependsOn", str x, str y) = dependsOn(x,y);
MegaRelation map1rel("refersTo", str x, str y) = refersTo(x,y);
MegaRelation map1rel("-|", str x, str y) = conformsTo(x,y);
MegaRelation map1rel("conformsTo", str x, str y) = conformsTo(x,y);
MegaRelation map1rel("=\>", str x, str y) = definitionOf(x,y);
MegaRelation map1rel("realizationOf", str x, str y) = realizationOf(x,y);
MegaRelation map1rel("descriptionOf", str x, str y) = descriptionOf(x,y);
MegaRelation map1rel("definitionOf", str x, str y) = definitionOf(x,y);
default MegaRelation map1rel(str b, str x, str y) = subsetOf(x,y); // no error report

MegaDeclaration map1decl((MegaEntity)`<MegaArtifact a><MaybePlus plus><ID x><MegaBin _><ID _>`,MegaMod m, str id)
	= map1decl((MegaEntity)`<a><plus><x>`, m, id);

MegaDeclaration map1decl((MegaEntity)`<MegaArtifact a><MaybePlus plus><ID _>`,MegaMod m, str id)
{
	switch("<a>")
	{
		case "Artifact": return artifact(m,id,isplus(plus));
		case "File": return file(m,id,isplus(plus));
		case "Language": return language(m,id,isplus(plus));
		case "Technology": return technology(m,id,isplus(plus));
		case "Fragment": return fragment(m,id,isplus(plus));
		case "ObjectGraph": return objectGraph(m,id,isplus(plus));
		case "Program": return program(m,id,isplus(plus));
		case "Library": return library(m,id,isplus(plus));
		default: println("ERROR in <a>.");
	}
}
MegaDeclaration map1decl((MegaEntity)`Function<MaybePlus plus><ID _><MegaFun? _>`,MegaMod m, str id) = function(m,id,isplus(plus)); //"
default MegaDeclaration map1decl(MegaEntity e,MegaMod m, str id) = println("ERROR in <e>");
