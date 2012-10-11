@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaDot

import backend::MegaHack;
import structure::MegaADT;
import structure::MegaGrammar;
import String;

import io::MegaL;
import IO;

public void main()
{
	ast = readAST(|project://megal/examples/guided.megal|);
	writeFile(|project://megal/examples/guided.dot|,exportmega(ast));
}

str exportmega(AMegaModel a) = exportmega(a,[],[]);

str exportmega(megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels), list[MegaDeclaration] ddecls, list[MegaRelation] drels)
	= "digraph G {
	'<for(MegaDeclaration d <- decls, d notin ddecls){><exportdecl(d,"solid")>
	'<}>
	'<for(MegaDeclaration d <- ddecls){><exportdecl(d,"dashed")>
	'<}>
	'<for(MegaRelation r <- rels, r notin drels){><exportrel(r,"solid")>
	'<}>
	'<for(MegaRelation r <- drels){><exportrel(r,"dashed")>
	'<}>
	'}";

str exportdecl(artifact(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"blue",line)>";
str exportdecl(file(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"indigo",line)>";
str exportdecl(language(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"yellow",line)>";
str exportdecl(technology(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"gray",line)>";
str exportdecl(fragment(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"darkslateblue",line)>";
str exportdecl(objectGraph(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"white",line)>";
str exportdecl(program(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"blue",line)>";
str exportdecl(library(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"blue",line)>";
str exportdecl(function(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"green",line)>";
str exportdecl(functionapp(MegaMod m, str id, _, str comment), str line) = "<exportmod(m,id,"darkgreen",line)>";
default str exportdecl(MegaDeclaration _, str _) = "UNKNOWN DECL <d>";

str exportmod(local(),str id, str col, str line) = makenode(id,col,line,"Times-Bold");
str exportmod(variable(),str id, str col, str line) = makenode(id,col,line,"Times-Italic");
str exportmod(nomod(),str id,str col, str line) = makenode(id,col,line,"Times");

str makenode(str id, str col, str style, str font) = "\"<id>\" [label=\"<id>\",shape=box,style=\"filled,<style>\",fillcolor=\"<col>\"<fliptextcolor(col)>,fontname=\"<font>\"];";

str fliptextcolor("blue") = ",fontcolor=\"white\"";
str fliptextcolor("indigo") = ",fontcolor=\"white\"";
str fliptextcolor("darkgreen") = ",fontcolor=\"white\"";
str fliptextcolor("darkslateblue") = ",fontcolor=\"white\"";
default str fliptextcolor(str c) = "";

str exportrel(MegaRelation r, str line) = makeedge(r.x,backend::MegaHack::nameOf(r),r.y,line);

str makeedge(str x, str f, str y, str line) = "<x> -\> <y> [label=\"<f>\",style=\"<line>\"];";
