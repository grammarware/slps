@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaDot

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

str exportmega(megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels))
	="digraph G {
	'<for(MegaDeclaration d <- decls){><exportdecl(d)>
	'<}>
	'<for(MegaRelation r <- rels){><exportrel(r)>
	'<}>
	'}";

str exportdecl(artifact(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"blue")>";
str exportdecl(file(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"indigo")>";
str exportdecl(language(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"yellow")>";
str exportdecl(technology(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"gray")>";
str exportdecl(fragment(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"darkslateblue")>";
str exportdecl(objectGraph(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"white")>";
str exportdecl(program(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"blue")>";
str exportdecl(library(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"blue")>";
str exportdecl(function(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"green")>";
str exportdecl(functionapp(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m,id,"darkgreen")>";
default str exportdecl(MegaDeclaration d) = "UNKNOWN DECL <d>";

str exportmod(local(),str id, str col) = makenode(id,col,"dashed");
str exportmod(variable(),str id, str col) = makenode(id,col,"dotted");
str exportmod(nomod(),str id,str col) = makenode(id,col,"solid");

str makenode(str id, str col, str style) = "\"<id>\" [label=\"<id>\",shape=box,style=\"filled,<style>\",fillcolor=\"<col>\"<fliptextcolor(col)>];";

str fliptextcolor("blue") = ",fontcolor=\"white\"";
str fliptextcolor("darkgreen") = ",fontcolor=\"white\"";
default str fliptextcolor(str c) = "";

str exportrel(subsetOf(str x, str y, str comment)) = makeedge(x,"subsetOf",y);
str exportrel(elementOf(str x, str y, str comment)) = makeedge(x,"elementOf",y);
str exportrel(partOf(str x, str y, str comment)) = makeedge(x,"partOf",y);
str exportrel(correspondsTo(str x, str y, str comment)) = makeedge(x,"correspondsTo",y);
str exportrel(dependsOn(str x, str y, str comment)) = makeedge(x,"dependsOn",y);
str exportrel(refersTo(str x, str y, str comment)) = makeedge(x,"refersTo",y);
str exportrel(conformsTo(str x, str y, str comment)) = makeedge(x,"conformsTo",y);
str exportrel(realizationOf(str x, str y, str comment)) = makeedge(x,"realizationOf",y);
str exportrel(descriptionOf(str x, str y, str comment)) = makeedge(x,"descriptionOf",y);
str exportrel(definitionOf(str x, str y, str comment)) = makeedge(x,"definitionOf",y);
str exportrel(inputOf(str x, str y, str comment)) = makeedge(x,"inputOf",y);
str exportrel(hasOutput(str x, str y, str comment)) = makeedge(x,"hasOutput",y);
str exportrel(domainOf(str x, str y, str comment)) = makeedge(x,"domainOf",y);
str exportrel(hasRange(str x, str y, str comment)) = makeedge(x,"hasRange",y);
default str exportrel(MegaRelation r) = "UNKNOWN REL";

str makeedge(str x, str f, str y) = "<x> -\> <y> [label=\"<f>\"];";
