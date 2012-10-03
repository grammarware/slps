@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module backend::MegaExport

import structure::MegaADT;
import structure::MegaGrammar;
import String;

list[MegaRelation] processed = [];

str exportmega(megamodel(str name, str desc, list[str] incs, list[MegaDeclaration] decls, list[MegaRelation] rels))
	="{- <desc> -}
	'
	'megamodel <name> .
	'
	'<for(str i <- incs){>include <i> .
	'<}>
	'<for(MegaDeclaration d <- decls){><exportdeclR(d,rels)>
	'<}>
	'<for(MegaRelation r <- rels, r notin processed){><exportrelR(r,rels)>
	'<}>";

str exportdecl(artifact(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Artifact<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(file(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>File<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(language(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Language<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(technology(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Technology<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(fragment(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Fragment<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(objectGraph(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>ObjectGraph<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(program(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Program<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(library(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Library<if(plus){>+<}> <id><displayDot(comment)>";
str exportdecl(function(MegaMod m, str id, bool plus, str comment)) = "<exportmod(m)>Function<if(plus){>+<}> <id><displayDot(comment)>";
default str exportdecl(MegaDeclaration d) = "UNKNOWN DECL";

str displayDot("") = " .";
default str displayDot(str c) = " . -- <c>";

str exportmod(local()) = "local ";
str exportmod(variable()) = "variable "; 
str exportmod(MegaMod m) = "";

str exportrel(subsetOf(str x, str y, str comment)) = "<x> subsetOf <y><displayDot(comment)>";
str exportrel(elementOf(str x, str y, str comment)) = "<x> elementOf <y><displayDot(comment)>";
str exportrel(partOf(str x, str y, str comment)) = "<x> partOf <y><displayDot(comment)>";
str exportrel(correspondsTo(str x, str y, str comment)) = "<x> correspondsTo <y><displayDot(comment)>";
str exportrel(dependsOn(str x, str y, str comment)) = "<x> dependsOn <y><displayDot(comment)>";
str exportrel(refersTo(str x, str y, str comment)) = "<x> refersTo <y><displayDot(comment)>";
str exportrel(conformsTo(str x, str y, str comment)) = "<x> conformsTo <y><displayDot(comment)>";
str exportrel(realizationOf(str x, str y, str comment)) = "<x> realizationOf <y><displayDot(comment)>";
str exportrel(descriptionOf(str x, str y, str comment)) = "<x> descriptionOf <y><displayDot(comment)>";
str exportrel(definitionOf(str x, str y, str comment)) = "<x> definitionOf <y><displayDot(comment)>";
str exportrel(inputOf(str x, str y, str comment)) = "<x> inputOf <y><displayDot(comment)>";
str exportrel(hasOutput(str x, str y, str comment)) = "<x> hasOutput <y><displayDot(comment)>";
str exportrel(domainOf(str x, str y, str comment)) = "<x> domainOf <y><displayDot(comment)>";
str exportrel(hasRange(str x, str y, str comment)) = "<x> hasRange <y><displayDot(comment)>";
default str exportrel(MegaRelation r) = "UNKNOWN REL";

str exportdeclR(MegaDeclaration d, list[MegaRelation] rels)
{
	str s = "";
	if (d.comment != "") return exportdecl(d); // if there is a comment, we need to stand on our own
	for (r <- rels, r.x == d.id)
		if (domainOf(a,b,_) := r, /hasRange(b,str c,dot) := rels)
		{
			s = " : <a> -\> <c><displayDot(dot)>";
			processed += r;
			processed += hasRange(b,c,dot);
			break;
		}
		elseif (hasRange(a,b,_) := r, /domainOf(c,a,dot) := rels)
		{
			s = " : <c> -\> <b><displayDot(dot)>";
			processed += r;
			processed += domainOf(c,a,dot);
			break;
		}
		else
		{
			s = exportrel(r);
			processed += r;
			break;
		}
	if (s=="")
		return exportdecl(d);
	else
		return replaceFirst(exportdecl(d)," .","")+replaceFirst(s,"<d.id>","");
}

str exportrelR(MegaRelation r, list[MegaRelation] rels)
{
	if (inputOf(str a, str b,_) := r, /hasOutput(b,str c, str d) := rels)
	{
		processed += r;
		processed += hasOutput(b,c,d);
		return "<b>(<a>) |-\> <c><displayDot(d)>";
	}
	elseif (hasOutput(a,b,_) := r, /inputOf(c,a,d) := rels)
	{
		processed += r;
		processed += inputOf(c,a,d);
		return "<a>(<c>) |-\> <b><displayDot(d)>";
	}
	else
		return exportrel(r);
}