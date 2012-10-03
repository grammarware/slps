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
	'<for(MegaDeclaration d <- decls){><exportdeclR(d,rels)> .
	'<}>
	'<for(MegaRelation r <- rels, r notin processed){><exportrelR(r,rels)> .
	'<}>";

str exportdecl(artifact(MegaMod m, str id, bool plus)) = "<exportmod(m)>Artifact<if(plus){>+<}> <id>";
str exportdecl(file(MegaMod m, str id, bool plus)) = "<exportmod(m)>File<if(plus){>+<}> <id>";
str exportdecl(language(MegaMod m, str id, bool plus)) = "<exportmod(m)>Language<if(plus){>+<}> <id>";
str exportdecl(technology(MegaMod m, str id, bool plus)) = "<exportmod(m)>Technology<if(plus){>+<}> <id>";
str exportdecl(fragment(MegaMod m, str id, bool plus)) = "<exportmod(m)>Fragment<if(plus){>+<}> <id>";
str exportdecl(objectGraph(MegaMod m, str id, bool plus)) = "<exportmod(m)>ObjectGraph<if(plus){>+<}> <id>";
str exportdecl(program(MegaMod m, str id, bool plus)) = "<exportmod(m)>Program<if(plus){>+<}> <id>";
str exportdecl(library(MegaMod m, str id, bool plus)) = "<exportmod(m)>Library<if(plus){>+<}> <id>";
str exportdecl(function(MegaMod m, str id, bool plus)) = "<exportmod(m)>Function<if(plus){>+<}> <id>";
default str exportdecl(MegaDeclaration d) = "UNKNOWN DECL";

str exportmod(local()) = "local ";
str exportmod(variable()) = "variable "; 
str exportmod(MegaMod m) = "";

str exportrel(subsetOf(str x, str y)) = "<x> subsetOf <y>";
str exportrel(elementOf(str x, str y)) = "<x> elementOf <y>";
str exportrel(partOf(str x, str y)) = "<x> partOf <y>";
str exportrel(correspondsTo(str x, str y)) = "<x> correspondsTo <y>";
str exportrel(dependsOn(str x, str y)) = "<x> dependsOn <y>";
str exportrel(refersTo(str x, str y)) = "<x> refersTo <y>";
str exportrel(conformsTo(str x, str y)) = "<x> conformsTo <y>";
str exportrel(realizationOf(str x, str y)) = "<x> realizationOf <y>";
str exportrel(descriptionOf(str x, str y)) = "<x> descriptionOf <y>";
str exportrel(definitionOf(str x, str y)) = "<x> definitionOf <y>";
str exportrel(inputOf(str x, str y)) = "<x> inputOf <y>";
str exportrel(hasOutput(str x, str y)) = "<x> hasOutput <y>";
str exportrel(domainOf(str x, str y)) = "<x> domainOf <y>";
str exportrel(hasRange(str x, str y)) = "<x> hasRange <y>";
default str exportrel(MegaRelation r) = "UNKNOWN REL";

str exportdeclR(MegaDeclaration d, list[MegaRelation] rels)
{
	str s = "";
	for (r <- rels, r.x == d.id)
		if (domainOf(a,b) := r, /hasRange(b,str c) := rels)
		{
			s = " : <a> -\> <c>";
			processed += r;
			processed += hasRange(b,c);
			break;
		}
		elseif (hasRange(a,b) := r, /domainOf(c,a) := rels)
		{
			s = " : <c> -\> <b>";
			processed += r;
			processed += domainOf(c,a);
			break;
		}
		else
		{
			s = replaceFirst(exportrel(r),"<r.x> "," ");
			processed += r;
			break;
		}
	return exportdecl(d)+s;
}

str exportrelR(MegaRelation r, list[MegaRelation] rels)
{
	if (inputOf(str a, str b) := r, /hasOutput(b,str c) := rels)
	{
		processed += r;
		processed += hasOutput(b,c);
		return "<b>(<a>) |-\> <c>";
	}
	elseif (hasOutput(a,b) := r, /inputOf(c,a) := rels)
	{
		processed += r;
		processed += inputOf(c,a);
		return "<a>(<c>) |-\> <b>";
	}
	else
		return exportrel(r);
}