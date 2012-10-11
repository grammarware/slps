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

str exportdecl(MegaDeclaration d) = "<exportmod(d.m)><backend::MegaHack::nameOf(d)><if(d.plus){>+<}> <d.id><displayDot(d.comment)>";

str displayDot("") = " .";
default str displayDot(str c) = " . -- <c>";

str exportmod(local()) = "local ";
str exportmod(variable()) = "variable "; 
str exportmod(MegaMod m) = "";

str exportrel(MegaRelation r) = "<r.x> <backend::MegaHack::nameOf(r)> <r.y><displayDot(r.comment)>";

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