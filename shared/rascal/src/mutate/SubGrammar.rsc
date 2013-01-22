@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{SubGrammar}
module mutate::SubGrammar

import lib::Rascalware;
import syntax::BGF;
import transform::library::Util;

public BGFGrammar subgrammar(BGFGrammar g) = subgrammar(g,g.roots); 

public BGFGrammar subgrammar(BGFGrammar g, list[str] roots)
{
	gr = grammar([],[]);
	for (root <- roots)
	{
		g2 = subgrammar(g,root);
		gr.roots += g2.roots;
		gr.prods += [p | p <- g2.prods, !inProds(p,gr.prods)];
	}
	return normalise(gr);
}

public BGFGrammar subgrammar(BGFGrammar g, str root)
{
	BGFProdList ps = [];
	set[str] covered = {}, uncovered = {root};
	while(!isEmpty(uncovered))
	for(str nt <- uncovered)
	{
		newps = [p | p:production(_,nt,_) <- g.prods];
		newnts = {n | /nonterminal(n) := newps};
		ps += newps;
		uncovered += (newnts - covered);
		uncovered -= nt;
		covered += nt;
		//println("Covered: <covered>\nUncovered: <uncovered>");
	}
	return grammar([root],ps);
}
