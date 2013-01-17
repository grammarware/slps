@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module mutate::Subgrammar

import syntax::BGF;
import transform::library::Util;

public BGFGrammar subgrammar(BGFGrammar g, str root)
{
	BGFProdList ps = [];
	set[str] covered = {}, uncovered = {root};
	for(str nt <- uncovered)
	{
		newps = [p | p:production(_,nt,_) <- g.prods];
		newnts = {n | /nonterminal(n) := newps};
		ps += newps;
		covered += newnts;
		uncovered -= nt;
	}
	return grammar([root],ps);
}
