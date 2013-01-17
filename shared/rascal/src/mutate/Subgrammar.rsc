@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module mutate::Subgrammar

import lib::Rascalware;
import syntax::BGF;
import transform::library::Util;

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
