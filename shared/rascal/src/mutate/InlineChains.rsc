@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{UnchainAll}
module mutate::InlineChains

import syntax::BGF;

public BGFGrammar unchainAll(BGFGrammar g)
{
	ps = g.prods;
	for (tp:production(_,_,nonterminal(chained)) <- [p | p:production(_,_,nonterminal(x)) <- g.prods])
	{
		myprods = [bp | bp:production(_,chained,_) <- ps];
		if ([production(_,_,rhs)] := myprods)
			ps = ps + production(tp.label,tp.lhs,rhs) - myprods - tp;
	}
	return grammar(g.roots,ps);
}
