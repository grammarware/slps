@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{Inline chains}
module mutate::InlineChains

import IO;
import syntax::BGF;
import export::BNF;

public BGFGrammar unchain_m(BGFGrammar g)
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
