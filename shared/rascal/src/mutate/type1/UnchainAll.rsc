@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{UnchainAll}
module mutate::type1::UnchainAll

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::library::Chaining;

BGFGrammar UnchainAll(BGFGrammar g)
{
	for (p <- g.prods, nonterminal(y) := p.rhs, y != p.lhs)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods, innt(y));
		if (len(ps2)==1)
			g = transform::library::Chaining::runUnchain(p,g);
	}
	return g;
}
