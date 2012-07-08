@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::CarveOut

import syntax::BGF;
import syntax::CBGF;
import analyse::Metrics;
import lib::Rascalware;

public CBGFSequence carveOutN(str n, BGFGrammar g)
{
	CBGFSequence c = [];
	ps = prodsOfN(n,g.prods);
	if(!isEmpty(ps))
		if (n in usedNs(g))
			c += undefine_define(ps);
		else
			c += eliminate_introduce(ps);
	for (p <- g.prods, p.lhs != n)
		if (/nonterminal(n) := p.rhs)
			c += project_inject(marknt(n,p));
	return c;
}

BGFProduction marknt(str n, BGFProduction p)
	= visit(p) { case nonterminal(n) => marked(nonterminal(n)) };
