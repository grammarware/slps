@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{UnfoldMax}
module mutate::type1::UnfoldMax

import lib::Rascalware;
import language::BGF;
import language::XScope;
import transform::library::Folding;

BGFGrammar UnfoldMax(BGFGrammar g)
{
	ns = definedNs(g.prods);
	bool done = False
	while(True)
	{
		if (isEmpty(ns))
			break;
		x,ns = takeOneFrom(ns);
		if (<ps1,[production(str l, x, BGFExpression rhs)],ps2> := splitPbyW(g.prods,innt(x)) && /nonterminal(x) !:= rhs)
		{
			g = transform::library::Folding::runUnfold(x,globally(),g);
			ns = definedNs(g);
		}
	}
	return g;
}
