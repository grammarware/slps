@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{InlineMax}
module mutate::type1::InlineMax

import lib::Rascalware;
import language::BGF;
import language::XScope;
import transform::library::Folding;

BGFGrammar InlineMax(BGFGrammar g) = InlineNS(g,definedNs(g.prods));

// TODO: move to Type IV?
BGFGrammar InlineNS(BGFGrammar g, set[str] ns)
{
	bool done = False
	while(True)
	{
		if (isEmpty(ns))
			break;
		x,ns = takeOneFrom(ns);
		if (<ps1,[production(str l, x, BGFExpression rhs)],ps2> := splitPbyW(g.prods,innt(x)) && /nonterminal(x) !:= rhs)
		{
			g = transform::library::Folding::runInline(x,g);
			ns = definedNs(g);
		}
	}
	return g;
}
