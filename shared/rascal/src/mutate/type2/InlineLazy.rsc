@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{InlineLazy}
module mutate::type2::InlineLazy

import lib::Rascalware;
import analyse::Metrics;
import language::BGF;
import language::XScope;
import transform::library::Folding;
import mutate::type1::InlineMax;

BGFGrammar InlineLazy(BGFGrammar g)
{
	set[str] usedonce = {};
	set[str] usedmore = {};
	for (/nonterminal(x) := g)
	{
		if (x notin usedonce && x notin usedmore)
			usedonce += {x};
		elseif (x in usedonce)
		{
			usedonce -= {x};
			usedmore += {x};
		}
	}
	return mutate::type1::InlineMax::InlineNS(g, definedNs(g.prods) & usedonce);
}
