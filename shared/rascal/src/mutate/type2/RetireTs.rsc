@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RetireTs}
module mutate::type2::RetireTs

import syntax::BGF;
import normal::BGF;

BGFGrammar RetireTs(BGFGrammar g)
{
	ps = visit(g.prods) {case terminal(_) => epsilon()};
	return grammar (g.roots, normalise(g.prods));
}
