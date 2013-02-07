@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RetireSs}
module mutate::type2::RetireSs

import syntax::BGF;
import normal::BGF;

BGFGrammar RetireSs(BGFGrammar g)
{
	ps = visit(g.prods) {case selectable(_,BGFExpression e) => e};
	return grammar (g.roots, normalise(g.prods));
}
