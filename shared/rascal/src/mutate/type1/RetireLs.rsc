@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RetireLs}
module mutate::type1::RetireLs

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Util;

BGFGrammar RetireLs(BGFGrammar g)
{
	BGFProdList ps = [];
	for (p <- g.prods)
		if (isEmpty(p.label))
			ps += [p];
		else
			ps += [production("", p.lhs, p.rhs)];
	return grammar(g.roots, g.prods);
}
