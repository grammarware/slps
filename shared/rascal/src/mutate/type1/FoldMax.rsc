@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{FoldMax}
module mutate::type1::FoldMax

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::Results;
import transform::library::Folding;

BGFGrammar FoldMax(BGFGrammar g)
{
	for (str x <- definedNs(g.prods))
	{
		res = transform::library::Folding::runFold(x,globally(),g);
		if (ok():=res) g = res.g;
	}
	return g;
}
