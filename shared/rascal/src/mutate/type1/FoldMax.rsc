@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{FoldMax}
module mutate::type1::FoldMax

import lib::Rascalware;
import language::BGF;
import language::XScope;
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
