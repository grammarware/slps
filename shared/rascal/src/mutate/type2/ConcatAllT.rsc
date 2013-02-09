@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{ConcatAllT}
module mutate::type2::ConcatAllT

import language::BGF;

BGFGrammar ConcatAllT(BGFGrammar g)
{
	BGFProdList ps = innermost visit(g.prods)
	{
		case sequence([*L1,terminal(t1),terminal(t2),*L2]) => sequence([*L1,terminal(t1+t2),*L2])
	}
	return grammar (g.roots, ps);
}
