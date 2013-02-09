@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{EliminateTop}
module mutate::type1::EliminateTop

import lib::Rascalware;
import language::BGF;
import language::XScope;

BGFGrammar EliminateTop(BGFGrammar g)
{
	ps = g.prods;
	for (x <- definedNs(g.prods) - g.roots)
	{
		<ps1,_,ps3> = splitPbyW(ps,innt(x));
		if (x notin usedNs(ps1+ps3))
			ps = ps1+ps3;
	}
	return grammar(g.roots, ps);
}
