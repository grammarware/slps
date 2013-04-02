@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{VerticalAll}
module mutate::type1::VerticalAll

import lib::Rascalware;
import language::BGF;
import language::XScope;

BGFGrammar HorizontalAll(BGFGrammar g)
{
	ps = g.prods;
	for (str x <- definedNs(ps))
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		if ([production(str l, str x, choice(BGFExprList es)] := ps2)
			ps = ps1 + [production("",x,e) | e <- es] + ps3;
	}
	return grammar(g.roots,ps);
}