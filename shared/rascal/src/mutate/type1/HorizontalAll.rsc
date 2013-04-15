@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{HorizontalAll}
module mutate::type1::HorizontalAll

import lib::Rascalware;
import language::BGF;
import language::XScope;
import analyse::Metrics;
import transform::library::Util;

BGFGrammar HorizontalAll(BGFGrammar g)
{
	ps = g.prods;
	for (str x <- definedNs(ps))
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		if (len(ps2)>1)
			ps = ps1 + production("",x,choice([(l=="" ? e : selectable(l,e)) | production(str l, str x, BGFExpression e) <- ps2])) + ps3;
	}
	return grammar(g.roots,ps);
}
