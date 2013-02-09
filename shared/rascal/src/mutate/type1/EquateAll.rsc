@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{EquateAll}
module mutate::type1::EquateAll

import lib::Rascalware;
import language::BGF;
import language::XScope;
import transform::Results;
import transform::library::Nonterminals;
import diff::GDT;

BGFGrammar EquateAll(BGFGrammar g)
{
	str xy = "UNDEF";
	ns = definedNs(g.prods);
	while (xy in ns) xy += "X";
	for (x <- ns, y <- ns)
	{
		XBGFResult r1 = runRenameN(x,xy,g);
		XBGFResult r2 = runRenameN(y,xy,g);
		if (ok() !:= r1.r || ok() !:= r2.r)
			continue;
		<_,ps2x,_> = splitPbyW(r1.prods,innt(xy));
		<_,ps2y,_> = splitPbyW(r2.prods,innt(xy));
		if (gdts(grammar([],ps2x),grammar([],ps2y)))
		{
			r = transform::library::Nonterminals::runEquate(x,y,g);
			if (ok() := r.r)
				g = r.g;
		}
	}
	return g;
}
