@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{ChainMixed}
module mutate::type2::ChainMixed

import lib::Rascalware;
import language::BGF;
import language::XScope;

// TODO: since ChainMixed is now used only for ANF, it softly assumes the vertical grammar style.
// If the style is not vertical, the result will be incomplete.
BGFGrammar ChainMixed(BGFGrammar g)
{
	ps = g.prods;
	ns = definedNs(ps);
	for (n <- ns)
	{
		<ps1,ps2,ps3> = splitPbyW(ps, innt(n));
		if (len(ps2)==1)
			continue;
		int cx = 1;
		ps4 = []
		for (p<-ps2)
			if (production(_,_,nonterminal(_)) := p)
				ps4 += [p];
			else
			{
				while ("<n><cx>" notin ns) cx += 1;
				ns += ["<n><cx>"];
				ps4 += [production(p.label,p.lhs,nonterminal("<n><cx>")), production("","<n><cx>",p.rhs)];
			}
		ps = ps1 + ps4 + ps3;
	}
	return grammar (g.roots, ps);
}
