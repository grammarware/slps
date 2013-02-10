@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{replace}
module transform::library::Brutal

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;
import normal::BGF;
import diff::GDT;

XBGFResult runReplace(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	list[BGFProduction] ps1,ps2,ps3,ps4;
	<ps1,ps2,ps3> = splitPbyW(g.prods, w);
	ps4 = performReplace(e1,e2,ps2);
	if (ps2 == ps4)
		{
			ps4 = performReplace(normalise(e1),normalise(e2),ps2); // TODO check if needed
			if (ps2 == ps4)
				return <problemExpr2("Vacuous replace",e1,e2),g>;
		}
	return <ok(),grammar(g.roots, ps1 + normalise(ps4) + ps3)>;
}

list[BGFProduction] performReplace(BGFExpression e1, BGFExpression e2, list[BGFProduction] ps)
{
	list[BGFExpression] L5;
	switch(e1)
	{
		case sequence(L1):
		{
			if (sequence(L4) := e2) L5 = L4; else L5 = [e2];
			return visit(ps){case sequence(L2) => sequence(replaceSubsequence(L2,L1,L5))};
		}
		case choice(L1): 
		{
			if (choice(L4) := e2) L5 = L4; else L5 = [e2];
			return visit(ps){case choice(L2) => choice(replaceChoice(L2,L1,L5))};
		}
		default:
			return visit(ps){case e1 => e2}
	}
}

list[BGFExpression] replaceSubsequence(list[BGFExpression] where, list[BGFExpression] what, list[BGFExpression] with)
{
	if (eqE(sequence(where),sequence(what))) return with;
	int i = 0, sz = len(what);
	while (i+sz <= len(where))
	{
		if (eqE(sequence(slice(where,i,sz)),sequence(what)))
			return slice(where,0,i) + with + slice(where, i+sz, len(where)-i-sz);
		i+=1;
	}
	return where;
}

list[BGFExpression] replaceChoice(list[BGFExpression] where, list[BGFExpression] what, list[BGFExpression] with)
{
	if (eqE(choice(where),choice(what))) return with;
	unmatched = where;
	<res,es1,es2> = diff::GDT::tryMatchChoices(what,where);
	if (res)
		return es1 + with + es2;
	else
		return where;
}
