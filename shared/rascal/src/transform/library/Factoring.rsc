@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{factor,distribute}
module transform::library::Factoring

import language::BGF;
import language::XScope;
import language::XOutcome;
import normal::BGF;
import diff::GDT;
import transform::library::Util;
import transform::library::Brutal;

XBGFResult runFactor(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	e3 = normalise(makeDistributed(e1));
	e4 = normalise(makeDistributed(e2));
	if (!eqE(e3, e4))
		return <problemExpr2("Expressions must be related by distribution.",e1,e2),g>;
	return transform::library::Brutal::runReplace(e1,e2,w,g);
}

XBGFResult runDistribute(XBGFScope w, BGFGrammar g)
{
	<ps1,ps2,ps3> = splitPbyW(g.prods,w);
	if (/choice(_) !:= ps2)
		return <problemScope("No choices found, nothing to distribute",w),g>;
	return <ok(),grammar(g.roots, ps1 + normalise([makeDistributed(p) | p <- ps2]) + ps3)>;
}

BGFProduction makeDistributed(BGFProduction p) = production(p.label, p.lhs, makeDistributed(p.rhs));

BGFExpression makeDistributed(BGFExpression e1)
{
	if (choice(L1) := e1) // excessive normalisation
	{
		list[BGFExpression] Ln = [];
		for (e2 <- L1)
		{
			e3 = makeDistributed(e2);
			if (choice(L2) := e3)
				Ln += L2;
			else
				Ln += e2; // TODO or e3?
		}
		return choice(Ln);
	}
	elseif (sequence(L1) := e1)
	{
		list[list[BGFExpression]] Ln = [[]];
		for (e2 <- L1)
		{
			e3 = makeDistributed(e2);
			if (choice(L2) := e3)
				{
					Lm = [];
					for (e4 <- L2)
						Lm += [Li + e4 | Li <- Ln];
					Ln = Lm;
				}
			else
				Ln = [Li + e3 | Li <- Ln]; // TODO or e2?
		}
		return choice([sequence(Li) | Li <- Ln]);
	}
	else
		return e1;
}
