@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Core

import syntax::BGF;
import syntax::XBGF;
import transform::library::Util;
import diff::GDT;
import List; // size

BGFGrammar performRenameN(str x, str y, grammar(rs, ps))
{
	list[BGFProduction] ps1,ps2,ps3,ps4;
	list[str] rs2;
	if ([*L1, x, *L2] := rs) rs2 = L1 + y + L2;
	else rs2 = rs;
	if (x in definedNs(ps))
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		ps4 = ps1 + [production(l,y,e) | p <- ps2, production(str l,x,BGFExpression e) := p] + ps3;
	}
	else
		ps4 = ps; 
	if (x in usedNs(ps4))
		return grammar(rs2,performReplace(nonterminal(x),nonterminal(y),ps4));
	else
		return grammar(rs2,ps4);
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
	int i = 0, len = size(what);
	while (i+len<=size(where))
	{
		if (eqE(sequence(slice(where,i,len)),sequence(what)))
			return slice(where,0,i) + with + slice(where,i+len,size(where)-i-len);
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
