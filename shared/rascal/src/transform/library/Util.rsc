@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::library::Util

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import normal::BGF;
import diff::GDT;
import List; // tail

public XBGFOutcome notFoundP(BGFProduction p) = problemProd("Production rule not found",p);
public XBGFOutcome notFreshN(str n) = notFreshName("Nonterminal",n);
public XBGFOutcome freshN(str n) = freshName("Nonterminal",n);
public XBGFOutcome notFreshName(str name, str n) = problemStr("<name> must be fresh",n);
public XBGFOutcome freshName(str name, str n) = problemStr("<name> must not be fresh",n);

public BGFProduction unmark (BGFProduction p1)
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case marked(BGFExpression e) => e
	};
	//return normalise(p2);
	return p2;
}

public BGFProduction demark (BGFProduction p1) 
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = visit(p1)
	{
		case sequence([*L1,marked(BGFExpression e),*L2]) => sequence(L1 + L2)
		case marked(_) => epsilon()
	}
	return p2;
}

public BGFProduction demarkH (BGFProduction p1) 
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case choice([*L1,marked(BGFExpression e),*L2]) => choice(L1 + L2)
	}
	return p2;
}

public BGFProduction replaceMarker (BGFProduction p1, BGFExpression e) 
{
	p2 = visit(p1)
	{
		case marked(_) => e
	}
	return normalise(p2);
}

// remove selectors from marked subexpressions
public BGFProduction demarkS (BGFProduction p1) 
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case marked(selectable(str selector, BGFExpression expr)) => expr
	}
	return normalise(p2);
}

public bool checkScope(BGFProduction p, XBGFScope w)
{
	switch(w)
	{
		case globally(): return true;
		case nowhere(): return false;
		case innt(x): return production(_,x,_) := p;
		case notinnt(x): return production(_,x,_) !:= p;
		case inlabel(x): return production(x,_,_) := p;
		case notinlabel(x): return production(x,_,_) !:= p;
		case comboscope(w1,w2): return checkScope(p,w1) && checkScope(p,w2);
		default: throw "Unknown context <w>.";
	}
}
 
// order-preserving splitting of production rules
// returns <prods before context; prods in context; prods after context>
public tuple[list[BGFProduction],list[BGFProduction],list[BGFProduction]] splitPbyW(list[BGFProduction] ps, XBGFScope w)
{
	if (globally() := w)
		return <[],ps,[]>;
	if (nowhere() := w)
		throw "Splitting by empty scope!";
	if (inlabel(str x) := w && isEmpty(x))
		throw "Empty label is not a proper scope.";
	bool hit = false;
	list[BGFProduction] ps1 = [], ps2 = [], ps3 = [];
	for (p <- ps)
		if (checkScope(p,w))
			{
				hit = true;
				ps2 += p;
			}
		elseif (hit)
			ps3 += p;
		else
			ps1 += p;
	if (isEmpty(ps2))
		throw "Scope <w> not found.";
	return <ps1,ps2,ps3>;
}

// TODO moved to analyse::Metrics
public set[str] allNs(list[BGFProduction] ps) = definedNs(ps) + usedNs(ps);
public set[str] allTs(list[BGFProduction] ps) = {s | /terminal(str s) := ps};
public set[str] usedNs(list[BGFProduction] ps) = {s | /nonterminal(str s) := ps};
public set[str] definedNs(list[BGFProduction] ps) = {s | production(_,str s,_) <- ps};

public list[BGFProduction] replaceP(list[BGFProduction] ps, BGFProduction p1, BGFProduction p2)
{
	list[BGFProduction] ps2 = [];
	for (p <- ps)
		if (eqP(normalise(p),normalise(p1)))
			ps2 += p2;
		else
			ps2 += p;
	return ps2;
}

public bool inProds(BGFProduction p, []) = false;
public default bool inProds(BGFProduction p, list[BGFProduction] ps) = eqP(normalise(p),normalise(ps[0])) || inProds(p,tail(ps));
