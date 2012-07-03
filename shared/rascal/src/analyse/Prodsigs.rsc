@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Prodsigs

import syntax::BGF;
import analyse::Metrics;
import Set;

data Footprint
	= fpnt()
	| fpopt()
	| fpplus()
	| fpstar()
	| fpmany(set[Footprint] fps)
	| fpempty()
	;

alias Signature = rel[str,Footprint];
alias NameMatch = rel[str,str];

Footprint makefp(n, nonterminal(n)) = fpnt();
Footprint makefp(n, optional(nonterminal(n))) = fpopt();
Footprint makefp(n, plus(nonterminal(n))) = fpplus();
Footprint makefp(n, star(nonterminal(n))) = fpstar();
Footprint makefp(n, sequence(L)) = fpmany({makefp(e) | e <- L});
default Footprint makefp(str n, BGFExpression x) = fpempty();

Signature makesig(BGFProduction p) = {<n,makefp(n,p.rhs)> | n <- usedNs(p.rhs)};

bool eqfp(fpnt(), fpnt()) = true;
bool eqfp(fpopt(), fpopt()) = true;
bool eqfp(fpplus(), fpplus()) = true;
bool eqfp(fpplus(), fpstar()) = true;
bool eqfp(fpstar(), fpplus()) = true;
bool eqfp(fpstar(), fpstar()) = true;
bool eqfp(fpempty(), fpempty()) = true;
bool eqfp(fpmany(L1), fpmany(L2))
{
	//tuple[Footprint,set[Footprint]]
	<head,tail> = takeOneFrom(L1);
	for (e <- L2)
		if (eqfp(head,e))
			return eqfp(tail, L2 - e);
	return false;
}
default bool eqfp(Footprint pi, Footprint xi) = false;

// strong equivalence relies on natural equality of footprints
// (i.e., == of rels)
bool eqps(BGFProduction p1, BGFProduction p2) = eqps(makesig(p1),makesig(p2));
default bool eqps(Signature p, Signature q) = geqps(p,q,bool(p,q){return p == q;});

// weak equivalence relies on equivalence of footprints
bool weqps(BGFProduction p1, BGFProduction p2) = weqps(makesig(p1),makesig(p2));
default bool weqps(Signature p, Signature q) = geqps(p,q,eqfp);

// footprint-comparator-parametrised equivalence
bool geqps(Signature p, Signature q, bool(Footprint,Footprint) cmp)// = p == q;
{
	for (<n,pi> <- p)
	{
		bool match = false;
		for(<m,xi> <- q, cmp(pi,xi))
			if (match)
				return false; // multiple matches!
			else
			{
				match = true;
				q -= <m,xi>;
			}
		if (!match)
			return false;
	}
	// all matched!
	return true;
}

NameMatch makenamematch(BGFProduction p1, BGFProduction p2) = makenamematch(makesig(p1),makesig(p2)); 
NameMatch makenamematch(Signature p, Signature q)
{
	NameMatch nm = {};
	set[str] unmatched = range(q);
	for (<a,pi> <- p, <b,xi> <- q, !eqfp(pi,xi))
	{
		nm += <a,b>;
		// TODO: should exit if we want to work with non-equivalent signatures.
		// But do we, really?
		unmatched -= b;
	}
	// the omega's:
	nm += {<"",c> | c <- unmatched};
	return nm;
}
