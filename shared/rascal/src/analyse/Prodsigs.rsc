@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Prodsigs

import syntax::BGF;
import analyse::Metrics;
import lib::Rascalware;
import Relation; //domain
import List;
import Set;

data Footprint
	= fpnt()
	| fpopt()
	| fpplus()
	| fpstar()
	| fpmany(list[Footprint] fps)
	| fpempty()
	;

alias Signature = rel[str,Footprint];
alias NameMatch = rel[str,str];

Footprint makefp(n, nonterminal(n)) = fpnt();
Footprint makefp(n, optional(nonterminal(n))) = fpopt();
Footprint makefp(n, plus(nonterminal(n))) = fpplus();
Footprint makefp(n, star(nonterminal(n))) = fpstar();
Footprint makefp(n, sequence(L))
{
	s = [fp | e <- L, fp := makefp(n,e), fp != fpempty()];
	if(len(s)==0)
		return fpempty();
	elseif(len(s)==1)
		return getOneFrom(s);
	else
		return fpmany(s);
}
default Footprint makefp(str n, BGFExpression x) = fpempty();

Signature makesig(BGFProduction p) = {<n,makefp(n,p.rhs)> | n <- usedNs(p.rhs)};
Signature makesig(BGFExpression e) = {<n,makefp(n,e    )> | n <- usedNs(e    )};

bool eqfp(fpnt(), fpnt()) = true;
bool eqfp(fpopt(), fpopt()) = true;
bool eqfp(fpplus(), fpplus()) = true;
bool eqfp(fpstar(), fpstar()) = true;
bool eqfp(fpempty(), fpempty()) = true;
bool eqfp(fpmany(L1), fpmany(L2)) = multiseteq(L1,L2);
default bool eqfp(Footprint pi, Footprint xi) = false;

bool equivfp(fpplus(), fpstar()) = true;
bool equivfp(fpstar(), fpplus()) = true;
bool equivfp(fpmany(L1), fpmany(L2))
{
	//tuple[Footprint,set[Footprint]]
	if (isEmpty(L1)) return isEmpty(L2);
	<car,cdr> = List::takeOneFrom(L1);
	for (e <- L2)
		if (equivfp(car,e))
			return equivfp(fpmany(cdr), fpmany(L2 - e));
	return false;
}
default bool equivfp(Footprint pi, Footprint xi) = eqfp(pi,xi);

// strong equivalence relies on natural equality of footprints
// (i.e., == of rels)
bool eqps(BGFProduction p1, BGFProduction p2) = eqps(makesig(p1),makesig(p2));
bool eqps(BGFExpression e1, BGFExpression e2) = eqps(makesig(e1),makesig(e2));
bool eqps(Signature p, Signature q) = geqps(p,q,eqfp,true);

// weak equivalence relies on equivalence of footprints
bool weqps(BGFProduction p1, BGFProduction p2) = weqps(makesig(p1),makesig(p2));
bool weqps(BGFExpression e1, BGFExpression e2) = weqps(makesig(e1),makesig(e2));
default bool weqps(Signature p, Signature q) = geqps(p,q,equivfp,false);

// footprint-comparator-parametrised equivalence
bool geqps(Signature p, Signature q, bool(Footprint,Footprint) cmp, bool strong)// = p == q;
{
	if (strong && len(p) != len(q)) return false;
	for (<n,pi> <- p)
	{
		bool match = false;
		//for(<m,xi> <- q, cmp(pi,xi))
		for(<m,xi> <- q, cmp(pi,xi))
			if (match)
				return false; // multiple matches!
			else
			{
				match = true;
				q -= {<m,xi>};
			}
		if (strong && !match)
			return false;
	}
	// all matched!
	return true;
}

NameMatch makenamematch(BGFExpression e1, BGFExpression e2) = makenamematch(makesig(e1),makesig(e2));
NameMatch makenamematch(BGFProduction p1, BGFProduction p2) = makenamematch(makesig(p1),makesig(p2)); 
NameMatch makenamematch(Signature p, Signature q)
{
	NameMatch nm = {};
	set[str] unmatched = domain(q);
	for (<a,pi> <- p, <b,xi> <- q, equivfp(pi,xi))
	{
		//println("Checking <a>:<pi> vs <b>:<xi>...<eqfp(pi,xi)>");
		nm += <a,b>;
		// TODO: should exit if we want to work with non-equivalent signatures.
		// But do we, really?
		unmatched -= b;
	}
	// the omega's:
	nm += {<"",c> | c <- unmatched};
	return nm;
}

public str pp(Signature sig) = "\<"+joinStrings(["<n>: <pp(f)>" | <n,f> <- sig],", ")+"\>";
public str pp(fpnt()) = "1";
public str pp(fpopt()) = "?";
public str pp(fpplus()) = "+";
public str pp(fpstar()) = "*";
public str pp(fpmany(L)) = joinStrings([pp(f) | f <- L],"");
public str pp(fpempty()) = "0";
public default str pp(Footprint sig) = "XXX";

public str pp(NameMatch nm) = "\<"+joinStrings(["<(n=="")?"OMEGA":n> = <m>" | <n,m> <- nm],", ")+"\>";