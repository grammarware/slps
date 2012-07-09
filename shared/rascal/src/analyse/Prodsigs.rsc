@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Prodsigs

import syntax::BGF;
import analyse::Metrics;
import lib::Rascalware;
import Relation; //domain
import List;
import Set;
import String;
import export::LaTeX;

data Footprint
	= fpnt()
	| fpopt()
	| fpplus()
	| fpstar()
	| fpmany(list[Footprint] fps)
	| fpempty()
	;

alias Signature = rel[str,Footprint];
alias NameMatch = rel[str,str,bool];

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
	//println("[?] Checking <pp(p)> and <pp(q)> for <strong?"strong":"weak"> equivalence.");
	if (strong && len(p) != len(q)) return false;
	for (<n,pi> <- p)
	{
		bool match = false;
		for(<m,xi> <- q, cmp(pi,xi))
		{
			// TODO: make recursive
			//println(" [?] <n> == <m> as <pp(pi)>?");
			//if (match)
			//	return false; // multiple matches!
			//else
			{
				match = true;
				q -= {<m,xi>};
			}
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
		nm += <a,b,eqfp(pi,xi)>;
		// TODO: should exit if we want to work with non-equivalent signatures.
		// But do we, really?
		unmatched -= b;
	}
	// the omega's:
	nm += {<"",c,false> | c <- unmatched};
	return nm;
}

set[NameMatch] makenamematches(BGFExpression e1, BGFExpression e2) = makenamematches(makesig(e1),makesig(e2));
set[NameMatch] makenamematches(BGFProduction p1, BGFProduction p2) = makenamematches(makesig(p1),makesig(p2));
set[NameMatch] makenamematches({}, {}) = {};
set[NameMatch] makenamematches(Signature p, {}) = {{<c,"",false> | <c,_> <- p}};
set[NameMatch] makenamematches({}, Signature q) = {{<"",c,false> | <c,_> <- q}};
set[NameMatch] makenamematches({<a,pi>}, {<b,xi>}) = equivfp(pi,xi) ? {{<a,b,eqfp(pi,xi)>}} : {{<a,"",false>,<"",b,false>}}; 

default set[NameMatch] makenamematches(Signature p, Signature q) =
 {
 	m + {<a,b,eqfp(pi,xi)>}
 	|
 	<a,pi> <- p, <b,xi> <- q, equivfp(pi,xi),
 	m <- makenamematches(domainX(p,{a}), domainX(q,{b}))
 };

public str pp(Signature sig) = "\<"+joinStrings(["<n>: <pp(f)>" | <n,f> <- sig],", ")+"\>";
public str pp(fpnt()) = "1";
public str pp(fpopt()) = "?";
public str pp(fpplus()) = "+";
public str pp(fpstar()) = "*";
public str pp(fpmany(L)) = joinStrings([pp(f) | f <- L],"");
public str pp(fpempty()) = "0";
public default str pp(Footprint sig) = "XXX";

public str pp(NameMatch nm) = "\<"+joinStrings(["<(n=="")?"OMEGA":n> <t?"=":"~"> <m>" | <n,m,t> <- nm],", ")+"\>";

public str ppl({}) = "\\varnothing";
public str ppl(Signature sig) = "\\{ <joinStrings(["\\langle <export::LaTeX::ppnt(n)>, <ppl(f)>\\rangle" | <n,f> <- sig],", ")>\\}";
public str ppl(fpnt()) = "1";
public str ppl(fpopt()) = "{?}";
public str ppl(fpplus()) = "{+}";
public str ppl(fpstar()) = "{*}";
public str ppl(fpmany(L)) = joinStrings([ppl(f) | f <- L],"");
public str ppl(fpempty()) = "\\varnothing";
public default str ppl(Footprint sig) = "XXX";

public str ppl(NameMatch nm) = "\\langle <joinStrings(["<(n=="")?"\\omega":export::LaTeX::ppnt(n)>, <m>" | <n,m,_> <- nm],", ")>\\rangle";
