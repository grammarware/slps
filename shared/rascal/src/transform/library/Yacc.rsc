@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{yaccify,deyaccify}
module transform::library::Yacc

import lib::Rascalware;
import language::BGF;
import language::XScope;
import language::XOutcome;
import transform::library::Util;

bool yaccification(production(_,n,sequence([y,star(x)])),
				  {production(_,n,sequence([nonterminal(n),x])),
				   production(_,n,y)}) = true;
bool yaccification(production(_,n,plus(x)),
				  {production(_,n,sequence([nonterminal(n),x])),
				   production(_,n,x)}) = true;
bool yaccification(production(_,n,sequence([star(x),y])),
				  {production(_,n,sequence([x,nonterminal(n)])),
				   production(_,n,y)}) = true;
bool yaccification(production(_,n,plus(x)),
				  {production(_,n,sequence([x,nonterminal(n)])),
				   production(_,n,x)}) = true;
// the next two are not really necessary, if we figure out how to combine pattern matching with normalisations
bool yaccification(production(_,n,star(x)),
				  {production(_,n,sequence([x,nonterminal(n)])),
				   production(_,n,epsilon())}) = true;
bool yaccification(production(_,n,star(x)),
				  {production(_,n,sequence([nonterminal(n),x])),
				   production(_,n,epsilon())}) = true;
default bool yaccification(BGFProduction p,set[BGFProduction] ps) = false;
//bool yaccification(_,_) = false;

BGFProduction performDeYacc(set[BGFProduction] pset)
{
	// TODO figure out a way to do the same as with yaccify
	switch(pset)
	{
		case {production(_,n,sequence([nonterminal(n),x])),
		      production(_,n,x)}:
			return production("",n,plus(x));
		case {production(_,n,sequence([x,nonterminal(n)])),
		      production(_,n,x)}:
			return production("",n,plus(x));
		case {production(_,n,sequence([nonterminal(n),x])),
		      production(_,n,y)}:
			return production("",n,sequence([y,star(x)]));
		case {production(_,n,sequence([x,nonterminal(n)])),
		      production(_,n,y)}:
			return production("",n,sequence([star(x),y]));
		case {production(_,n,sequence([x,nonterminal(n)])),
		      production(_,n,epsilon())}:
			return production("",n,star(x));
		case {production(_,n,sequence([nonterminal(n),x])),
		      production(_,n,epsilon())}:
			return production("",n,star(x));
		default:
			throw "Nonterminal <x> is not deyaccifiable.";
	};
}

XBGFResult runDeyaccify(str n, BGFGrammar g)
{
	if (n notin definedNs(g.prods))
		return <freshName(n),g>;
	<ps1,ps2,ps3> = splitPbyW(g.prods,innt(n));
	if (len(ps2) < 2)
		return <problemStr("Nonterminal must be defined vertically for deyaccification to work",n),g>;
	if (len(ps2) > 2)
		return <problemProds("No deyaccification patterns for <len(ps2)> production rules known",ps2),g>;
	return <ok(),grammar(g.roots, ps1 + performDeYacc(toSet(ps2)) + ps3)>;
}

XBGFResult runYaccify(list[BGFProduction] ps1, BGFGrammar g)
{
	if ({str x} := definedNs(ps1))
	{
		<ps3,ps4,ps5> = splitPbyW(g.prods,innt(x));
		if ([dyp1] := ps4 && [yp1,yp2] := ps1 && yaccification(dyp1,{yp1,yp2}))
			return <ok(),grammar(g.roots, ps3 + ps1 + ps5)>;
		else
			return <problemProds2("Unsuitable yaccification",ps1,ps4),g>;
	}
	else 
		return <problem("Production rules must define just one nonterminal."),g>;
}

