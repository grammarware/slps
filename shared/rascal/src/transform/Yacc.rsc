@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::Yacc

import syntax::BGF;

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

BGFProduction deyaccify(set[BGFProduction] pset)
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
