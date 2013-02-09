@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{DeyaccifyAll}
module mutate::type1::DeyaccifyAll

import lib::Rascalware;
import language::BGF;

list[BGFProduction] tryDeYacc({production(_,n,sequence([nonterminal(n),x])),production(_,n,x)}) = [production("",n,plus(x))];
list[BGFProduction] tryDeYacc({production(_,n,sequence([x,nonterminal(n)])),production(_,n,x)}) = [production("",n,plus(x))];
list[BGFProduction] tryDeYacc({production(_,n,sequence([nonterminal(n),x])),production(_,n,y)}) = [production("",n,sequence([y,star(x)]))];
list[BGFProduction] tryDeYacc({production(_,n,sequence([x,nonterminal(n)])),production(_,n,y)}) = [production("",n,sequence([star(x),y]))];
list[BGFProduction] tryDeYacc({production(_,n,sequence([x,nonterminal(n)])),production(_,n,epsilon())}) = [production("",n,star(x))];
list[BGFProduction] tryDeYacc({production(_,n,sequence([nonterminal(n),x])),production(_,n,epsilon())}) = [production("",n,star(x))];
default list[BGFProduction] tryDeYacc(set[BGFProduction] pset) = [];

BGFGrammar DeyaccifyAll(BGFGrammar g)
{
	BGFProdList ps = g.prods;
	for (n in definedNs(ps))
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(n));
		ps4 = tryDeYacc(toSet(ps2));
		if (isEmpty(ps4))
			continue;
		else
			ps = ps1 + ps4 + ps3;
	}
	return grammar(g.roots, ps);
}
