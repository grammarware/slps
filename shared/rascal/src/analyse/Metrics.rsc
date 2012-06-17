@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Metrics

import syntax::BGF;

public set[str] allNs(list[BGFProduction] ps) = definedNs(ps) + usedNs(ps);
public set[str] allNs(BGFGrammar g) = allNs(g.prods);

public set[str] topNs(list[BGFProduction] ps) = definedNs(ps) - usedNs(ps);
public set[str] topNs(BGFGrammar g) = topNs(g.prods);

public set[str] allTs(list[BGFProduction] ps) = {s | /terminal(str s) := ps};
public set[str] allTs(BGFGrammar g) = allTs(g.prods);

public set[str] usedNs(list[BGFProduction] ps) = {s | /nonterminal(str s) := ps};
public set[str] usedNs(BGFGrammar g) = usedNs(g.prods);

public set[str] definedNs(list[BGFProduction] ps) = {s | production(_,str s,_) <- ps};
public set[str] definedNs(BGFGrammar g) = definedNs(g.prods);

