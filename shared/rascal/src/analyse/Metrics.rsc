@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Metrics

import syntax::BGF;

@doc{All nonterminals in a grammar: defined or used}
public set[str] allNs(list[BGFProduction] ps) = definedNs(ps) + usedNs(ps);
public set[str] allNs(BGFGrammar g) = allNs(g.prods);

@doc{Top nonterminals in a grammar: defined but not used}
public set[str] topNs(list[BGFProduction] ps) = definedNs(ps) - usedNRNs(ps);
 // We allow top nonterminals to refer to themselves. In general, this is arguable.
public set[str] topNs(BGFGrammar g) = topNs(g.prods);

@doc{Bottom nonterminals in a grammar: used but not defined}
public set[str] bottomNs(list[BGFProduction] ps) = usedNs(ps) - definedNs(ps);
public set[str] bottomNs(BGFGrammar g) = bottomNs(g.prods);

@doc{Leaf nonterminals in a grammar: not using any others}
//public set[str] leafNs(list[BGFProduction] ps) = {n | n <- definedNs(ps), production(_,n,rhs) <- ps, /nonterminal(n2) := rhs, n2 != n};
public set[str] leafNs(list[BGFProduction] ps) = {n | n <- definedNs(ps), (calls(n,ps)-n)=={} };
public set[str] leafNs(BGFGrammar g) = leafNs(g.prods);

@doc{All terminals used in a grammar}
public set[str] allTs(list[BGFProduction] ps) = {s | /terminal(str s) := ps};
public set[str] allTs(BGFGrammar g) = allTs(g.prods);

@doc{All nonterminals used in a grammar}
public set[str] usedNs(list[BGFProduction] ps) = {s | /nonterminal(str s) := ps};
public set[str] usedNs(BGFGrammar g) = usedNs(g.prods);

@doc{All nonterminals used non-recursively in a grammar}
public set[str] usedNRNs(list[BGFProduction] ps) = {s | p <- ps, /nonterminal(str s) := p.rhs, s != p.lhs};
public set[str] usedNRNs(BGFGrammar g) = usedNRNs(g.prods);

@doc{All nonterminals defined in a grammar}
public set[str] definedNs(list[BGFProduction] ps) = {s | production(_,str s,_) <- ps};
public set[str] definedNs(BGFGrammar g) = definedNs(g.prods);

public rel[str,str] calls(list[BGFProduction] ps) = {<n1,n2> | production(_,n1,rhs) <- ps, /nonterminal(n2) := rhs};
public set[str] calls(str x, list[BGFProduction] ps) = {n2 | production(_,x,rhs) <- ps, /nonterminal(n2) := rhs};

public list[BGFProduction] prodsOfN(str x, list[BGFProduction] ps) = [p | p <- ps, production(_,x,_) := p];