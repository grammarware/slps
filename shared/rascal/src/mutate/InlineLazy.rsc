@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{InlineLazy}
module mutate::InlineLazy

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::library::Util;
import transform::XBGF;
import diff::GDT;

// if a nonterminal has one production rule and one occurrence in the grammar, it is inlined
public BGFGrammar inlineLazy(BGFGrammar g)
{
	ns = allNs(g.prods);
	BGFGrammar g1 = grammar([],[]), g2 = g;
	// keep rewriting
	while(!gdts(g1,g2))
	{
	g1 = g2;
	lazy = {nt | nt <- ns, len({p | p:production(_,nt,_) <- g1.prods})==1} // defined by one prod
	     & {nt | nt <- ns, len([1 | /nonterminal(nt) := g1.prods])==1};    // occur once
	g2 = vtransform([inline(nt) | nt <- lazy],g1);
	println("-----second stage------");
	}
	// TODO: might want to change to non-verbal transform if bored to see messages
	return g1;
}
