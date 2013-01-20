@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{InlinePlus}
module mutate::InlinePlus

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import transform::library::Util;
import transform::XBGF;
import IO;

// (1) inline all nonterminals defined as pluses of other nonterminals
// (2) massage x+? to x* if necessary
public BGFGrammar inlinePlus(BGFGrammar g)
{
	ns = allNs(g.prods);
	todo = ({nt | p:production(_,nt,plus(nonterminal(_))) <- g.prods}       // defined as plus
	     & {nt | nt <- ns, len({p | p:production(_,nt,_) <- g.prods})==1})  // not defined otherwise
	     - toSet(g.roots);
	//println({nt | p:production(_,nt,plus(nonterminal(_))) <- g.prods});
	//println({nt | nt <- ns, len({p | p:production(_,nt,_) <- g.prods})==1});
	//println(todo);
	//println(g.roots);
	//g2 = vtransform([inline(nt) | nt <- todo],g);
	// TODO: might want to change to non-verbal transform if bored to see messages
	return visit(vtransform([inline(nt) | nt <- todo],g))
	{
		case optional(plus(nonterminal(nt))) => star(nonterminal(nt))
	};
}

