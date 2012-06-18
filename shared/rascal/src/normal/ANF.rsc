@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{Abstract Normal Form}
module normal::ANF

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import transform::CBGF;
import transform::XBGF;
import analyse::Metrics;
import Set;
import List;
import IO; //debug

CBGFSequence normalise(BGFGrammar g)
	= (topNs(g) - leafNs(g) == toSet(g.roots)
	? []
	: [reroot_reroot(g.roots,toList(topNs(g) - leafNs(g)))])
	+ normStage2(normStage1(g.prods),g);

CBGFSequence normStage1(list[BGFProduction] ps) = [*normStage1(p) | p <- ps];

CBGFSequence normStage2(CBGFSequence cbgf, BGFGrammar g)
{
	list[BGFProduction] afterps = transform(forward(cbgf),g).prods;
	set[str] used = usedNs(afterps);
	set[str] epsilons = {};
	//iprintln(afterps);
	for(q <- afterps)
	{
		if (choice(_) := q.rhs)
			cbgf += vertical_horizontal(innt(q.lhs));
		if (epsilon() := q.rhs)
			epsilons += q.lhs;
	}
	//println(epsilons);
	for (n <- epsilons)
		if (n in used)
			cbgf += undefine_define([production("",n,epsilon())]);
		else
			cbgf += eliminate_introduce([production("",n,epsilon())]);
	return cbgf;
}

CBGFSequence normStage1(BGFProduction p)
{
	CBGFSequence cbgf = [];
	if (p.label != "")
		cbgf += unlabel_designate(p);
	if (/selectable(_,_) := p)
		cbgf += anonymize_deanonymize(markAllSelectors(p));
	if (/terminal(_) := p)
		cbgf += abstractize_concretize(markAllTerminals(p));
	// TODO: distribute
	return visit(cbgf)
	{
		case anonymize_deanonymize(production(_,n,rhs)) => anonymize_deanonymize(production("",n,rhs))
		case abstractize_concretize(production(_,n,rhs)) => abstractize_concretize(production("",n,rhs))
	};
}

BGFProduction markAllSelectors(BGFProduction p) = visit(p)
{
	case selectable(s,e) => marked(selectable(s,e))
};

BGFProduction markAllTerminals(BGFProduction p) = visit(p)
{
	case terminal(t) => marked(terminal(t))
	case selectable(s,e) => e
};

//void main()
//{
//	BGFProduction p = production("","function",sequence([selectable("n",nonterminal("ID")),plus(selectable("a",nonterminal("ID"))),terminal("="),selectable("e",nonterminal("expr")),plus(nonterminal("NEWLINE"))]));
//	iprintln(markAllTerminals
//}