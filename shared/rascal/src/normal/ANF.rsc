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
import io::WriteBGF; // batch
import io::WriteCBGF; // batch
import io::ReadBGF; // batch

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
	set[str] defined = definedOnceNs(afterps);
	set[str] epsilons = {};
	//iprintln(afterps);
	for(q <- afterps)
		switch(q.rhs)
		{
			case choice(_) :
				cbgf += vertical_horizontal(innt(q.lhs));
			case epsilon() :
				epsilons += q.lhs;
			case nonterminal(str n) :
				if (n in defined && n notin usedNs(afterps - q))
					cbgf += (n == q.lhs)? abridge_detour(q) : unchain_chain(q);
			// default?
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

public void main()
{
	for (src <- ["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"])
	{
		println("Reading <src>...");
		BGFGrammar g = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
		CBGFSequence c = normalise(g);
		println("Writing the normalising trafo <src>...");
		writeCBGF(c,|home:///projects/slps/topics/convergence/guided/bgf/<src>.normalise.cbgf|);
		println("Transforming <src>...");
		g = transform(forward(c),g);
		println("Writing output to <src>...");
		writeBGF(g,|home:///projects/slps/topics/convergence/guided/bgf/<src>.normal.bgf|);
	}
}
