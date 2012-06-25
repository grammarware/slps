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
	+ normAllStages(g);

CBGFSequence normAllStages(BGFGrammar gr)
{
	CBGFSequence c = [], c2 = [], c1 = [];
	BGFGrammar g = gr;
	do
	{
		c1 = [];
		for (f <- [dropAllLabels,dropAllSelectors,dropAllTerminals,dropAllHorizontals,dropAllUnknowns,dropAllChains,dropAllLabels])
		{
			c2 = f(g);
			//println("Stage <f>:");
			//iprintln(c2);
			g = transform(forward(c2),g);
			c1 += c2;
		}
		c += c1;
	} while (!isEmpty(c1));
	return c;
}

CBGFSequence dropAllLabels(BGFGrammar g) = [unlabel_designate(p) | p <- g.prods, p.label != ""];
CBGFSequence dropAllSelectors(BGFGrammar g) = [anonymize_deanonymize(markAllSelectors(p)) | p <- {q | q <- g.prods, /selectable(_,_) := q}];
//CBGFSequence dropAllSelectors(BGFGrammar g) = [anonymize_deanonymize(markAllSelectors(p)) | p <- g.prods, /selectable(_,_) := p];
//CBGFSequence dropAllTerminals(BGFGrammar g) = [abstractize_concretize(markAllTerminals(p)) | p <- g.prods, /terminal(_) := p];
CBGFSequence dropAllTerminals(BGFGrammar g) = [abstractize_concretize(markAllTerminals(p)) | p <- {q | q <- g.prods, /terminal(_) := q}];
// TODO: distribute
CBGFSequence dropAllHorizontals(BGFGrammar g) = [vertical_horizontal(innt(p.lhs)) | p <- g.prods, choice(_) := p.rhs];
CBGFSequence dropAllUnknowns(BGFGrammar g)
{
	CBGFSequence cbgf = [];
	set[str] used = usedNs(g.prods);
	for (p <- {q | q <- g.prods, epsilon() := q.rhs})
		if (p.lhs in used)
			cbgf += undefine_define([p]);
		else
			cbgf += eliminate_introduce([p]);
	return cbgf;
}
CBGFSequence dropAllChains(BGFGrammar g)
{
	CBGFSequence cbgf = [];
	set[str] defined = definedOnceNs(g.prods);
	for(p <- g.prods, nonterminal(str n) := p.rhs)
		if (n in defined && n notin usedNs(g.prods - p))
			cbgf += (n == p.lhs)? abridge_detour(p) : unchain_chain(p);
	return cbgf;
}


BGFProduction markAllSelectors(BGFProduction p) = visit(p) {case selectable(s,e) => marked(selectable(s,e)) };
BGFProduction markAllTerminals(BGFProduction p) = visit(p) {case terminal(t) => marked(terminal(t))};

public void main()
{
	for (src <- ["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"])
	//for (src <- ["python"])
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
