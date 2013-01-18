@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@doc{Abstract Normal Form}
@wiki{Abstract Normal Form}
module normal::ANF

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import transform::CBGF;
import transform::XBGF;
import analyse::Metrics;
import lib::Rascalware;
import io::WriteBGF; // batch
import io::WriteCBGF; // batch
import io::ReadBGF; // batch
import export::BNF;
import IO;

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
		for (f <- [
					dropAllLabels,
					dropAllSelectors,
					dropAllTerminals,
					dropAllHorizontals,
					dropAllUnknowns,
					dropAllValues,
					dropAllChains,
					dropAllChainsToValues
			])
		{
			c2 = f(g);
			//println("Stage <f>:");
			//iprintln(c2);
			g = transform(forward(c2),g);
			c1 += c2;
		}
		c += c1;
	} while (!isEmpty(c1));
	// introduce chain rules
	for (n <- definedNs(g))
	{
		ps = prodsOfN(n,g.prods);
		//println("<len(ps)>");
		if (len(ps)>1)
		{
			// go over all vertical production rules
			for (p <- ps)
				if (nonterminal(_) !:= p.rhs && val(_) !:= p.rhs)
				{
					c2 = [extract_inline(production("",uniqueName(n,allNs(g)),p.rhs),innt(n))];
					// global extract can introduce conflicts with subsequent extracts,
					// that's why we need to transform immediately
					//println("Transforming <pp(g)>...");
					g = transform(forward(c2),g);
					c1 += c2;
				}
			//c1 += horizontal_vertical(innt(n));
		}
	}
	//iprintln(c1);
	// now we can have constructions like this:
	//   expression ::= (expression1 | expression2 | expression3 | expression | id | number) ;
	// with cleverly hidden reflexive chain rules
	//for (p <- g.prods, production(str l,str n,choice(L)) := p, [*L1,nonterminal(n),*L2] := L)
	//	// the last expression is a traceable variant of "nonterminal(n) in L"
	//	c1 += removeH_addH(production(l,n,choice([*L1, marked(nonterminal(n)), *L2])));
	for (p <- g.prods, production(str l,str n,nonterminal(n)) := p)
		// the last expression is a traceable variant of "nonterminal(n) in L"
		c1 += removeV_addV(p);
	return c+c1;
}

str uniqueName(str n, set[str] nts)
{
	int cx = 1;
	while ("<n><cx>" in nts) cx += 1;
	return "<n><cx>";
}

CBGFSequence dropAllLabels(BGFGrammar g) = [unlabel_designate(p) | p <- g.prods, p.label != ""];

// TODO: distribute

CBGFSequence dropAllHorizontals(BGFGrammar g) = toList({vertical_horizontal(innt(p.lhs)) | p <- g.prods, choice(_) := p.rhs});

CBGFSequence dropAllUnknowns(BGFGrammar g)

{
	CBGFSequence cbgf = [];
	set[str] used = usedNs(g.prods);
	for (p <- {q | q <- g.prods, (epsilon() := q.rhs || empty() := q.rhs)})
		if (p.lhs in used)
			cbgf += undefine_define([p]);
		else
			cbgf += eliminate_introduce([p]);
	return cbgf;
}

CBGFSequence dropAllValues(BGFGrammar g)
{
	CBGFSequence cbgf = [];
	set[str] used = usedNs(g.prods);
	if (/val(string()) := g)
		cbgf += replace_replace(val(string()),nonterminal("STRING"),globally());
	if (/val(integer()) := g)
		cbgf += replace_replace(val(integer()),nonterminal("INTEGER"),globally());
	return cbgf;
}

CBGFSequence dropAllChains(BGFGrammar g)
{
	CBGFSequence cbgf = [];
	set[str] defined = definedOnceNs(g.prods);
	for(p <- g.prods, nonterminal(str n) := p.rhs)
	{
		if (n == p.lhs)
			cbgf += abridge_detour(p);
		elseif (n notin usedNs(g.prods - p) && n in defined)
				cbgf += unchain_chain(p);
	}
	return cbgf;
}

CBGFSequence dropAllChainsToValues(BGFGrammar g)
{
	CBGFSequence cbgf = [];
	for(p <- g.prods, nonterminal(str n) := p.rhs)
		// TODO: shouldn't we also check for (n notin usedNs(g.prods - p)) ?
		if (p.lhs in usedNs(g.prods - p) && len(prodsOfN(p.lhs,g.prods))<2)
			cbgf += inline_extract(p,globally());
	return cbgf;
}

CBGFSequence dropAllSelectors(BGFGrammar g) = [anonymize_deanonymize(markAllSelectors(p)) | p <- {q | q <- g.prods, /selectable(_,_) := q}];
BGFProduction markAllSelectors(BGFProduction p) = visit(p) {case selectable(s,e) => marked(selectable(s,e)) };

CBGFSequence dropAllTerminals(BGFGrammar g) = [abstractize_concretize(markAllTerminals(p)) | p <- {q | q <- g.prods, /terminal(_) := q}];
BGFProduction markAllTerminals(BGFProduction p) = visit(p) {case terminal(t) => marked(terminal(t))};

public void main()
{
	for (src <- ["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"])
	//for (src <- ["ecore"])
	{
		println("Reading <src>...");
		BGFGrammar g = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
		CBGFSequence c = normalise(g);
		println("Writing the normalising trafo <src>...");
		writeCBGF(c,|home:///projects/slps/topics/convergence/guided/bgf/<src>.normalise.cbgf|);
		//iprintln(c);
		println("Transforming <src>...");
		g = transform(forward(c),g);
		//println(pp(g));
		println("Writing output to <src>...");
		writeBGF(g,|home:///projects/slps/topics/convergence/guided/bgf/<src>.normal.bgf|);
		writeFile(|home:///projects/slps/topics/convergence/guided/bgf/<src>.normal.bnf|,pp(g));
		//println(pp(g));
	}
}
