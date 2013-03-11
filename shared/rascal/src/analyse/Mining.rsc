@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Mining

import analyse::Metrics;
import language::BGF;
import io::ReadBGF;
import String;
import Map;
import IO;
import lib::Rascalware;
import export::BNF;

alias dict = map[BGFExpression,int];
alias NPC = tuple[int ns, int clasns, int ps, int cx, dict patterns];
alias SGrammar = tuple[set[str] roots, map[str,set[BGFProduction]] prods];

public void main(list[str] as)
{
	loc zoo = |home:///projects/webslps/zoo|;
	NPC npc = getZoo(|home:///projects/webslps/zoo|,<0,0,0,0,()>);
	npc = getZoo(|home:///projects/webslps/tank|,npc);
	println("Total: <npc.cx> grammars, <npc.ps> production rules, <npc.ns> nonterminals (<npc.clasns> thereof classified), <len(npc.patterns)> patterns.");
	// Just the Zoo:
	//              Total: 42 grammars, 8927 production rules, 8277 nonterminals.
	// Zoo + Tank:
	//              Total: 99 grammars, 11570 production rules, 10943 nonterminals.
	// TODO: only report unclassified ones
	// for (BGFExpression e <- domain(npc.patterns))
	// 	println("<pp(e)>: <npc.patterns[e]>");
}

NPC getZoo(loc zoo, NPC npc)
{
	// NPC res;
	dict patterns = npc.patterns;
	int n = npc.ns, pcx = npc.ps, cx = npc.cx, cns = npc.clasns;
	BGFGrammar g;
	SGrammar s;
	set[str] allNTs = {};
	for (str lang <- listEntries(zoo), !endsWith(lang,".html"), str s <- listEntries(zoo+"/<lang>"), endsWith(s,".bgf"))
	{
		println(s);
		cx += 1;
		g = readBGF(zoo+"/<lang>/<s>");
		n += len(allNs(g));
		pcx += len(g.prods);

		s = splitGrammar(g);
		ts = tops(s);
		
		// TODO: generalise for all metrics
		// allNTs = range(s.prods) - ts;
		// cns += len(s.prods) - len(allNTs);
		cns += len(ts);
		
		g = abstractPattern(g);
		for (BGFProduction p <- abstractPattern(g).prods)
			if (p.rhs in domain(patterns))
				patterns[p.rhs] += 1;
			else
				patterns[p.rhs] = 1;
	}
	// res.ns = n;
	// res.
	return <n,cns,pcx,cx,patterns>;
}

BGFGrammar abstractPattern(BGFGrammar g)
	= visit(g)
	{
		case nonterminal(_) => nonterminal("N")
		case terminal(_) => terminal("T")
		case selectable(_, BGFExpression expr) => expr
	};

SGrammar splitGrammar(BGFGrammar g)
{
	map[str,set[BGFProduction]] ps = ();
	for (BGFProduction p <- g.prods)
		if (p.lhs in domain(ps))
			ps[p.lhs] += {p};
		else
			ps[p.lhs] = {p};
	for (str n <- bottomNs(g))
		ps[n] = {};
	return <toSet(g.roots), ps>;
}

set[str] tops(SGrammar g)
// 	alias SGrammar = tuple[set[str] roots, map[str,set[BGFProduction]] prods];
{
	set[str] d = domain(g.prods);
	set[set[BGFProduction]] r = range(g.prods);
	return {t | str t <- d, /nonterminal(t) !:= r};
}
