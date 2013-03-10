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
alias NPC = tuple[int,int,int,dict];
alias SGrammar = tuple[set[str] roots, map[str,set[BGFProduction]] prods];

public void main(list[str] as)
{
	loc zoo = |home:///projects/webslps/zoo|;
	NPC npc = getZoo(|home:///projects/webslps/zoo|,<0,0,0,()>);
	npc = getZoo(|home:///projects/webslps/tank|,npc);
	println("Total: <npc<2>> grammars, <npc<1>> production rules, <npc<0>> nonterminals, <len(npc<3>)> patterns.");
	// Just the Zoo:
	//              Total: 42 grammars, 8927 production rules, 8277 nonterminals.
	// Zoo + Tank:
	//              Total: 99 grammars, 11570 production rules, 10943 nonterminals.
	for (BGFExpression e <- domain(npc<3>))
		println("<pp(e)>: <npc<3>[e]>");
}

NPC getZoo(loc zoo, NPC npc)
{
	dict patterns;
	int n, pcx, cx;
	<n,pcx,cx,patterns> = npc;
	BGFGrammar g;
	SGrammar s;
	for (str lang <- listEntries(zoo), !endsWith(lang,".html"), str s <- listEntries(zoo+"/<lang>"), endsWith(s,".bgf"))
	{
		println(s);
		cx += 1;
		g = readBGF(zoo+"/<lang>/<s>");
		s = splitGrammar(g);
		n += len(allNs(g));
		pcx += len(g.prods);
		
		g = abstractPattern(g);
		for (BGFProduction p <- abstractPattern(g).prods)
			if (p.rhs in domain(patterns))
				patterns[p.rhs] += 1;
			else
				patterns[p.rhs] = 1;
	}
	return <n,pcx,cx,patterns>;
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
