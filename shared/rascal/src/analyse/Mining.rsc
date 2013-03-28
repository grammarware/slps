@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Mining

import analyse::Metrics;
import language::BGF;
import io::ReadBGF;
import String;
import List;
import Map;
import IO;
import lib::Rascalware;
import export::BNF;

// TODO: just import mutate::type2::RetireSs ?
// import normal::BGF;
// 
// SGrammar RetireSs(SGrammar g)
// {
// 	ps = visit(g.prods) {case selectable(_,BGFExpression e) => e};
// 	return <g.roots, normalise(g.prods)>;
// }
BGFProduction RetireSs(BGFProduction p) = visit(p) {case selectable(_,BGFExpression e) => e};

alias dict = map[BGFExpression,int];
alias NPC = tuple[int ns, int clasns, int ps, int cx, dict patterns, map[str,int] counts];
alias SGrammar = tuple[set[str] roots, map[str,BGFProdSet] prods];

NPC getZoo(loc zoo, NPC npc)
{
	dict patterns = npc.patterns;
	int n = npc.ns, pcx = npc.ps, cx = npc.cx, cns = npc.clasns;
	map[str,int] counts = npc.counts;
	BGFGrammar g;
	SGrammar s;
	set[str] allNTs = {};
	for (str lang <- listEntries(zoo), !endsWith(lang,".html"), str s <- listEntries(zoo+"/<lang>"), endsWith(s,".bgf"))
	{
		println("<lang>::<s>");
		cx += 1;
		g = readBGF(zoo+"/<lang>/<s>");
		allNTs = allNs(g);
		n += len(allNTs);
		pcx += len(g.prods);

		sg = splitGrammar(g);
		if (domain(sg.prods) != allNTs)
			println("Nonterminal sets are not equal!\n<range(sg.prods)>\n<allNTs>");
		
		for (metric <- AllMetrics)
		{
			res = metric(sg);
			if ("<metric>" notin counts) counts["<metric>"] = 0;
			counts["<metric>"] += len(res);
			allNTs -= res;
		}
		cns += len(allNTs);
		
		g = abstractPattern(g);
		for (BGFProduction p <- abstractPattern(g).prods)
			if (p.rhs in domain(patterns))
				patterns[p.rhs] += 1;
			else
				patterns[p.rhs] = 1;
	}
	return <n,cns,pcx,cx,patterns,counts>;
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
	map[str,BGFProdSet] ps = ();
	for (BGFProduction p <- g.prods)
		if (p.lhs in domain(ps))
			ps[p.lhs] += {p};
		else
			ps[p.lhs] = {p};
	for (str n <- bottomNs(g))
		ps[n] = {};
	for (str n <- g.roots, n notin ps)
		ps[n] = {};
	return <toSet(g.roots), ps>;
}

set[str] tops(SGrammar g)    = definedNs(g) - usedNs(g); // = {t | str t <- domain(g.prods), /nonterminal(t) !:= range(g.prods)};
set[str] bottoms(SGrammar g) = usedNs(g) - definedNs(g);
set[str] ifroots(SGrammar g) = g.roots & domain(g.prods);
// TODO: not _, but in fact [*nonterminal(_)]
// TODO: also account for vertical roots
set[str] multiroots(SGrammar g) = {n | str n<-g.roots, {production(_,n,choice(L))} := g.prods[n], allnonterminals(L)};

set[str] preterminals(SGrammar g) = {n | str n <- domain(g.prods), allterminals(g.prods[n])};

set[str] constructors(SGrammar g) = {n | str n <- domain(g.prods), allconstructors(g.prods[n])};
bool allconstructors(BGFProdSet ps) = ( true | it && selectable(_,epsilon()) := p | p <- ps );

set[str] horizontals(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,choice(L))} := g.prods[n] };
set[str] verticals(SGrammar g) = {n | str n <- domain(g.prods), len(g.prods[n])>1 };

set[str] pureseqs(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,rhs)} := g.prods[n], pureseq(rhs)};
bool pureseq(epsilon()) = true;
bool pureseq(empty()) = true;
bool pureseq(anything()) = true; // arguable
bool pureseq(val(_)) = true; // arguable
bool pureseq(terminal(_)) = true;
bool pureseq(nonterminal(_)) = true;
bool pureseq(sequence(L)) = ( true | it && pureseq(e) | e <- L );
default bool pureseq(BGFExpression rhs) = false;

set[str] cnfs(SGrammar g) = {n | str n <- domain(g.prods), allCNFs(g.prods[n]) };
bool allCNFs(BGFProdSet ps) = ( true | it && isCNF(p.rhs) | p <- ps );
bool isCNF(epsilon()) = true;
bool isCNF(terminal(_)) = true;
bool isCNF(sequence([nonterminal(_),nonterminal(_)])) = true;
default bool isCNF(BGFExpression e) = false;

// TODO: include other patterns?
set[str] seplists(SGrammar g) = {n | str n <- domain(g.prods), {p} := g.prods[n], isseplist(RetireSs(p))};
bool isseplist(production(_,_,sequence([BGFExpression a,star(sequence([BGFExpression b, a]))]) )) = true;
default bool isseplist(BGFProduction p) = false;

set[str] abstracts(SGrammar g) = {n | str n <- domain(g.prods), /terminal(_) !:= g.prods[n]};

set[str] empties(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,epsilon())} := g.prods[n]};

set[str] justplusses(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,plus(nonterminal(_)))} := g.prods[n]};
set[str] juststars(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,star(nonterminal(_)))} := g.prods[n]};
set[str] justopts(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,optional(nonterminal(_)))} := g.prods[n]};

// does not tolerate folding
set[str] names1(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,plus(choice(L)))} := g.prods[n], allterminals(L)};
set[str] names2(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,sequence([choice(L1),star(choice(L2))]))} := g.prods[n], allterminals(L1), allterminals(L2)};

// TODO: simple chain as an all chain where $m$ is used only once in the whole grammar
set[str] allchains(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,nonterminal(m))} := g.prods[n]};
set[str] reflchains(SGrammar g) = {n | str n <- domain(g.prods), {production(_,n,nonterminal(n))} := g.prods[n]};

// lower level functions
set[str] definedNs(SGrammar g) = {n | n <- domain(g.prods), {production(_,n,empty())} !:= g.prods[n], !isEmpty(g.prods[n]) };
set[str] usedNs(SGrammar g) = {n | /nonterminal(n) := range(g.prods)};

bool allnonterminals(BGFExprList xs) = ( true | it && nonterminal(_) := e | e <- xs );
// TODO: too permissive?
bool allterminals(BGFProdSet xs) = ( true | it && (terminal(_) := e || (sequence(L) := e && allterminals(L))) | e <- xs );
bool allterminals(BGFExprList xs) = ( true | it && terminal(_) := e | e <- xs );

// 
//                ADD CLASSIFIERS HERE!
// 
set[set[str](SGrammar)] AllMetrics =
	{
		tops,			// defined but not used
		bottoms,		// used but not defined
		names1,			// identifier names [a-z]+
		names2,			// identifier names [a-z][a-zA-Z_]*
		ifroots,		// if it is a root
		multiroots,		// a “fake” multiple root
		preterminals,	// defined with terminals
		constructors,	// defined with labelled epsilons
		pureseqs,		// pure sequential composition
		seplists,		// “fake” separator list
		cnfs,			// production rules in Chomsky normal form
		abstracts,		// abstract syntax (no terminal symbols)
		empties,		// nonterminal defines an empty language (epsilon)
		justplusses,	// x defined as y+
		juststars,		// x defined as y*
		justopts,		// x defined as y?
		allchains,		// chain production rule: a nonterminal on the left hand side and a nonterminal on the right hand side
		reflchains,		// reflexive chain production rule: right hand side equal to the left hand side
		horizontals,	// top level choice
		verticals		// multiple production rules per nonterminal
	};

// MAIN
public void main(list[str] as)
{
	loc zoo = |home:///projects/webslps/zoo|;
	NPC npc = getZoo(|home:///projects/webslps/zoo|,<0,0,0,0,(),()>);
	npc = getZoo(|home:///projects/webslps/tank|,npc);
	println("Total: <npc.cx> grammars, <npc.ps> production rules, <npc.ns> nonterminals (<npc.ns-npc.clasns> thereof classified), <len(npc.patterns)> patterns.");
	for (metric <- AllMetrics)
	{
		println("<100*npc.counts["<metric>"]/npc.ns>% classified as <metric>: <npc.counts["<metric>"]>.");
	}
	// Just the Zoo:
	//              Total: 42 grammars, 8927 production rules, 8277 nonterminals.
	// Zoo + Tank:
	//              Total: 99 grammars, 11570 production rules, 10943 nonterminals.
	// TODO: only report unclassified ones
	// for (BGFExpression e <- domain(npc.patterns))
	// 	println("<pp(e)>: <npc.patterns[e]>");
}

