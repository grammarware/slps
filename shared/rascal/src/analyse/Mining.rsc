@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Mining

import analyse::Metrics;
import language::BGF;
import io::ReadBGF;
import String;
import IO;
import lib::Rascalware;

public void main(list[str] as)
{
	loc zoo = |home:///projects/webslps/zoo|;
	tuple[int,int,int] npc = getZoo(|home:///projects/webslps/zoo|,<0,0,0>);
	npc = getZoo(|home:///projects/webslps/tank|,npc);
	println("Total: <npc<2>> grammars, <npc<1>> production rules, <npc<0>> nonterminals.");
	// Just the Zoo:
	//              Total: 42 grammars, 8927 production rules, 8277 nonterminals.
	// Zoo + Tank:
	//              Total: 99 grammars, 11570 production rules, 10943 nonterminals.
}

tuple[int,int,int] getZoo(loc zoo, tuple[int,int,int] npc)
{
	int n, p, cx;
	<n,p,cx> = npc;
	BGFGrammar g;
	for (str lang <- listEntries(zoo), !endsWith(lang,".html"), str s <- listEntries(zoo+"/<lang>"), endsWith(s,".bgf"))
	{
		println(s);
		cx += 1;
		g = readBGF(zoo+"/<lang>/<s>");
		n += len(allNs(g));
		p += len(g.prods);
	}
	return <n,p,cx>;
}