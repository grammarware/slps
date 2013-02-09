@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module analyse::Layers

import language::BGF;
import language::XBGF;
import language::CBGF;
import io::ReadBGF;
import lib::Rascalware;
import analyse::Metrics;
import normal::BGF;
import export::BNF;
import IO;

public CBGFSequence removeLayers(rel[str,str] layers, BGFGrammar g)
	= [unite_splitN(l<0>, prodsOfN( l<1>, g.prods), globally()) | l <- layers];

public rel[str,str] detectLayers(BGFGrammar g)
{
	rel[str,str] res = {};
	rel[str,str] dres = {};
	g = normalise4(g);
	for (n1 <- definedNs(g), n2 <- definedNs(g))
	{
		BGFProdList ps1 = prodsOfN(n1,g.prods);
		BGFProdList ps2 = prodsOfN(n2,g.prods);
		// n2 chains to n1, but not exclusively
		if (
				/p:production(_,n2,nonterminal(n1)) := ps2
				&& !isEmpty(ps2 - p)
			)
			res += <n1,n2>;
		if (
				/production(_,n2,choice([*L1,nonterminal(n1),*L2])) := ps2
				&& !isEmpty(L1+L2)
			)
			res += <n1,n2>;
		
		for (<n1,n2> <- res)
			if (/production(_,n1,e) := ps1 && /nonterminal(n2) := e && n1!=n2)
				dres += <n1,n2>;
	}
	return dres;
}

public void main()
{
	for (src <- ["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"])
	{
		println("Reading <src>...");
		BGFGrammar g = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
		println("Analysing <src>...");
		rel[str,str] layers = detectLayers(g);
		if(!isEmpty(layers))
		{
			println("Found <layers>");
			println(removeLayers(layers,g));
			//println(pp(transform(forward(removeLayers(layers,g)),g)));
		}
	}
}