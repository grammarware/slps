@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module converge::Guided

import IO;
import Set;
import Map;
import List;
import io::ReadBGF;
import syntax::BGF;
import analyse::Metrics;

list[str] sources =
	["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];

public void main()
{
	map[str,BGFGrammar] bgfs = ();
	println("Reading the grammars...");
	for (src <- sources)
		bgfs[src] = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
	println("Parsing the grammarbase with <size(bgfs)> grammars is done.");
	println("Starting with the root.");
	for (src <- sources)
	{
		println(" * Roots in <src>: <bgfs[src].roots>");
		if (isEmpty(bgfs[src].roots))
		{
			bgfs[src].roots = toList(topNs(bgfs[src]));
			println("   * Roots in <src> changed to tops: <bgfs[src].roots>");
		}
	}
	println("Done.");
}