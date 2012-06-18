@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module converge::Guided

import IO;
import Set;
import Map;
import List;
import io::ReadBGF;
import io::WriteCBGF;
import syntax::BGF;
import syntax::CBGF;
import analyse::Metrics;
import normal::ANF;
import transform::XBGF;
import transform::CBGF;

list[str] sources =
	["master","antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];

public str prodsig(str n, list[BGFProduction] ps) = prodsig(prodsOfN(n,ps));
public str prodsig([]) = "";
public str prodsig(list[BGFProduction] ps) = (prodsig(ps[0]) | it + "/" + prodsig(p) | p <- tail(ps));
public str prodsig(BGFProduction p) = signature(p.rhs,p.lhs);

str signature(nonterminal(x),n) = x == n ? "0" : "1";
str signature(optional(e),n) = nonterminal(n) := e ? "O" : "?";
str signature(plus(e),n) = nonterminal(n) := e ? "P" : "+";
str signature(star(e),n) = nonterminal(n) := e ? "S" : "*";
str signature(anything(),_) = "a";
str signature(seplistplus(e,s),n) = nonterminal(n) := e ? "0*" : "1*";
str signature(sepliststar(e,s),n) = "?";
str signature(sequence(L),n) = ("" | it + signature(e,n) | e <- L);
default str signature(BGFExpression e, str n) = "_";

public void main()
{
	map[str,BGFGrammar] bgfs = ();
	map[str,CBGFSequence] cbgfs = ();
	println("Reading the grammars...");
	//BGFGrammar master = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
		bgfs[src] = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
	println("Parsing the grammarbase with <size(bgfs)> grammars is done.");
	println("Normalising the grammars...");
	for (src <- sources)
	{
		cbgfs[src] = normal::ANF::normalise(bgfs[src]);
		println("Normalising <src> with");
		//iprintln(cbgfs[src]); 
		bgfs[src] = transform(forward(cbgfs[src]),bgfs[src]);
		//if (src == "rascal-c")
		//	iprintln(bgfs[src]);
	}
	println("Starting with the root.");
	for (src <- sources)
	{
		if(src=="master")
		{
			println(" * In the master grammar, the root is called <bgfs[src].roots[0]>, prodsig <prodsig(bgfs[src].roots[0],bgfs[src].prods)>");
			//namemapping?
			continue;
		}
		if (size(bgfs[src].roots)==1)
		{
			println(" * In <src>: maps to <bgfs[src].roots[0]>, prodsig <prodsig(bgfs[src].roots[0],bgfs[src].prods)>");
		}
		else
		{
			println(" * In <src>: unconclusive, looking at definitions of <bgfs[src].roots>");
			for (r <- bgfs[src].roots)
				println("    * <r>: prodsig <prodsig(r,bgfs[src].prods)>");
		}
		//if (isEmpty(bgfs[src].roots))
		//{
		//	bgfs[src].roots = toList(topNs(bgfs[src]));
		//	println("   * Roots in <src> changed to tops: <bgfs[src].roots>");
		//}
	}
	println("Writing CBGFs...");
	for (src <- sources)
		writeCBGF(cbgfs[src],|home:///projects/slps/topics/convergence/guided/bgf/<src>.cbgf|);
	println("Done.");
}