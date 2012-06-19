@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module converge::Guided

import IO;
import Set;
import Map;
import List;
import io::ReadBGF;
import io::WriteBGF;
import io::WriteCBGF;
import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import analyse::Metrics;
import normal::ANF;
import transform::XBGF;
import transform::CBGF;

list[str] sources =
	["master","antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];

alias PRODSIG = list[list[tuple[str,BGFExpression]]];

//BGFProduction get1Prod(nt, grammar(_,[production(str l, nt, BGFExpression e)])) = production(l, nt, e);
BGFProduction get1Prod(str nt, BGFGrammar g)
{
	if ([production(str l, nt, BGFExpression e)] := prodsOfN(nt,g.prods))
		return production(l, nt, e);
	else
		throw "Unexpected grammar <prodsOfN(nt,g.prods)> for nonterminal <nt>";
}

public str prodsig(str n, list[BGFProduction] ps) = prodsig(prodsOfN(n,ps));
public str prodsig([]) = "";
public str prodsig(list[BGFProduction] ps) = (prodsig(ps[0]) | it + "/" + prodsig(p) | p <- tail(ps));
public str prodsig(BGFProduction p) = signature(p.rhs,p.lhs);
//prodsig(
//public list[list[tuple[str,BGFExpression]]] prodsig(str n, list[BGFProduction] ps) = prodsig(prodsOfN(n,ps));
//public list[list[tuple[str,BGFExpression]]] prodsig([]) = [[]];
//public list[list[tuple[str,BGFExpression]]] prodsig(list[BGFProduction] ps)
//	//= (prodsig(ps[0]) | it + "/" + prodsig(p) | p <- tail(ps));
//	= [prodsig(p) | p <- ps] ;
//public list[tuple[str,BGFExpression]] prodsig(BGFProduction p) = [<signature(e,p.lhs),e> | BGFExpression e <- p.rhs];

str signature(nonterminal(x),n) = x == n ? "0" : "1";
str signature(optional(e),n) = nonterminal(n) := e ? "O" : "?";
str signature(plus(e),n) = nonterminal(n) := e ? "P" : "+";
str signature(star(e),n) = nonterminal(n) := e ? "S" : "*";
str signature(anything(),_) = "a";
str signature(seplistplus(e,s),n) = nonterminal(n) := e ? "0*" : "1*";
str signature(sepliststar(e,s),n) = "?";
str signature(sequence(L),n) = ("" | it + signature(e,n) | e <- L);
default str signature(BGFExpression e, str n) = "_";

bool wider(star(_), plus(_)) = true;
bool wider(star(_), nonterminal(_)) = true;
bool wider(star(_), val(_)) = true;
bool wider(star(_), optional(_)) = true;
bool wider(plus(_), nonterminal(_)) = true;
bool wider(plus(_), val(_)) = true;
bool wider(sequence([e1]),e2) = wider(e1,e2);
bool wider(e1,sequence([e2])) = wider(e1,e2);
//bool wider(sequence([e1,*L1]),sequence([*L2a,e2,*L2b]))
bool wider(sequence([*L1a,e1,*L1b]),sequence([e2,*L2]))
	= wider(e1,e2)
	&& wider(sequence(L1a+L1b),sequence(L2));
bool wider(sequence([*L1a,e1,*L1b]),e2) = wider(e1,e2);
//bool wider(sequence(L1),sequence(L2)) = wider(L1[0],L2[0]) && wider(sequence(tail(L1)),sequence(tail(L1)));
default bool wider(BGFExpression e1, BGFExpression e2) = same(e1,e2);

bool isAtomic(val(_)) = true;
bool isAtomic(nonterminal(_)) = true;
default bool isAtomic(BGFExpression _) = false;

bool same(star(_),star(_)) = true;
bool same(plus(_),plus(_)) = true;
bool same(optional(_),optional(_)) = true;
bool same(sequence([e1]), sequence([e2])) = same(e1,e2);
bool same(sequence(L1),sequence(L2)) = same(L1[0],L2[0]) && same(sequence(tail(L1)),sequence(tail(L1)));
default bool same(BGFExpression e1, BGFExpression e2) = isAtomic(e1) && isAtomic(e2);

//bool wider("*", "+") = true;
//bool wider("*", "1") = true;
//bool wider("*", "?") = true;
//bool wider("+", "1") = true;
//default bool wider(str ps1, str ps2) = false;

//<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],renameN_renameN(nt,masternt));
tuple[BGFGrammar,CBGFSequence] makeStep(BGFGrammar g,CBGFSequence c,CBGFCommand s)
	= <transform(forward([s]),g), c+s>;

//list[str] checkMatch(str nt, list[BGFProduction] ps1, list[str] cnts, list[BGFProduction] ps2)
list[str] checkMatch(str nt, BGFProduction p1, list[str] cnts, list[BGFProduction] gps)
{
	list[str] good = [];
	for (cnt <- cnts)
	{
		// checking cnt
		ps2 = prodsOfN(cnt,gps);
		if ([BGFProduction p2] := ps2)
		{
			if(same(p1.rhs,p2.rhs)) good += cnt;
			elseif(wider(p2.rhs,p1.rhs)) good += cnt;
			else
				println("No luck matching <cnt>: <p1.rhs> vs <p2.rhs>.");
		}
		else 
			throw "Cannot match one production rule with multiple."; 
	}
	return good;
} 

public void main()
{
	map[str,BGFGrammar] bgfs = ();
	map[str,CBGFSequence] cbgfs = ();
	map[str,map[str,str]] namebind = ();
	println("Reading the grammars...");
	//BGFGrammar master = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
		bgfs[src] = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
	println("Parsing the grammarbase with <size(bgfs)> grammars is done.");
	println("Normalising the grammars...");
	for (src <- sources)
	{
		cbgfs[src] = normal::ANF::normalise(bgfs[src]);
		namebind[src] = ("":""); // ???
		println("Normalising <src> with");
		//iprintln(cbgfs[src]); 
		bgfs[src] = transform(forward(cbgfs[src]),bgfs[src]);
		//if (src == "rascal-c")
		//	iprintln(bgfs[src]);
	}
	println("Starting with the root.");
	list[str] checked = [];
	list[str] tocheck = bgfs["master"].roots; //we assume is has length 1
	str masternt;
	while(!isEmpty(tocheck))
	{
		<masternt,tocheck> = takeOneFrom(tocheck);
		for (src <- sources)
		{
			res = checkMatch(masternt,prodsOfN(masternt,bgfs["master"].prods)[0],bgfs[src].roots,bgfs[src].prods);
			if ([nt] := res)
			{
				println(" * In <src>: maps to <nt>");
				namebind[src][masternt] = nt;
				if (nt != masternt)
					<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],renameN_renameN(nt,masternt));
			}
			else throw "Multiple results for <masternt>: <res>!";
		} 
	}
	//str masternt, nt;
	//PRODSIG ps, masterps;
	//for (src <- sources)
	//{
	//	if(src=="master")
	//	{
	//		masternt = bgfs[src].roots[0];
	//		masterps = prodsig(masternt,bgfs[src].prods);
	//		println(" * In the master grammar, the root is called <masternt>, prodsig <masterps>");
	//		//namemapping?
	//		continue;
	//	}
	//	res = checkMatch(masternt,prodsOfN(masternt,bgfs["master"].prods)[0],bgfs[src].roots,bgfs[src].prods);
	//	if ([nt] := res)
	//	{
	//		println(" * In <src>: maps to <nt>");
	//		namebind[src][masternt] = nt;
	//		if (nt != masternt)
	//			<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],renameN_renameN(nt,masternt));
	//	}
	//	else throw "Multiple results for <masternt>: <res>!";
	//	//if (size(bgfs[src].roots)==1)
	//	//{
	//	//	nt = bgfs[src].roots[0];
	//	//	ps = prodsig(nt,bgfs[src].prods);
	//	//	println(" * In <src>: maps to <nt>, prodsig <ps>");
	//	//	namebind[src][masternt] = nt;
	//	//	if (nt != masternt)
	//	//		<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],renameN_renameN(nt,masternt));
	//	//	if (ps == masterps)
	//	//		println("Equal prodsigs")
	//	//		; // equal prodsigs
	//	//	else
	//	//	{
	//	//		e1 = get1Prod(masternt,bgfs[src]).rhs;
	//	//		e2 = get1Prod(masternt,bgfs["master"]).rhs;
	//	//		if (wider(e1,e2)) 
	//	//			println("More liberal prodsigs")
	//	//			//<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],narrow_widen(e1,e2,innt(masternt)))
	//	//			// too early
	//	//		//(wider(ps,masterps))
	//	//		// TODO: account for multiple steps
	//	//		//<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],narrow_widen(?,?));
	//	//		;
	//	//		else
	//	//			throw "Prodsig mismatch";
	//	//	}
	//	//}
	//	//else
	//	//{
	//	//	println(" * In <src>: unconclusive, looking at definitions of <bgfs[src].roots>");
	//	//	for (r <- bgfs[src].roots)
	//	//		println("    * <r>: prodsig <prodsig(r,bgfs[src].prods)>");
	//	//}
	//	////if (isEmpty(bgfs[src].roots))
	//	////{
	//	////	bgfs[src].roots = toList(topNs(bgfs[src]));
	//	////	println("   * Roots in <src> changed to tops: <bgfs[src].roots>");
	//	////}
	//}
	iprintln(namebind);
	println("Writing CBGFs...");
	for (src <- sources)
		writeCBGF(cbgfs[src],|home:///projects/slps/topics/convergence/guided/bgf/<src>.cbgf|);
	println("Writing BGFs...");
	for (src <- sources)
		writeBGF(bgfs[src],|home:///projects/slps/topics/convergence/guided/bgf/<src>.res.bgf|);
	println("Done.");
}