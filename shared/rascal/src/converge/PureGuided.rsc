@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module converge::PureGuided

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import analyse::Prodsigs;
import analyse::Metrics;
import normal::ANF;
import export::BNF;
import io::ReadBGF;
import io::WriteBGF;
import transform::XBGF;
import transform::CBGF;
import lib::Rascalware;
import IO;

list[str] sources =
	//["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];
	["xsd"];
	// atom/expr: antlr, dcg
	// arg/string: ecore, rascal-a
	// good: emf, jaxb, om, rascal-c, sdf, xsd
	// multiroot: python
	// unknown: txl

bool conflicted(NameMatch a, NameMatch b)
{
	println("a o b = <a o b>");
	return !isEmpty(a o b);
}

BGFProduction getSingleProd(str n, BGFProdList ps)
{
	//BGFProdList ps1 = [*prodsOfN(n,ps) | n <- ns]; // Y SO complicated?
	BGFProdList ps1 = prodsOfN(n,ps);
	if (len(ps1)!=1)
		throw "Grammar not in ANF with <ps1>";
	else
		return ps1[0];
}

BGFProduction unwind(BGFProduction p1, BGFProdList ps1)
	= (production(_,_,nonterminal(n)) := p1 && n in definedNs(ps1))? getSingleProd(n,ps1) : p1;

bool strongEq(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::eqps(unwind(p1,ps1),unwind(p2,ps2));

bool weakEq(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::weqps(unwind(p1,ps1),unwind(p2,ps2));

tuple[NameMatch,BGFProdList,BGFProdList]
	matchProds(NameMatch known, BGFProdList mps, BGFProdList sps)
{
	BGFProdList ps1 = [*prodsOfN(n,mps) | <n,_> <- known];
	BGFProdList ps2 = [*prodsOfN(n,sps) | <n,_> <- known];
	println("Trying to match production rules:");
	for (p <- ps1) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	println("   vs");
	for (p <- ps2) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	// check for strong prodsig-equivalence first
	println("Looking for strong equivalence.");
	//println("<pp(analyse::Prodsigs::makesig(p1))> vs <pp(analyse::Prodsigs::makesig(p2))>");
	//println("Equality: <analyse::Prodsigs::eqps(p1,p2)>; equivalence: <analyse::Prodsigs::weqps(p1,p2)>");
	for (p1 <- ps1, p2 <- ps2, strongEq(p1,mps,p2,sps))
	{
		nm = analyse::Prodsigs::makenamematch(p1,p2);
		//println("Found prodsig-equivalent production rules:\n <pp(p1)>   &\n <pp(p2)>");
		println("Found prodsig-equivalent production rules: <pp(nm)>");
		if (!isEmpty(nm-known))
			println("Will assume that <pp(nm)> after <pp(known)>");
		return <nm, mps - p1, sps - p2>; 
	}
	// check for weak prodsig-equivalence now
	println("Looking for weak equivalence.");
	for (p1 <- ps1, p2 <- ps2, weakEq(p1,mps,p2,sps))
	{
		nm = analyse::Prodsigs::makenamematch(p1,p2);
		//println("Found weakly prodsig-equivalent production rules:\n <pp(p1)>   &\n <pp(p2)>");
		println("Found weakly prodsig-equivalent production rules: <pp(nm)>");
		if (conflicted(nm,known))
			println("Naming conflict, reconsider.");
		else
		{
			if (!isEmpty(nm-known))
				println("Will assume that <pp(nm)> after <pp(known)>");
			return <nm, mps - p1, sps - p2>;
		} 
	}
	//println(assumeRenamings(servant,known));
	println("No match found.");
}

BGFProdList assumeRenamings(BGFProdList where, NameMatch naming)
{
	BGFProdList ps = where;
	for (<n1,n2> <- naming)
		if (n1 != n2 && n2 in allNs(ps) && n1 != "")
			ps = transform(forward([renameN_renameN(n2,n1)]),grammar([],ps)).prods;
	return ps;
}

//BGFGrammar
NameMatch converge(BGFGrammar master, BGFGrammar servant)
{
	println("Master grammar:\n<pp(master)>");
	CBGFSequence acbgf = []; // normalisation
	CBGFSequence ncbgf = []; // nominal matching
	CBGFSequence scbgf = []; // structural matching
	//println("Input: <src>");
	println("Normalising the grammar...");
	acbgf = normal::ANF::normalise(servant);
	servant = transform(forward(acbgf),servant);
	//iprintln(ncbgf);
	println("Servant grammar:\n<pp(servant)>");
	println("Starting with the root: <master.roots>, <servant.roots>.");
	// TODO: multiple roots
	NameMatch known = {<master.roots[0],servant.roots[0]>};
	ps1 = master.prods;
	ps2 = assumeRenamings(servant.prods, known);
	int cx = 10;
	//println("Let\'s go!\n<isEmpty(ps1)>");
	while(!isEmpty(ps1))
	{
		print("...<cx>...");
		cx -= 1;
		<nnm,ps1a,ps2a> = matchProds(known, ps1, ps2);
		ps1 = ps1a;
		ps2 = assumeRenamings(ps2a,nnm);
		known = known + nnm;
		if (cx==0)
			break;
	}
	println("Done with the grammar.");
	println("Nominal matching: <pp(known)>");
	for (<a,b> <- known)
		if (a==b)
			;
		else
			ncbgf += renameN_renameN(b,a);
	// Assume nominal matching!
	//servant = transform(forward(ncbgf),servant);
	println("<pp(servant)>");
	//return servant;
	return known;
}

public void main()
{
	master = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
	{
		//BGFGrammar res = 
		NameMatch res = converge(master,loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|));
		//writeBGF(res,|home:///projects/slps/topics/convergence/guided/bgf/<src>.almost.bgf|);
		writeFile(|home:///projects/slps/topics/convergence/guided/bgf/<src>.almost.bnf|,pp(res));
	}
	println("Done.");
}

BGFGrammar loadSimpleGrammar(loc l)
{
	BGFGrammar g = readBGF(l), q;
	//return g;
	 //we simplify our life by converting built-in types ("values") to regular nonterminals
	if (/val(string()) := g)
		q = transform([replace(val(string()),nonterminal("STRING"),globally())],g);
	else
		q = g;
	if (/val(integer()) := q)
		q = transform([replace(val(integer()),nonterminal("INTEGER"),globally())],q);
	//return <g,q>;
	return q;
}