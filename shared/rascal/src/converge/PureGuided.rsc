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
import transform::XBGF;
import transform::CBGF;
import lib::Rascalware;
//import IO;

list[str] sources =
	//["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];
	["sdf"];

bool conflicted(NameMatch a, NameMatch b)
{
	println("a o b = <a o b>");
	return !isEmpty(a o b);
}

BGFProduction getSingleProd(set[str] ns, BGFProdList ps)
{
	BGFProdList ps1 = [*prodsOfN(n,ps) | n <- ns];
	if (len(ps1)!=1)
		throw "Grammar not in ANF with <ps1>";
	else
		return ps1[0];
}

tuple[NameMatch,BGFProdList,BGFProdList]
	matchProds(NameMatch known, BGFProdList mps, BGFProdList sps)
{
	 // TODO double-check
	BGFProduction p1 = getSingleProd({n | <n,_> <- known},mps);
	BGFProduction p2 = getSingleProd({n | <n,_> <- known},sps);
	
	println("Trying to match production rules:");
	println(" <pp(p1)>\t <pp(analyse::Prodsigs::makesig(p1))>");
	println("   vs");
	println(" <pp(p2)>\t <pp(analyse::Prodsigs::makesig(p2))>");
	// check for strong prodsig-equivalence first
	println("Looking for strong equivalence.");
	if (production(_,_,choice(_)) !:= p1 && production(_,_,choice(_)) !:= p2)
	{
		// match p1.rhs vs p2.rhs
		if (analyse::Prodsigs::eqps(p1,p2))
		{
			nm = analyse::Prodsigs::makenamematch(p1,p2);
			println("Found prodsig-equivalent production rules: <pp(nm)>");
			if (!isEmpty(nm-known))
				println("Will assume that <pp(nm)> after <pp(known)>");
			return <nm, mps - p1, sps - p2>;
		}
	}
	elseif (production(_,_,choice(L1)) := p1 && production(_,_,choice(L2)) := p2)
	{
		// match L1 vs L2
		for (e1 <- L1, e2 <- L2, nonterminal(n1) := e1, nonterminal(n2) := e2)
		{
			pps1 = prodsOfN(n1,mps);
			switch(len(pps1))
			{
				case 0: ep1 = e1;
				case 1: ep1 = pps1[0].rhs;
				default: throw "Grammar not in ANF with <pps1>";
			}
			pps2 = prodsOfN(n2,sps);
			switch(len(pps2))
			{
				case 0: ep2 = e2;
				case 1: ep2 = pps2[0].rhs;
				default: throw "Grammar not in ANF with <pps2>";
			}
			if (analyse::Prodsigs::eqps(e1,e2))
			{
				nm = analyse::Prodsigs::makenamematch(e1,e2);
				println("Found prodsig-equivalent production rules: <pp(nm)>");
				if (!isEmpty(nm-known))
					println("Will assume that <pp(nm)> after <pp(known)>");
				return <nm, mps - p1, sps - p2>;
			}
		}
	}
	else
		throw "Choice vs no choice";
	//println("<pp(analyse::Prodsigs::makesig(p1))> vs <pp(analyse::Prodsigs::makesig(p2))>");
	//println("Equality: <analyse::Prodsigs::eqps(p1,p2)>; equivalence: <analyse::Prodsigs::weqps(p1,p2)>");
	for (p1 <- ps1, p2 <- ps2, analyse::Prodsigs::eqps(p1,p2))
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
	for (p1 <- ps1, p2 <- ps2, analyse::Prodsigs::weqps(p1,p2))
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

void converge(BGFGrammar master, BGFGrammar servant)
{
	println("Master grammar:\n<pp(master)>");
	CBGFSequence acbgf = []; // normalisation
	CBGFSequence ncbgf = []; // nominal matching
	CBGFSequence scbgf = []; // structural matching
	//println("Input: <src>");
	println("Normalising the grammar...");
	ncbgf = normal::ANF::normalise(servant);
	servant = transform(forward(ncbgf),servant);
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
	println("<pp(servant)>");
}

public void main()
{
	master = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
		converge(master,loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|));
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