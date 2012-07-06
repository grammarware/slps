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
import String;
import Relation;

list[str] sources =
	//["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];
	//["emf","jaxb","om","rascal-c","sdf","xsd"];
	["txl"];
	// atom/expr: antlr, dcg
	// arg/string: ecore, rascal-a
	// good: emf, jaxb, om, rascal-c, sdf, xsd
	// multiroot: python
	// unknown: txl

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

bool strong(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::eqps(unwind(p1,ps1),unwind(p2,ps2));

//bool strong(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
//{
//	println("<unwind(p1,ps1)> vs <unwind(p2,ps2)>");
//	s1 = analyse::Prodsigs::makesig(unwind(p1,ps1));
//	s2 = analyse::Prodsigs::makesig(unwind(p2,ps2));
//	println("<s1> vs <s2>");
//	println("EQ: <analyse::Prodsigs::eqps(s1,s2)>; EQUIV: <analyse::Prodsigs::weqps(s1,s2)>");
//	return analyse::Prodsigs::eqps(unwind(p1,ps1),unwind(p2,ps2));
//}

bool weak(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::weqps(unwind(p1,ps1),unwind(p2,ps2));

NameMatch tryHypothesis(NameMatch known, BGFProdList mps, BGFProdList sps, int cx)
{
	NameMatch nnm;
	BGFProdList ps1, ps2, ps1a, ps2a;
	while(!isEmpty(mps))
	{
		print("...<cx>...");
		cx -= 1;
		//<nnm,ps1a,ps2a> = matchProds(known, mps, sps);
		// BEGIN
		BGFProdList ps1 = [*prodsOfN(n,mps) | <n,_> <- known];
		BGFProdList ps2 = [*prodsOfN(n,sps) | <n,_> <- known];
		println("Trying to match production rules:");
		for (p <- ps1) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
		println("   vs");
		for (p <- ps2) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	
		// check for strong prodsig-equivalence first, then for the weak one
		megabreak = false;
		for (bool(BGFProduction,BGFProdList,BGFProduction,BGFProdList) eq <- [strong,weak])
		{
			println("Looking for <eq> equivalence.");
			for (p1 <- ps1, p2 <- ps2, eq(p1,mps,p2,sps))
			{
				for (nm <- analyse::Prodsigs::makenamematches(p1,p2))
				{
					println("Trying <nm>...");
					truenm = tryMatch(nm,known,p1,mps,p2,sps);
					println("Got <truenm>...");
					if (!isEmpty(invert(truenm) o known))
						println("Naming conflict, reconsider.");
					else
					{
						nnm = truenm;
						ps1a = mps - p1;
						ps2a = sps - p2;
						megabreak = true;
						break;
					} 
				}
				if (megabreak) break;
			}
			if (megabreak) break;
		}
		println("No match found.");
		// END
		mps = ps1a;
		sps = assumeRenamings(ps2a,nnm);
		known = known + nnm;
		if (cx==0)
			break;
	}
	return known;
}

tuple[NameMatch,BGFProdList,BGFProdList]
	matchProds(NameMatch known, BGFProdList mps, BGFProdList sps)
{

	BGFProdList ps1 = [*prodsOfN(n,mps) | <n,_> <- known];
	BGFProdList ps2 = [*prodsOfN(n,sps) | <n,_> <- known];
	println("Trying to match production rules:");
	for (p <- ps1) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	println("   vs");
	for (p <- ps2) println(" <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");

	//println("<pp(analyse::Prodsigs::makesig(p1))> vs <pp(analyse::Prodsigs::makesig(p2))>");
	//println("Equality: <analyse::Prodsigs::eqps(p1,p2)>; equivalence: <analyse::Prodsigs::weqps(p1,p2)>");
	
	// check for strong prodsig-equivalence first, then for the weak one
	for (bool(BGFProduction,BGFProdList,BGFProduction,BGFProdList) eq <- [strong,weak])
	{
		println("Looking for <eq> equivalence.");
		for (p1 <- ps1, p2 <- ps2, eq(p1,mps,p2,sps))
		{
			//nm = analyse::Prodsigs::makenamematch(p1,p2);
			//nms = analyse::Prodsigs::makenamematches(p1,p2);
			//if (len(nms)==1)
			//	nm = getOneFrom(nms);
			//else
			//	println("----multiple versions!");
			//println("!!!nm  = <nm>");
			//println("!!!nms = <nms>");
			//
			for (nm <- analyse::Prodsigs::makenamematches(p1,p2))
			{
				truenm = tryMatch(nm,known,p1,mps,p2,sps);
				
				if (!isEmpty(invert(truenm) o known))
					println("Naming conflict, reconsider.");
				else
					return <truenm, mps - p1, sps - p2>; 
			}
		}
	}
	println("No match found.");
}

NameMatch tryMatch(	NameMatch nm, NameMatch known,
					BGFProduction p1, BGFProdList mps,
					BGFProduction p2, BGFProdList sps)
{
	println("Found prodsig-equivalent production rules: <pp(nm)>");
	p1a = unwind(p1,mps);
	p2a = unwind(p2,sps);
	if (p1 != p1a && p1 != p2a)
	{
		nm2 = analyse::Prodsigs::makenamematch(p1a,p2a);
		println("More prodsig-equivalent production rules: <pp(nm2)>");
		nm += nm2;
	}
	truenm = {};
	for (<a,b> <- nm-known)
		if ((a==b) && a in known<0>)
			println("Reconfirmed <a>");
		else
		{
			println("Will assume that <a> == <b>");
			truenm += <a,b>;
		}
	//if (isEmpty(invert(truenm) o known))
		return truenm;
 //	println("Naming conflict, reconsider.");
	//return {};
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

	known = tryHypothesis(known,ps1,ps2,2);

	println("Done with the grammar.");
	println("Nominal matching: <pp(known)>");
	for (<a,b> <- known)
		if (a==b)
			;
		elseif (a=="")
			;
		else
			ncbgf += renameN_renameN(b,a);
	// Assume nominal matching!
	servant = transform(forward(ncbgf),servant);
	println("<pp(servant)>");
	//return servant;
	return known;
}

public void main()
{
	//Signature s1 = {<"a",fpplus()>,<"b",fpopt()>};
	//Signature s2 = {<"x",fpplus()>,<"y",fpopt()>};
	//Signature s1 = {<"a",fpplus()>,<"b",fpplus()>,<"c",fpmany([fpnt(),fpplus()])>,<"z",fpmany([fpnt(),fpplus()])>};
	//Signature s2 = {<"x",fpplus()>,<"y",fpmany([fpnt(),fpplus()])>};
	//println("\t<pp(s1)>\nvs\n\t<pp(s2)>");
	//println("<analyse::Prodsigs::makenamematches(s1,s2)>");
	//return;
	master = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
	{
		//BGFGrammar res = 
		NameMatch res = converge(master,loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|));
		//writeBGF(res,|home:///projects/slps/topics/convergence/guided/bgf/<src>.almost.bgf|);
		writeFile(|home:///projects/slps/topics/convergence/guided/bgf/<src>.almost.bnf|,replaceAll(pp(res),", ",",\n"));
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