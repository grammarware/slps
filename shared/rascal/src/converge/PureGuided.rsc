@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module converge::PureGuided

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import analyse::Prodsigs;
import analyse::Metrics;
import analyse::CarveOut;
import normal::ANF;
import export::BNF;
import io::ReadBGF;
import io::WriteBGF;
import transform::XBGF;
import transform::library::Core;
import transform::CBGF;
import lib::Rascalware;
import IO;
import String;
import Relation;
import analyse::Layers;
import analyse::Associativity;

int verbose = 0;

void report(int level, str s)
{
	if (level<=verbose) println(s);
}

list[str] sources =
	//["python"]; list[str] other =
	["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"]
	-
	["ecore"];
	// ecore is fundamentally different

BGFProduction getSingleProd(str n, BGFProdList ps)
{
	//BGFProdList ps1 = [*prodsOfN(n,ps) | n <- ns]; // Y SO complicated?
	BGFProdList ps1 = prodsOfN(n,ps);
	if (len(ps1)!=1)
		report(1,"Unexpectedly many production rules of <n>! Grammar not in ANF?");
	return ps1[0];
}

BGFProduction unwind(BGFProduction p1, BGFProdList ps1)
	= (production(_,_,nonterminal(n)) := p1 && n in definedNs(ps1))? getSingleProd(n,ps1) : p1;

bool strong(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::eqps(unwind(p1,ps1),unwind(p2,ps2));

bool weak(BGFProduction p1, BGFProdList ps1, BGFProduction p2, BGFProdList ps2)
	= analyse::Prodsigs::weqps(unwind(p1,ps1),unwind(p2,ps2));

NameMatch tryMatch(	NameMatch nm, NameMatch known,
					BGFProduction p1, BGFProdList mps,
					BGFProduction p2, BGFProdList sps)
{
	report(1,"Found prodsig-equivalent production rules: <pp(nm)>");
	p1a = unwind(p1,mps);
	p2a = unwind(p2,sps);
	if (p1 != p1a && p1 != p2a)
	{
		nm2 = analyse::Prodsigs::makenamematch(p1a,p2a);
		report(1,"More prodsig-equivalent production rules: <pp(nm2)>");
		nm += nm2;
	}
	truenm = {};
	for (<a,b> <- nm-known)
		if ((a==b) && a in known<0>)
			report(2,"Reconfirmed <a>");
		else
		{
			report(2,"Will assume that <a> == <b>");
			truenm += <a,b>;
		}
	return truenm;
}

BGFProdList assumeRenamings(BGFProdList where, NameMatch naming)
{
	BGFProdList ps = [p | p <- where, <"",p.lhs> notin naming];
	for (<n1,n2> <- naming)
		if (n1 != n2 && n2 in allNs(ps) && n1 notin [""])
			// dirty
			//ps = transform::library::Core::performRenameN(n2,n1,grammar([],ps)).prods;
			//if (n1 in ["STRING","INTEGER"])
			//	ps = transform(forward([replace_replace(nonterminal(n2),nonterminal(n1),globally())]),grammar([],ps)).prods;
			//else
				ps = transform(forward([renameN_renameN(n2,n1)]),grammar([],ps)).prods;
	return ps;
}

int cx=20;

set[NameMatch] resolveNames(NameMatch known, BGFProdList mps, BGFProdList sps)
{
	cx -= 1;
	if (cx==0) throw "Limit exceeded";
	NameMatch nnm;
	//BGFProdList ps1, ps2, ps1a, ps2a;
	
	set[BGFProduction] ps1 = {*prodsOfN(n,mps) | <n,_> <- known};
	set[BGFProduction] ps2 = {*prodsOfN(n,sps) | <n,_> <- known};
	if (isEmpty(ps1))
	{
		if (!isEmpty(ps2))
			report(2,"Disregarded servant production rules: <ps2>");
		cx += 1;
		return {known};
	}

	report(2,"Trying to match production rules:");
	for (p <- ps1) report(2," <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	report(2,"   vs");
	for (p <- ps2) report(2," <pp(p)>\t <pp(analyse::Prodsigs::makesig(p))>");
	
	// check for strong prodsig-equivalence first, then for the weak one
	for (bool(BGFProduction,BGFProdList,BGFProduction,BGFProdList) eq <- [strong,weak])
	{
		report(3,"Looking for <split(" ",split("(","<eq>")[0])[1]> equivalence.");
		for (p1 <- ps1, p2 <- ps2, eq(p1,mps,p2,sps))
		{
			matches = analyse::Prodsigs::makenamematches(p1,p2);
			if (isEmpty(matches))
			{cx += 1;
				return {}; //rollback?
				}
			nms = {};
			for (nm <- matches)
			{
				report(3,"Trying <pp(nm)>...");
				truenm = tryMatch(nm,known,p1,mps,p2,sps);
				report(3,"Got <pp(truenm)> with <pp(known)>...");
				if (conflicted(truenm,known))
					report(2,"Naming conflict: <pp(truenm)> vs <pp(known)>, reconsider.");
				else
				{
					newmatch = resolveNames(known + truenm, mps - p1, assumeRenamings(sps - p2, truenm));
					if (!isEmpty(newmatch))
						nms += newmatch;
				} 
			}
			if (!isEmpty(nms))
			{
				cx += 1;
				return nms;
			}
		}
	}
	report(1,"No match found in <pp(ps1)> & <pp(ps2)>.");
	//throw "ERROR";
	// END
	cx += 1;
	return {};
	
}

bool conflicted(NameMatch n, NameMatch m)
{
	n1 = carrierX(n,{""});
	m1 = carrierX(m,{""});
	return !isEmpty(invert(n1) o m1) || !isEmpty(n1 o invert(m1));
}

//BGFGrammar
tuple[NameMatch,BGFGrammar] converge(BGFGrammar master, BGFGrammar servant, str src)
{
	report(3,"Master grammar:\n<pp(master)>");
	CBGFSequence mcbgf = []; // mutation
	CBGFSequence acbgf = []; // normalisation
	CBGFSequence ncbgf = []; // nominal resolution
	CBGFSequence scbgf = []; // structural resolution
	report(0,"Input: <src>");
	report(0,"Mutating the grammar...");
	NameMatch res = detectLayers(servant);
	if (!isEmpty(res))
	{
		mcbgf += removeLayers(res, servant);
		servant = transform(forward(mcbgf), servant);
		report(1,"Delayering successful.");
	}
	res = detectAssociativity(servant);
	if (!isEmpty(res))
	{
		mcbgf += removeAssociativity(res, servant);
		servant = transform(forward(removeAssociativity(res, servant)), servant);
		report(1,"Associativity detection successful.");
	}
	
	if(isEmpty(mcbgf))
		report(1,"No mutation necessary.");
	report(0,"Normalising the grammar...");
	acbgf = normal::ANF::normalise(servant);
	servant = transform(forward(acbgf),servant);
	report(3,"Servant grammar:\n<pp(servant)>");
	report(2,"Starting with the root: <master.roots>, <servant.roots>.");
	report(2,"------------------------------");
	report(0,"Resolving names in the grammar...");
	NameMatch known;
	 //= {<master.roots[0],servant.roots[0]>};
	set[NameMatch] nknown = {};

	for (rootmatch <- {<r1,r2> | r1 <- master.roots, r2 <- servant.roots})
		nknown += resolveNames({rootmatch}, master.prods, assumeRenamings(servant.prods, {rootmatch}));
	
	if (len(nknown)==1)
		known = getOneFrom(nknown);
	elsefor (k <- nknown)
		report(2,"Got version: <pp(k)>");

	report(0,"[!] Nominal resolution: <pp(known)>");
	for (<a,b> <- known)
		if (a==b)
			;
		elseif (a=="")
			;
		else
			ncbgf += renameN_renameN(b,a);
	// Assume nominal matching!
	for (<"",b> <- known)
		ncbgf += carveOutN(b,transform(forward(ncbgf),servant));
	//servant = transform(forward(ncbgf),servant);
	// Assume nominal matching!
	servant = transform(forward(ncbgf),servant);
	report(3,"<pp(servant)>");

	report(0,"Resolving structural mismatches in the grammar...");
	
	scbgf = resolveStructures(known, master, servant);
	servant = transform(forward(scbgf), servant);
	
	report(0,"Done with the grammar.");
	return <known,servant>;
}

CBGFSequence resolveStructures(NameMatch known, BGFGrammar m, BGFGrammar s)
{
	CBGFSequence c = [];
	// resolving differences in roots (starting nonterminals)
	if (m.roots != s.roots)
	{
		report(3,"M: <m.roots>\nS: <s.roots>");
		roots2go = s.roots - m.roots;
		c += reroot_reroot(s.roots,m.roots);
		s = transform(forward(c),s);
		for (r <- roots2go)
			if (r in known<1>)
				throw "Do something about nonterminal <r> first!";
			//else
			//	c += carveOutN(r,s);
			// ^^^^^^^^^^^^^^^^^^^^ this will happen naturally below when we sift through the production rules
	}
	// going through the production rules
	//ps1 = m.prods;
	//ps2 = s.prods;
	//for (p1 <- ps1, p2 <- ps2, strong(p1,p2))
	//{
	//	ps1 -= 
	//}
	ps = s.prods - m.prods;
	for (p <- ps)
	{
		report(0,"Not yet matching production <pp(p)>");
		if (p.lhs notin usedNs(s) && p.lhs notin definedNs(m))
		{
			c += eliminate_introduce([p]);
			ps -= p; //needed?
			continue;
		} 
		pms = prodsOfN(p.lhs,m.prods);
		report(0,"Candidates: <pp(pms)>");
		// option 1: permutations:
		if (sequence(L1) := p.rhs && [production(_,_,sequence(L2))] := pms)
		{
			if (seteq(L1,L2))
				c += permute_permute(p,pms[0]);
			else
			{
				es1 = L1-L2;
				es2 = L2-L1;
				report(0,"<pp(es1)> vs <pp(es2)>");
				for (e1 <- es1)
					if (star(e1a) := e1 && plus(e1a) in es2)
					{
						c += narrow_widen(star(e1a),plus(e1a),innt(p.lhs));
						es1 -= e1;
						es2 -= plus(e1a);
					}
					else
						throw "Given up";
				if (!isEmpty(es1) || !isEmpty(es2))
					throw "<pp(es1)> and <pp(es2)> remained unmatched!";
			}
		}
		elseif (star(e1a) := p.rhs && [production(_,_,plus(e1a))] := pms)
		{
			c += narrow_widen(star(e1a),plus(e1a),innt(p.lhs));
		}
		else
		{
			report(0,pp(p));
			throw "Dunno :(";
		}
	}
	//if (!isEmpty(ps))
	//{
	//	report(0,pp(ps));
	//	throw "";
	//}
	iprintln(c);
	return c;
}

public void main()
{
	master = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	for (src <- sources)
	{
		NameMatch res;
		BGFGrammar g;
		<res,g> = converge(master,loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|),src);
		writeFile(|home:///projects/slps/topics/convergence/guided/bgf/<src>.almost.bnf|,replaceAll(pp(res),", ",",\n"));
		writeSimpleGrammar(g,|home:///projects/slps/topics/convergence/guided/bgf/<src>.converging.bgf|);
	}
	report(2,"Done.");
}

BGFGrammar loadSimpleGrammar(loc l)
{
	BGFGrammar g = readBGF(l);
	 //we simplify our life by converting built-in types ("values") to regular nonterminals
	if (/val(string()) := g)
		g = transform([replace(val(string()),nonterminal("STRING"),globally())],g);
	if (/val(integer()) := g)
		g = transform([replace(val(integer()),nonterminal("INTEGER"),globally())],g);
	return g;
}

void writeSimpleGrammar(BGFGrammar g, loc l)
{
	 // putting built-in types ("values") back before serialisation
	if (/nonterminal("STRING") := g)
		g = transform([replace(nonterminal("STRING"),val(string()),globally())],g);
	if (/nonterminal("INTEGER") := g)
		g = transform([replace(nonterminal("INTEGER"),val(integer()),globally())],g);
	writeBGF(g,l);
}
