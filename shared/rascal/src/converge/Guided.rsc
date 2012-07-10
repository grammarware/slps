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
	//["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];
	["sdf"];

alias PRODSIG = list[list[tuple[str,BGFExpression]]];

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
bool wider(star(_), optional(_)) = true;
bool wider(plus(_), nonterminal(_)) = true;
bool wider(sequence([e1]),e2) = wider(e1,e2);
bool wider(e1,sequence([e2])) = wider(e1,e2);
//bool wider(sequence([e1,*L1]),sequence([*L2a,e2,*L2b]))
bool wider(sequence([*L1a,e1,*L1b]),sequence([e2,*L2]))
	= wider(e1,e2)
	&& wider(sequence(L1a+L1b),sequence(L2));
bool wider(sequence([*L1a,e1,*L1b]),e2) = wider(e1,e2);
//bool wider(sequence(L1),sequence(L2)) = wider(L1[0],L2[0]) && wider(sequence(tail(L1)),sequence(tail(L1)));
default bool wider(BGFExpression e1, BGFExpression e2) = same(e1,e2);

//bool isAtomic(val(_)) = true;
bool isAtomic(nonterminal(_)) = true;
default bool isAtomic(BGFExpression _) = false;

bool same(star(_),star(_)) = true;
bool same(plus(_),plus(_)) = true;
bool same(optional(_),optional(_)) = true;
bool same(sequence([e1]), sequence([e2])) = same(e1,e2);
bool same(sequence(L1),sequence(L2)) = same(L1[0],L2[0]) && same(sequence(tail(L1)),sequence(tail(L1)));
bool same(nonterminal(_), nonterminal(_)) = true;
default bool same(BGFExpression e1, BGFExpression e2) = false;
//default bool same(BGFExpression e1, BGFExpression e2) = isAtomic(e1) && isAtomic(e2);

tuple[BGFGrammar,CBGFSequence] makeStep(BGFGrammar g,CBGFSequence c,CBGFCommand s)
	= <transform(forward([s]),g), c+s>;

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
			//else println("No luck matching <cnt>: <p1.rhs> vs <p2.rhs>.");
		}
		else 
			throw "Cannot match one production rule with multiple."; 
	}
	return good;
}

map[str,list[str]] allCandidates(list[BGFProduction] ps1, list[BGFProduction] ps2)
	= allCandidates(ps1[0], ps2[0]);

map[str,list[str]] allCandidates(BGFProduction p1, BGFProduction p2)
	= allCandidates(p1.rhs, p2.rhs);

map[str,list[str]] addto(map[str,list[str]] repo, str where, str what)
{
	if (where in repo)
		repo[where] += [what];
	else
		repo[where] = [what];
	return repo;
}

map[str,str] makeSig([]) = ("":"");
map[str,str] makeSig([BGFProduction p]) = makeSig(p);
map[str,str] makeSig(list[BGFProduction] ps) = println("Not implemented.");

map[str,str] makeSig(BGFProduction p) = makeSig(p.rhs);

map[str,str] makeSig(nonterminal(str X)) = (X:"1");
map[str,str] makeSig(plus(nonterminal(str X))) = (X:"+");
map[str,str] makeSig(star(nonterminal(str X))) = (X:"*");
map[str,str] makeSig(sequence(L)) = makeSig(L);
map[str,str] makeSig(val(string())) = ("STRING": "1");
map[str,str] makeSig(val(integer())) = ("INTEGER": "1");
map[str,str] makeSig(empty()) = ("": "?");
default map[str,str] makeSig(BGFExpression e) = println("Unhandled case in makeSig: <e>");

map[str,str] makeSig(list[BGFExpression] L1)
{
	map[str,str] sig = ();
	for (e1 <- L1)
		switch (e1)
		{
			case nonterminal(str X):
				if (X in sig) sig[X] += "1";
				else sig[X] = "1";
			case plus(nonterminal(str X)):
				if (X in sig) sig[X] += "+";
				else sig[X] = "+";
			case star(nonterminal(str X)):
				if (X in sig) sig[X] += "*";
				else sig[X] = "*";
			default:
				println("Unhandled case in makeSig: <e1>");
		}
	return sig;
}

list[rel[str,str]] matchSigs(map[str,str] sig1, map[str,str] sig2)
{
	list[rel[str,str]] versions = [];
	rel[str,str] version = {};
	bool fs;
	//println(" Matching <sig1> with <sig2>...");
	//for (x:s <- sig1)
	for (x <- sig1)
	{
		str s = sig1[x];
		//println("Matching of <x> : <s>...");
		for (y <- sig2, sig2[y]==s)
		{
			<fs,nv> = mergeVersions({<x,y>},matchSigs(sig1-(x:s),sig2-(y:s)));
			if (fs)
				versions = nv;
			//println("Versions: <versions>");
		}
	}
	// unused in matches
	return versions + {<"",y> | y:_ <- sig2};
}

list[rel[str,str]] reduceSearchSpace(list[rel[str,str]] vs)
{
	list[rel[str,str]] ss = [];
	for (v <- vs)
	{
		bool inc = true;
		for (y <- ss)
			if (novoid(v) < novoid(y))
				inc = false;
		if (inc)
			ss += v;
	}
	return ss;
}

rel[str,str] novoid (rel[str,str] v) = {<i1,i2> | <i1,i2> <- v, i1 != ""};

tuple[bool,list[rel[str,str]]] mergeVersions(rel[str,str] n, list[rel[str,str]] vs)
{
	//println("Merging <n> with <vs>...");
	if (isEmpty(vs))
		return <false,vs>;
	else
		return <true,[v+n | v <- vs]>;
}

map[str,list[str]] allCandidates(BGFExpression e1, BGFExpression e2)
{
	map[str,list[str]] cands = ();
	println(" --- Searching for candidates of <e1> with <e2>...");
	switch(e1)
	{
		case nonterminal(n1):
			if (nonterminal(n2) := e2)
				cands = addto(cands, n1, n2);
			else fail;
		case plus(e1a):
			if (plus(e2a) := e2)
				cands += allCandidates(e1a,e2a);
			elseif (star(e2b) := e2)
				cands += allCandidates(e1a,e2b);
			else fail;
		case star(e1a):
			if (star(e2a) := e2)
				cands += allCandidates(e1a,e2a);
			else fail;
		case sequence(L1):
			if (sequence(L2) := e2)
			{
				//println("--! <L1> vs <L2>");
				//println("++! <makeSig(L1)> vs <makeSig(L2)>");
				//println("==! <matchSigs(makeSig(L1),makeSig(L2))>");
				list[rel[str,str]] versions = reduceSearchSpace(matchSigs(makeSig(L1),makeSig(L2)));
				if (isEmpty(versions))
				{
					println("     * Hopeless matching of <L1> with <L2>.");
					fail;
				}
				println("=====\> <versions>");
			}
			else fail;
		case sequence([e1a]):
		{
			//println(" Hardly trying <e1a> with <e2>...");
			if (sequence([e2a]) := e2)
				cands += allCandidates(e1a,e2a);
			// TODO seq to seq
		}
		default:
			for (sequence([*L1,e2a,*L2]) := e2)
			{
				//println(" Trying <e1> against <e2a>...");
				cands += allCandidates(e1,e2a);
			}
	}
	//iprintln(cands);
	return cands;
}

list[BGFProduction] assumeRenamings(list[BGFProduction] where, rel[str,str] naming)
{
	list[BGFProduction] ps = where;
	for (<n1,n2> <- naming)
		if (n1 != n2 && /nonterminal(n2) := ps)
		{
			<g,_> = makeStep(grammar([],ps),[],renameN_renameN(n2,n1));
			ps = g.prods;
		}
	return ps;
}

//list[rel[BGFProduction,BGFProduction]] matchProds(list[BGFProduction] ps1, list[BGFProduction] ps2)
//tuple[bool,rel[str,str],CBGFSequence] tryHypothesis(rel[str,str] approved, rel[str,str] version, BGFGrammar master, BGFGrammar servant, CBGFSequence c)
tuple[rel[BGFProduction,BGFProduction],rel[str,str]] matchProds(list[BGFProduction] ps1, list[BGFProduction] ps2)
{
	rel[BGFProduction,BGFProduction] approved = {};
	rel[str,str] new = {};
	if (isEmpty(ps1))
	{
		if (!isEmpty(ps2))
			println("Unable to match anything with <ps2>.");
		return <{},{}>;
	}
	//println("    * Production signatures:");
	//for (p <- ps1)
	//	println("     * <makeSig(p)>");
	//println("    * vs");
	//for (p <- ps2)
	//	println("     * <makeSig(p)>");
	map[BGFProduction,list[BGFProduction]] pmatches = ();
	for (p1 <- ps1)
	{
		pmatches[p1] = [];
		for (p2 <- ps2)
			// TODO: allow liberation
			if (range(makeSig(p1)) == range(makeSig(p2)))
				//return <p1,p2> + matchProds(ps1 - p1, ps2 - p2);
				pmatches[p1] = pmatches[p1] + p2;
	}
	list[BGFProduction] unmatched1 = [], unmatched2 = ps2, unmatched3, unmatched4, lastmatched = [];
	for (m <- pmatches)
		if (size(pmatches[m])==1)
		{
			approved += <m,pmatches[m][0]>;
			unmatched2 -= pmatches[m][0];
		}
		else
			unmatched1 += m;
	// disregarding a reflexive chain production
	for (p:production(_,s,nonterminal(s)) <- unmatched2)
		unmatched2 -= p;
	for (<p1,p2> <- approved)
	{
		// TODO pretty-print
		println("    * Productions <p1> and <p2> match.");
		list[rel[str,str]] versions = reduceSearchSpace(matchSigs(makeSig(p1),makeSig(p2)));
		if (size(versions)==1)
			new += versions[0];
		else
			println("      * too many versions: <versions>");
		//println("      * ergo <versions>");
	}
	println("We know <new>");
	unmatched3 = assumeRenamings(unmatched2, new);
	// last try
	// TODO: should be prodsig-aware just for the sake of completeness
	for (p1 <- unmatched1, p1 in unmatched3)
	{
		println("    * Production <p1> matches barely.");
		approved += <p1,unmatched2[indexOf(unmatched3,p1)]>;
		unmatched1 -= p1;
		lastmatched += unmatched2[indexOf(unmatched3,p1)];
	}
	unmatched2 -= lastmatched;
	if (isEmpty(unmatched1) && isEmpty(unmatched2))
		;
	elseif (size(unmatched1)==1 && size(unmatched2)==1)
		approved += <unmatched1[0],unmatched2[0]>;
	else
		throw "Utterly unable to match <unmatched1> with <unmatched2>.";
	return <approved,new>;
	iprintln(unmatched1);
	iprintln(unmatched2);
	return;
	p1 = getOneFrom(ps1);
	for (p2 <- ps2)
	{
		ms = reduceSearchSpace(matchSigs(makeSig(ps1[0]),makeSig(ps2[0])));
		if (size(ms)==1)
			return {<p1,p2>}+matchProds(ps1 - p1, ps2 - p2);
		else
			//throw "Unexpected search space size: <ms>";
		{
			//for (m <- ms)
				println("Unexpected search space size: <ms>");
			return;
		}
	}
	throw "Unable to match the production rule: <p1>";
	//ms = reduceSearchSpace(matchSigs(makeSig(ps1[0]),makeSig(ps2[0])));
	//if (size(ps1)==1 && size(ps2)==1 && size(ms)==1)
	//	return {<ps1[0],ps2[0]>};
	return;
} 

// tuple[bool,list[rel[str,str]],CBGFSequence] tryHypothesis(list[rel[str,str]] approved, rel[str,str] version, BGFGrammar master, BGFGrammar servant, CBGFSequence c)

tuple[bool,rel[str,str],CBGFSequence] tryHypothesis(rel[str,str] approved, rel[str,str] version, rel[BGFProduction,BGFProduction] prodversion, BGFGrammar master, BGFGrammar servant, CBGFSequence cbgf)
{
	// TODO: fail?  
	for (<p1,p2> <- prodversion)
	{
		list[rel[str,str]] versions = reduceSearchSpace(matchSigs(makeSig(p1),makeSig(p2)));
		if (isEmpty(versions))
		{
			println("Reached a fixed point?");
			//return <true,approved,cbgf>; // or false?
		}
		else
		{
			for (v <- versions)
			{
				<r, res, cbgf> = tryHypothesis(approved + version, v, master, servant, cbgf);
				if (r)
					approved += res;
			}
			//return <true,approved,cbgf>; // or false?
		}
	}
	return <true,approved,cbgf>;
}

tuple[bool,rel[str,str],CBGFSequence] tryHypothesis(rel[str,str] known, rel[str,str] version, BGFGrammar master, BGFGrammar servant, CBGFSequence c)
{
	rel[str,str] approved = known;
	CBGFSequence cbgf = c;
	// checking
	for (<n1,n2> <- version)
	{
		if (<n1,n3> <- approved)
			if (n1 == n2)
			{
				// trivial self-bindings
				version -= {<n1,n2>};
			}
			elseif (n3 != n2)
			{
				println("Hey, <n1> is already bound to <n3>, no need for <n2>!");
				return;
			}
	}
	if (isEmpty(version))
		return <true,approved,cbgf>;
	println("Approved: <approved>, version: <version>");
	// trying
	for (<n1,n2> <- version)
	{
		if (n1=="") continue;
		if (n1 != n2)
			<servant,cbgf> = makeStep(servant,cbgf,renameN_renameN(n2,n1));
		println(" * Checking <n1> as <n2>...");
		println("    * Production rules:");
		for (p <- prodsOfN(n1,master.prods))
			println("     * <p> <makeSig(p)>");
		println("    * vs");
		for (p <- prodsOfN(n1,servant.prods))
			println("     * <p> <makeSig(p)>");
		// TODO: many productions
		//ps1 = prodsOfN(n1,master.prods);
		//ps2 = prodsOfN(n1,servant.prods);
		//for (prodversion <- matchProds(prodsOfN(n1,master.prods),prodsOfN(n1,servant.prods)))
		//{
		//	println("Prodversion: <prodversion>");
		//	<r, res, cbgf> = tryHypothesis(approved, version, prodversion, master, servant, cbgf);
		//	if (r)
		//	{
		//		println("Prodversion approved.");
		//		approved += res;
		//	}
		//} 
		<prodmatch,namematch> = matchProds(prodsOfN(n1,master.prods),prodsOfN(n1,servant.prods));
		approved += namematch; // premature?
			for (<p1,p2> <- prodmatch)
			{
				list[rel[str,str]] versions = reduceSearchSpace(matchSigs(makeSig(p1),makeSig(p2)));
				if (isEmpty(versions))
				{
					println("Reached a fixed point?");
					//return <true,approved,cbgf>; // or false?
				}
				else
				{
					for (v <- versions)
					{
						<r, res, cbgf> = tryHypothesis(approved + version, v, master, servant, cbgf);
						if (r)
							approved += res;
					}
					//return <true,approved,cbgf>; // or false?
				}
			}
	}
	//res = checkMatch(masternt,prodsOfN(masternt,master.prods)[0],candidates[masternt],servant.prods);
	return <true,approved,cbgf>;
			
		//map[str,map[str,list[str]]] candidates = (master.roots[0] : (src : servant.roots | src <- sources));
		map[str,list[str]] candidates = (master.roots[0] : servant.roots);
		while(!isEmpty(tocheck))
		{
			<masternt,tocheck> = takeOneFrom(tocheck);
			println(" * Checking <masternt>...");
			println("    * Production rules:");
			for (p <- prodsOfN(masternt,master.prods))
				println("     * <p>");
			//println(servant);
			res = checkMatch(masternt,prodsOfN(masternt,master.prods)[0],candidates[masternt],servant.prods);
			if ([nt] := res)
			{
				println("    * vs");
				for (p <- prodsOfN(nt,servant.prods))
					println("     * <p>");
				println("   * Confirmed mapping of <masternt> to <nt>");
				namebind[masternt] = nt;
				if (nt != masternt)
					<servant,cbgf> = makeStep(servant,cbgf,renameN_renameN(nt,masternt));
				for (p <- prodsOfN(masternt,servant.prods))
					println("     * <p>");
				//candidates += 
				map[str,list[str]] newcands = allCandidates( prodsOfN(masternt,master.prods), prodsOfN(masternt,servant.prods) );
				println( newcands );
				for (k <- newcands)
				{
					//if (k notin candidates)
					//	candidates[k] = (); 
					candidates[k] = newcands[k];
				}
			}
			else //throw "     * No single result for <masternt>: <res>!";
				println("     * No result for <masternt> in <src>: <res>!");
			
			checked += masternt;
			nextnts = toList(analyse::Metrics::usedNs(prodsOfN(masternt,master.prods))) - checked;
			// nextnts -> candidates???
			tocheck += nextnts;
		}
}

tuple[BGFGrammar,BGFGrammar] loadSimpleGrammar(loc l)
{
	BGFGrammar g = readBGF(l), q;
	// we simplify our life by converting built-in types ("values") to regular nonterminals
	if (/val(string()) := g)
		q = transform([replace(val(string()),nonterminal("STRING"),globally())],g);
	else
		q = g;
	if (/val(integer()) := q)
		q = transform([replace(val(integer()),nonterminal("INTEGER"),globally())],q);
	return <g,q>;
}

public void main()
{
	//map[str,BGFGrammar] bgfs = ();
	//map[str,CBGFSequence] cbgfs = ();
	//map[str,map[str,str]] namebind = ();
	map[str,str] namebind = ();
	rel[str,str] naming, finbind;
	BGFGrammar original, servant, master;
	<_,master> = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	CBGFSequence cbgf, ncbgf;
	for (src <- sources)
	{
		println("Reading the <src> grammar...");
		<original,servant> = loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/<src>.bgf|);
		//println("Parsing the grammarbase with <size(bgfs)> grammars is done.");
		println("Normalising the grammar...");
		ncbgf = normal::ANF::normalise(servant);
		cbgf = ncbgf;
		//namebind = ( "" : "" ); // ???
		//println("Normalising <src> with");
		//iprintln(cbgf); 
		servant = transform(forward(cbgf),servant);
		println("Starting with the root <master.roots>, <servant.roots>.");
		println(servant);
		println([{<rm,rs>} | rm <- master.roots, rs <- servant.roots]);
		for (naming <- [{<rm,rs>} | rm <- master.roots, rs <- servant.roots])
		{
			//println("Is <rm> the same as <rs>?");
			<r,finbind,cbgf> = tryHypothesis({}, naming, master, servant, cbgf);
			if (r)
				println("Hypothesis resulted in <finbind> and <cbgf>");
		}
		// TODO: remove scraps of CBGF from above
		// Naming mismatches are all resolved now, let's construct a good CBGF
		CBGFSequence rcbgf = [];
		for (<n1,n2> <- finbind)
		{
			println(" * Binding <n2> to <n1>.");
			if (n1 == "" )
				rcbgf += [define_undefine([production("",n2,epsilon())]),inline_extract(production("",n2,epsilon()),globally())];
			elseif (n1 != "STRING" && n2 == "STRING")
				rcbgf += inline_extract(production("",n1,val(string())),globally());
			elseif (n1 == "STRING" && n2 == "STRING")
				;
			elseif (n1 == "STRING" && n2 != "STRING")
				rcbgf += define_undefine([production("",n2,val(string()))]);
			elseif (n1 != "STRING" && n2 != "STRING" && n1!=n2)
				rcbgf += renameN_renameN(n2,n1);
		}
		println("Writing CBGF...");
		//for (src <- sources)
		iprintln(rcbgf);
		cbgf = ncbgf+rcbgf;
		writeCBGF(cbgf,|home:///projects/slps/topics/convergence/guided/bgf/<src>.cbgf|);
		println("Writing BGF...");
		writeBGF(transform(forward(cbgf),original),|home:///projects/slps/topics/convergence/guided/bgf/<src>.res.bgf|);
	}
	println("Done.");
}
