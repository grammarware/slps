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
	["antlr","dcg","ecore","emf","jaxb","om","python","rascal-a","rascal-c","sdf","txl","xsd"];

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

map[str,list[str]] allCandidates(BGFExpression e1, BGFExpression e2)
{
	map[str,list[str]] cands = ();
	println(" --- Searching for candidates of <e1> with <e2>...");
	switch(e1)
	{
		case nonterminal(n1):
			if (nonterminal(n2) := e2)
				cands = addto(cands, n1, n2);
			elseif (val(string()) := e2)
				cands = addto(cands, n1, "STRING");
			elseif (val(integer()) := e2)
				cands = addto(cands, n1, "INTEGER");
			else fail;
		case val(string()):
			if (nonterminal(n2) := e2)
				cands = addto(cands, "STRING", n2);
			elseif (val(string()) := e2)
				cands = addto(cands, "STRING", "STRING");
			elseif (val(integer()) := e2)
				cands = addto(cands, "STRING", "INTEGER");
			else fail;
		case val(integer()):
			if (nonterminal(n2) := e2)
				cands = addto(cands, "INTEGER", n2);
			elseif (val(string()) := e2)
				cands = addto(cands, "INTEGER", "STRING");
			elseif (val(integer()) := e2)
				cands = addto(cands, "INTEGER", "INTEGER");
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

public void main()
{
	map[str,BGFGrammar] bgfs = ();
	map[str,CBGFSequence] cbgfs = ();
	map[str,map[str,str]] namebind = ();
	println("Reading the grammars...");
	bgfs["master"] = readBGF(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
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
	map[str,map[str,list[str]]] candidates = (bgfs["master"].roots[0] : (src : bgfs[src].roots | src <- sources));
	while(!isEmpty(tocheck))
	{
		<masternt,tocheck> = takeOneFrom(tocheck);
		println(" * Checking <masternt>...");
		for (src <- sources)
		{
			//println(bgfs[src]);
			res = checkMatch(masternt,prodsOfN(masternt,bgfs["master"].prods)[0],candidates[masternt][src],bgfs[src].prods);
			if ([nt] := res)
			{
				println("   * In <src>: maps to <nt>");
				namebind[src][masternt] = nt;
				if (nt != masternt)
					<bgfs[src],cbgfs[src]> = makeStep(bgfs[src],cbgfs[src],renameN_renameN(nt,masternt));
				for (p <- prodsOfN(masternt,bgfs[src].prods))
					println("      * <p>");
				//candidates += 
				map[str,list[str]] newcands = allCandidates( prodsOfN(masternt,bgfs["master"].prods), prodsOfN(masternt,bgfs[src].prods) );
				println( newcands );
				for (k <- newcands)
				{
					if (k notin candidates)
						candidates[k] = (); 
					candidates[k][src] = newcands[k];
				}
			}
			else //throw "     * No single result for <masternt>: <res>!";
				println("     * No result for <masternt> in <src>: <res>!");
		} 
		checked += masternt;
		nextnts = toList(analyse::Metrics::usedNs(prodsOfN(masternt,bgfs["master"].prods))) - checked;
		// nextnts -> candidates???
		tocheck += nextnts;
	}
	iprintln(namebind);
	println("Writing CBGFs...");
	for (src <- sources)
		writeCBGF(cbgfs[src],|home:///projects/slps/topics/convergence/guided/bgf/<src>.cbgf|);
	println("Writing BGFs...");
	for (src <- sources)
		writeBGF(bgfs[src],|home:///projects/slps/topics/convergence/guided/bgf/<src>.res.bgf|);
	println("Done.");
}