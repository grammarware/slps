@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::NegotiatedXBGF

import lib::Rascalware;
import IO;
import syntax::BGF;
import syntax::XBGF;
import normal::BGF;
import transform::library::Util;
import transform::XBGF;
import transform::Results;
import String;
import util::Math;

public BGFGrammar transformAnyway(XBGFSequence xbgf, BGFGrammar g)
{
	map[str,str] negacy = ();
	for (XBGFCommand step <- xbgf)
		<negacy,g> = keepTrying(step,g,negacy);
	return g;
}

tuple[map[str,str],BGFGrammar] keepTrying(XBGFCommand step, BGFGrammar g, map[str,str] negacy)
{
	//if (!isEmpty(negacy)) println("Negacy: <negacy>");
	<g,out,adv> = attemptTransform(adjustNames(step,negacy),g);
	if (ok() := out)
		return <negacy,normalise(g)>;
	else
	{
		report(out);
		if (len(adv)==0)
			thw(out);
		next = getOneFrom(adv);
		return keepTrying(next,g,negacy+impact(step,next));
	}
}

XBGFCommand adjustNames(XBGFCommand step, map[str,str] negacy)
{
	// TODO left hand sides
	for (str k <- negacy)
		step = visit(step){case nonterminal(k) => nonterminal(negacy[k])};
	//println("Fixed fine: <step>");
	return step;
}

map[str,str] impact(renameN(x,y),renameN(x,z)) = (y:z);
map[str,str] impact(renameN(x,z),renameN(y,z)) = (x:y);
default map[str,str] impact(XBGFCommand step1, XBGFCommand step2) = (); // impact unknown

tuple[BGFGrammar,XBGFOutcome,set[XBGFCommand]] attemptTransform(XBGFCommand xbgf, BGFGrammar g)
{
	XBGFResult res = transform::XBGF::vtransform(xbgf, g);
	//iprintln(res.r);
	return <res.g,res.r,negotiate(res.g,xbgf,res.r)>;	
}

set[XBGFCommand] negotiate(BGFGrammar g, XBGFCommand _, ok()) = {};
set[XBGFCommand] negotiate(BGFGrammar g, renameN(str x, str y), problemStr("Nonterminal must not be fresh", x))
	= {renameN(n,y) | str n <- adviseUsedNonterminal(x,allNs(g.prods))};
set[XBGFCommand] negotiate(BGFGrammar g, renameN(str x, str y), problemStr("Nonterminal must be fresh", y))
	= {renameN(x,n) | str n <- adviseFreshNonterminal(y,allNs(g.prods))};
default set[XBGFCommand] negotiate(BGFGrammar _, XBGFCommand _, XBGFOutcome _) = {};

//tuple[Problem,Advice,BGFGrammar] runAbridge(BGFProduction prod, grammar(rs, ps))
//{
//	if (production(_,x,nonterminal(x)) !:= prod)
//		return
//		<
//			error("Production <prod> cannot be abridged."),
//			adviseDetouredProd(prod,ps), // NOT IMPLEMENTED YET
//			grammar(rs,ps)
//		>;
//	if (!inProds(prod,ps))
//		return
//		<
//			error("Production <prod> not found."),
//			setadvice("Consider the nonterminal <prod.lhs> already abridged.",[]),
//			grammar(rs,ps)
//		>;
//	return <noproblem(),noadvice(),grammar(rs, ps - prod)>;
//}

set[str] adviseUsedNonterminal(str x, set[str] nts)
	= {z | z <- nts, levenshtein(z,x) == min([levenshtein(s,x) | s <- nts])};

set[str] adviseFreshNonterminal(str x, set[str] nts)
{
	list[str] low = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y"];
	list[str] upp = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y"];
	set[str] adv = {};
	int cx = 1;
	str s = x;
	// expr -> expr1
	while ("<x><cx>" in nts) cx+=1;
	adv += "<x><cx>";
	// expr -> expr_
	while (s in nts) s += "_"; 
	adv += s;
	// expr -> shjk
	s = "";
	for (c <- [stringChar(charAt(x,i)) | i <- [0..len(x)-1]])
		if (c in low)
			s += low[arbInt(len(low))];
		elseif (c in upp)
			s += upp[arbInt(len(upp))];
		else
			s += stringChar(c);
	adv += s;
	return adv;
}