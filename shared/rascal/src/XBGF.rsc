@contributor{Vadim Zaytsev - vadim@grammarware.net - CWI}
module XBGF

import IO;
import XBGFSyntax;
import lang::xml::DOM;

public void main()
{
	println(readXBGF(|project://xbgf/tests/undefine1.xbgf|));
}

public bool tryAll()
{
	loc base = |project://xbgf/tests|;
	for (f <- listEntries(base))
	{
		if (f == ".gitignore") continue;
		println(f);
		println(readXBGF(base+f));
	}
	return true;
}


public XBGFSequence readXBGF(loc f)
{
	Node N = parseXMLDOMTrim(readFile(f));
	XBGFSequence xbgfs = [];
	//XBGFCommand x;
	if (document(element(namespace(_,"http://planet-sl.org/xbgf"),"sequence",L)) := N)
		return [mapxbgf(element(namespace("xbgf","http://planet-sl.org/xbgf"),name,kids)) | element(namespace(_,"http://planet-sl.org/xbgf"),name,kids) <- L];
}

XBGFCommand mapxbgf(Node el)
{
	if (element(namespace(_,"http://planet-sl.org/xbgf"),_,_) := el)
 		{
	 		switch(el)
	 		{
		 		case element(_,"abridge",[prod]): return abridge(mapprod(prod));
		 		case element(_,"abstractize",[prod]): return abstractize(mapprod(prod));
		 		case element(_,"add",[element(none(),"horizontal",[prod])]): return addH(mapprod(prod));
		 		case element(_,"add",[element(none(),"vertical",[prod])]): return addV(mapprod(prod));
		 		case element(_,"anonymize",[prod]): return anonymize(mapprod(prod));
		 		case element(_,"appear",[prod]): return appear(mapprod(prod));
		 		case element(_,"chain",[prod]): return chain(mapprod(prod));
		 		// clone(str x, str y, BGFContext w)
		 		// concatT(list[str] xs, str y, BGFContext w)
		 		case element(_,"concretize",[prod]): return concretize(mapprod(prod));
		 		case element(_,"deanonymize",[prod]): return deanonymize(mapprod(prod));
		 		case element(_,"define",ps): return define([mapprod(p) | p <- ps]);
		 		case element(_,"designate",[prod]): return designate(mapprod(prod));
				case element(_,"detour",[prod]): return detour(mapprod(prod));
				case element(_,"deyaccify",[element(none(),"nonterminal",[charData(str s)])]): return deyaccify(s);
				case element(_,"disappear",[prod]): return disappear(mapprod(prod));
				case element(_,"distribute",[w]): return distribute(mapcontext(w));
				case element(_,"downgrade",[p1,p2]): return downgrade(mapprod(p1),mapprod(p2));
				case element(_,"eliminate",[element(none(),"nonterminal",[charData(str s)])]): return eliminate(s);
				case element(_,"equate",[element(none(),"align",[charData(str s1)]),element(none(),"with",[charData(str s2)])]): return equate(s1,s2);
				case element(_,"extract",[prod]): return extract(mapprod(prod),globally());
				case element(_,"extract",[prod,w]): return extract(mapprod(prod),mapcontext(w));
				case element(_,"factor",[e1,e2]): return factor(mapexpr(e1),mapexpr(e2),globally());
				case element(_,"factor",[e1,e2,w]): return factor(mapexpr(e1),mapexpr(e2),mapcontext(w));
				case element(_,"fold",[element(none(),"nonterminal",[charData(str s)])]): return fold(s,globally());
				case element(_,"fold",[element(none(),"nonterminal",[charData(str s)]),w]): return fold(s,mapcontext(w));
				case element(_,"horizontal",[w]): return horizontal(mapcontext(w));
				case element(_,"import",ps): return \import([mapprod(p) | p <- ps]);
				case element(_,"inject",[prod]): return inject(mapprod(prod));
				case element(_,"inline",[charData(str s)]): return inline(s);
				case element(_,"introduce",ps): return introduce([mapprod(p) | p <- ps]);
				case element(_,"iterate",[prod]): return iterate(mapprod(prod));
				case element(_,"lassoc",[prod]): return lassoc(mapprod(prod));
				case element(_,"massage",[e1,e2]): return massage(mapexpr(e1),mapexpr(e2),globally());
				case element(_,"massage",[e1,e2,w]): return massage(mapexpr(e1),mapexpr(e2),mapcontext(w));
		 		case element(_,"narrow",[e1,e2]): return narrow(mapexpr(e1),mapexpr(e2),globally());
		 		case element(_,"narrow",[e1,e2,w]): return narrow(mapexpr(e1),mapexpr(e2),mapcontext(w));
				case element(_,"permute",[prod]): return permute(mapprod(prod));
				case element(_,"project",[prod]): return project(mapprod(prod));
				case element(_,"rassoc",[prod]): return rassoc(mapprod(prod));
				case element(_,"redefine",ps): return redefine([mapprod(p) | p <- ps]);
		 		case element(_,"remove",[element(none(),"horizontal",[prod])]): return removeH(mapprod(prod));
		 		case element(_,"remove",[element(none(),"vertical",[prod])]): return removeV(mapprod(prod));
				case element(_,"rename",[element(none(),"label",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameL(s1, s2, globally());
				case element(_,"rename",[element(none(),"label",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)]),w])]): return renameL(s1, s2, mapcontext(w));
				case element(_,"rename",[element(none(),"nonterminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameN(s1, s2, globally());
				case element(_,"rename",[element(none(),"nonterminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)]),w])]): return renameN(s1, s2, mapcontext(w));
				case element(_,"rename",[element(none(),"selector",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameS(s1, s2, globally());
				case element(_,"rename",[element(none(),"selector",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)]),w])]): return renameS(s1, s2, mapcontext(w));
				case element(_,"rename",[element(none(),"terminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameT(s1, s2, globally());
				case element(_,"rename",[element(none(),"terminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)]),w])]): return renameT(s1, s2, mapcontext(w));
		 		case element(_,"replace",[e1,e2]): return replace(mapexpr(e1),mapexpr(e2),globally());
		 		case element(_,"replace",[e1,e2,w]): return replace(mapexpr(e1),mapexpr(e2),mapcontext(w));
				case element(_,"reroot",roots): return reroot([r | element(none(),"root",[charData(r)]) <- roots]);
				// TODO: what is the best wat to parametrise splitN?
				// also, the current structure is too hard to match in a one-liner in Rascal 
				//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*,element(none(),"label",[charData(str s)])]): return splitN(s,[mapprod(p) | p <- ps],mapcontext(element(none(),"label",[charData(s)])));
				//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*]): return splitN(s,[mapprod(p) | p <- ps],globally());
				// splitN(str x, list[BGFProduction] ps, BGFContext w)
				case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),p]): return splitN(s,[mapprod(p)],globally());
				case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),p,w]): return splitN(s,[mapprod(p)],mapcontext(w));
				// TODO: not implemented anywhere
				// splitT(str x, list[str] ys, BGFContext w)
				case element(_,"unchain",[prod]): return unchain(mapprod(prod));
				case element(_,"undefine",xs): return undefine([s | element(none(),"nonterminal",[charData(s)]) <- xs]);
				case element(_,"unfold",[element(none(),"nonterminal",[charData(str s)])]): return unfold(s,globally());
				case element(_,"unfold",[element(none(),"nonterminal",[charData(str s)]),w]): return unfold(s,mapcontext(w));
				case element(_,"unite",[element(none(),"add",[charData(str s1)]),element(none(),"to",[charData(str s2)])]): return unite(s1, s2);
				case element(_,"unlabel",[element(none(),"label",[charData(str s)])]): return unlabel(s);
		 		case element(_,"upgrade",[p1,p2]): return upgrade(mapprod(p1),mapprod(p2));
				case element(_,"vertical",[w]): return vertical(mapcontext(w));
				case element(_,"widen",[e1,e2]): return widen(mapexpr(e1),mapexpr(e2),globally());
				case element(_,"widen",[e1,e2,w]): return widen(mapexpr(e1),mapexpr(e2),mapcontext(w));
		 		case element(_,"yaccify",ps): return yaccify([mapprod(p) | p <- ps]);
		 		// legacy
 				case element(_,"atomic",L): return atomic([mapxbgf(element(namespace("xbgf","http://planet-sl.org/xbgf"),name,kids)) | element(namespace(_,"http://planet-sl.org/xbgf"),name,kids) <- L]);
 				case element(_,"strip",[element(none(),str s,[])]): return strip(s);
		 		// default
		 		case element(_,elname,elkids):
		 			{
		 				println("ERROR mapping "+elname);
		 				println(elkids);
		 				return;
		 			}
	 		}
 		}
	else
 		{
 			println("ERROR with:");
 			println(el);
 		}
}

BGFContext mapcontext(Node n)
{return globally();}

BGFProduction mapprod(Node n)
{
	str label = "";
	str lhs = "";
	BGFExpression rhs;
	if (element(namespace(_,"http://planet-sl.org/bgf"),"production",kids) := n)
	{
		for (k <- kids)
			switch (k)
			{
				case element(none(),"label",[charData(str s)]) : label = s;
				case element(none(),"nonterminal",[charData(str s)]) : lhs = s;
				case element(namespace(_,"http://planet-sl.org/bgf"),"expression",[expr]): rhs = mapexpr(expr);
			}
		return production (label, lhs, rhs);
	}
	else
		{println("ERROR in mapprod");println(n);return;}
}

BGFExpression mapexpr(Node n)
{
	switch(n)
	{
		case element(namespace(_,"http://planet-sl.org/bgf"),"expression",[e]): return mapexpr(e);
		case element(none(),"epsilon",[]): return epsilon();
		case element(none(),"empty",[]): return empty();
		case element(none(),"value",[charData("string")]): return val(string());
		case element(none(),"value",[charData("int")]): return val(integer());
		case element(none(),"any",[]): return anything();
		case element(none(),"terminal",[charData(str s)]): return terminal(s);
		case element(none(),"nonterminal",[charData(str s)]): return nonterminal(s);
		case element(none(),"selectable",[element(none(),"selector",[charData(str s)]),expr]): return selectable(s,mapexpr(expr));
		case element(none(),"sequence",kids): return sequence([mapexpr(k) | k <- kids]);
		case element(none(),"choice",kids): return choice([mapexpr(k) | k <- kids]);
		case element(none(),"marked",[expr]): return marked(mapexpr(expr));
		case element(none(),"optional",[expr]): return optional(mapexpr(expr));
		case element(none(),"plus",[expr]): return plus(mapexpr(expr));
		case element(none(),"star",[expr]): return star(mapexpr(expr));
		case element(none(),"starsepplus",[e1,e2]): return starsepplus(mapexpr(e1),mapexpr(e2));
		case element(none(),"starsepstar",[e1,e2]): return starsepstar(mapexpr(e1),mapexpr(e2));
		default: {println("ERROR in mapexpr");println(n);return;}
	}
}
