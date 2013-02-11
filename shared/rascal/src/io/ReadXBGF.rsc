@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{ReadXBGF}
module io::ReadXBGF

import IO;
import language::BGF;
import language::XBGF;
import language::XScope;
import lang::xml::DOM;
import io::ReadBGF;

public XBGFSequence readXBGF(loc f)
{
	Node N = parseXMLDOMTrim(readFile(f));
	if (document(element(namespace(_,"http://planet-sl.org/xbgf"),"sequence",L)) := N)
		return [mapxbgf(step) | step <- L, element(namespace(_,"http://planet-sl.org/xbgf"),name,kids) := step];
	else
		throw "<f> is not a proper XBGF file";
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
				case element(_,"bypass",_): return bypass();
		 		case element(_,"chain",[prod]): return chain(mapprod(prod));
		 		// clone(str x, str y, XBGFScope w)
		 		// concatT(list[str] xs, str y, XBGFScope w)
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
				case element(_,"import",ps): return importG([mapprod(p) | p <- ps]);
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
				case element(_,"rename",[element(none(),"label",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameL(s1, s2);
				case element(_,"rename",[element(none(),"nonterminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameN(s1, s2);
				case element(_,"rename",[element(none(),"selector",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameS(s1, s2, globally());
				case element(_,"rename",[element(none(),"selector",[element(none(),"in",[charData(str w)]),element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameS(s1, s2, inlabel(w));
				case element(_,"rename",[element(none(),"terminal",[element(none(),"from",[charData(str s1)]),element(none(),"to",[charData(str s2)])])]): return renameT(s1, s2);
		 		case element(_,"replace",[e1,e2]): return replace(mapexpr(e1),mapexpr(e2),globally());
		 		case element(_,"replace",[e1,e2,w]): return replace(mapexpr(e1),mapexpr(e2),mapcontext(w));
				case element(_,"reroot",roots): return reroot([r | element(none(),"root",[charData(r)]) <- roots]);
				// TODO: what is the best wat to parametrise splitN?
				// also, the current structure is too hard to match in a one-liner in Rascal 
				//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*,element(none(),"label",[charData(str s)])]): return splitN(s,[mapprod(p) | p <- ps],mapcontext(element(none(),"label",[charData(s)])));
				//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*]): return splitN(s,[mapprod(p) | p <- ps],globally());
				// splitN(str x, list[BGFProduction] ps, XBGFScope w)
				case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),p]): return splitN(s,[mapprod(p)],nowhere());
				case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),p,w]): return splitN(s,[mapprod(p)],mapcontext(w));
				// splitT(str x, list[str] ys, XBGFScope w)
				case element(_,"splitT",[element(none(),"terminal",[charData(str s)]),element(none(),"into",ts)]): return splitT(s,[mapt(t) | t <- ts],globally());
				case element(_,"splitT",[element(none(),"terminal",[charData(str s)]),element(none(),"into",ts),w]): return splitT(s,[mapt(t) | t <- ts],mapcontext(w));
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
 				case element(_,"atomic",L): return atomic([mapxbgf(x) | x <- L, element(namespace(_,"http://planet-sl.org/xbgf"),_,_) := x]);
 				case element(_,"strip",[element(none(),str s,[])]): return strip(s);
		 		// default
		 		default:
		 			if(element(_,elname,elkids):=el)
		 				throw "ERROR mapping <elname> with kids:\n<elkids>";
	 				else
		 				throw "Unknown ERROR in mapxbgf";
	 		}
 		}
	else
		throw "ERROR with:\n<el>";
}

XBGFScope mapcontext(Node n)
{
	switch(n)
	{
		case element(none(),"label",[charData(str s)]): return inlabel(s);
		case element(none(),"nonterminal",[charData(str s)]): return innt(s);
		case element(none(),"in",[charData(str s)]): return inlabel(s); // conceptually wrong yet happens in renameS
		case element(none(),"in",[w]): return mapcontext(w);
		default: throw "ERROR in context: <n>";
	}
}

str mapt(element(_,"terminal",[charData(str s)])) = s;
