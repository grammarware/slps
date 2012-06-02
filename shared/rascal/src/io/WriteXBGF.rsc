@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::WriteXBGF

import IO;
import syntax::BGF;
import syntax::XBGF;
import lang::xml::DOM;
import io::WriteBGF;

public void writeXBGF(XBGFSequence xbgf, loc f)
{
	list[Node] xml = [xbgf2xml(x) | x <- xbgf];
	//println(xml);
	writeFile(f,xmlRaw(document(element(namespace("xbgf","http://planet-sl.org/xbgf"),"sequence",xml))));
	//Node N = parseXMLDOMTrim(readFile(f));
	//if (document(element(namespace(_,"http://planet-sl.org/xbgf"),"sequence",L)) := N)
	//	return [mapxbgf(step) | step <- L, element(namespace(_,"http://planet-sl.org/xbgf"),name,kids) := step];
	//else
	//	throw "<f> is not a proper XBGF file";
}

Node xbgf2xml(XBGFCommand step)
{
	switch(step)
	{
		case abridge(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"abridge",[prod2xml(prod)]);
		case abstractize(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"abstractize",[prod2xml(prod)]);
		case addH(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"add",[element(none(),"horizontal",[prod2xml(prod)])]);
		case addV(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"add",[element(none(),"vertical",[prod2xml(prod)])]);
		case anonymize(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"anonymize",[prod2xml(prod)]);
		case appear(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"appear",[prod2xml(prod)]);
		case chain(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"chain",[prod2xml(prod)]);
		// clone(str x, str y, XBGFContext w)
		// concatT(list[str] xs, str y, XBGFContext w)
		case concretize(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"concretize",[prod2xml(prod)]);
		case deanonymize(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"deanonymize",[prod2xml(prod)]);
		case define(list[BGFProduction] ps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"define",[prod2xml(p) | p <- ps]);
		
		case designate(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"designate",[prod2xml(prod)]);
		case detour(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"detour",[prod2xml(prod)]);
		case deyaccify(str s): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"deyaccify",[element(none(),"nonterminal",[charData(s)])]);
		case disappear(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"disappear",[prod2xml(prod)]);
		case distribute(w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"distribute",[context2xml(w)]);
		case downgrade(p1,p2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"downgrade",[prod2xml(p1),prod2xml(p2)]);
		case eliminate(str s): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"eliminate",[element(none(),"nonterminal",[charData(s)])]);
		case equate(str s1,str s2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"equate",[element(none(),"align",[charData(s1)]),element(none(),"with",[charData(s2)])]);
		case extract(prod,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"extract",[prod2xml(prod)]);
		case extract(prod,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"extract",[prod2xml(prod),element(none(),"in",[context2xml(w)])]);
		case factor(e1,e2,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"factor",[expr2xml(e1),expr2xml(e2)]);
		case factor(e1,e2,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"factor",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case fold(str s,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"fold",[element(none(),"nonterminal",[charData(s)])]);
		case fold(str s, w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"fold",[element(none(),"nonterminal",[charData(s)]),element(none(),"in",[context2xml(w)])]);
		case horizontal(w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"horizontal",[context2xml(w)]);
		case importG(ps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"import",[prod2xml(p) | p <- ps]);
		case inject(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"inject",[prod2xml(prod)]);
		case inline(str s): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"inline",[charData(s)]);
		case introduce(ps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"introduce",[prod2xml(p) | p <- ps]);
		case iterate(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"iterate",[prod2xml(prod)]);
		case lassoc(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"lassoc",[prod2xml(prod)]);
		case massage(e1,e2,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"massage",[expr2xml(e1),expr2xml(e2)]);
		case massage(e1,e2,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"massage",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case narrow(e1,e2,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"narrow",[expr2xml(e1),expr2xml(e2)]);
		case narrow(e1,e2,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"narrow",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case permute(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"permute",[prod2xml(prod)]);
		case project(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"project",[prod2xml(prod)]);
		case rassoc(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rassoc",[prod2xml(prod)]);
		case redefine(ps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"redefine",[prod2xml(p) | p <- ps]);
		case removeH(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"remove",[element(none(),"horizontal",[prod2xml(prod)])]);
		case removeV(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"remove",[element(none(),"vertical",[prod2xml(prod)])]);
		case renameL(s1, s2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rename",[element(none(),"label",[element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		case renameN(s1, s2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rename",[element(none(),"nonterminal",[element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		case renameS(s1, s2, globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rename",[element(none(),"selector",[element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		case renameS(s1, s2, inlabel(str s)): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rename",[element(none(),"selector",[element(none(),"in",[charData(s)]),element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		case renameT(s1, s2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"rename",[element(none(),"terminal",[element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		case replace(e1,e2,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"replace",[expr2xml(e1),expr2xml(e2)]);
		case replace(e1,e2,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"replace",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case reroot(roots): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"reroot",[element(none(),"root",[charData(r)]) | r <- roots]);
		// TODO: what is the best wat to parametrise splitN?
		// also, the current structure is too hard to match in a one-liner in Rascal 
		//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*,element(none(),"label",[charData(str s)])]): return splitN(s,[mapprod2xml(prod)(p) | p <- ps],mapcontext(element(none(),"label",[charData(s)])));
		//case element(_,"split",[element(none(),"nonterminal",[charData(str s)]),ps*]): return splitN(s,[mapprod2xml(prod)(p) | p <- ps],globally());
		// splitN(str x, list[BGFProduction] ps, XBGFContext w)
		case splitN(s,[p],globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"split",[element(none(),"nonterminal",[charData(s)]),prod2xml(p)]);
		case splitN(s,[p],w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"split",[element(none(),"nonterminal",[charData(s)]),prod2xml(p),context2xml(w)]);
		// TODO: not implemented anywhere
		// splitT(str x, list[str] ys, XBGFContext w)
		case unchain(prod): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"unchain",[prod2xml(prod)]);
		case undefine(xs): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"undefine",[element(none(),"nonterminal",[charData(s)]) | s <- xs]);
		case unfold(s,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"unfold",[element(none(),"nonterminal",[charData(s)])]);
		case unfold(s,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"unfold",[element(none(),"nonterminal",[charData(s)]),element(none(),"in",[context2xml(w)])]);
		case unite(s1, s2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"unite",[element(none(),"add",[charData(s1)]),element(none(),"to",[charData(s2)])]);
		case unlabel(s): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"unlabel",[element(none(),"label",[charData(s)])]);
		case upgrade(p1,p2): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"upgrade",[prod2xml(p1),prod2xml(p2)]);
		case vertical(w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"vertical",[context2xml(w)]);
		case widen(e1,e2,globally()): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"widen",[expr2xml(e1),expr2xml(e2)]);
		case widen(e1,e2,w): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"widen",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case yaccify(ps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"yaccify",[prod2xml(p) | p <- ps]);
		// legacy
		case atomic(list[XBGFCommand] steps): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"atomic",[xbgf2xml(x) | x <- steps]);
		case strip(s): return element(namespace("xbgf","http://planet-sl.org/xbgf"),"strip",[element(none(),s,[])]);
		// default
		default:
			throw "ERROR: <step>";
	}
}

Node context2xml(XBGFContext w)
{
	switch(w)
	{
		case inlabel(str s): return element(none(),"label",[charData(s)]);
		case innt(str s): return element(none(),"nonterminal",[charData(s)]);
		case globally(): return comment("globally");
		default: throw "ERROR in context: <w>";
	}
}
