@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::WriteBGF

import IO;
import syntax::BGF;
import lang::xml::DOM;

public void writeBGF(BGFGrammar bgf, loc f)
{
	//grammar (list[str] roots, list[BGFProduction] prods)
	if (grammar(list[str] roots, list[BGFProduction] prods) := bgf)
	{
	list[Node] xml1 = [element(none(),"root",[charData(s)]) | s <- roots];
	list[Node] xml2 = [prod2xml(p) | p <- prods];
	//println(xml);
	writeFile(f,xmlRaw(document(element(namespace("bgf","http://planet-sl.org/bgf"),"grammar",xml1+xml2))));
	}
	else throw "ERROR: grammar expected in place of <bgf>";
	//Node N = parseXMLDOMTrim(readFile(f));
	//if (document(element(namespace(_,"http://planet-sl.org/xbgf"),"sequence",L)) := N)
	//	return [mapxbgf(step) | step <- L, element(namespace(_,"http://planet-sl.org/xbgf"),name,kids) := step];
	//else
	//	throw "<f> is not a proper XBGF file";
}

public Node prod2xml(BGFProduction p)
{
	if (production (str label, str lhs, BGFExpression rhs) := p)
	{
		list[Node] kids = [];
		if (label!="") kids += element(none(),"label",[charData(label)]);
		kids += element(none(),"nonterminal",[charData(lhs)]);
		kids += expr2xml(rhs);
		return element(namespace("bgf","http://planet-sl.org/bgf"),"production",kids);
	}
	else throw "ERROR: production rule expected in place of <p>";
}

public Node expr2xml(BGFExpression ex)
{
	Node e;
	switch(ex)
	{
		case epsilon(): e = element(none(),"epsilon",[]);
		case empty(): e = element(none(),"empty",[]);
		case val(string()): e = element(none(),"value",[charData("string")]);
		case val(integer()): e = element(none(),"value",[charData("int")]);
		case anything(): e = element(none(),"any",[]);
		case terminal(str s): e = element(none(),"terminal",[charData(s)]);
		case nonterminal(str s): e = element(none(),"nonterminal",[charData(s)]);
		case selectable(s,expr): e = element(none(),"selectable",[element(none(),"selector",[charData(s)]),expr2xml(expr)]);
		case sequence(L): e = element(none(),"sequence",[expr2xml(expr) | expr <- L]);
		case choice(L): e = element(none(),"choice",[expr2xml(expr) | expr <- L]);
		case marked(expr): e = element(none(),"marked",[expr2xml(expr)]);
		case optional(expr): e = element(none(),"optional",[expr2xml(expr)]);
		case plus(expr): e = element(none(),"plus",[expr2xml(expr)]);
		case star(expr): e = element(none(),"star",[expr2xml(expr)]);
		case starsepplus(e1,e2): e = element(none(),"starsepplus",[expr2xml(e1),expr2xml(e2)]);
		case starsepstar(e1,e2): e = element(none(),"starsepstar",[expr2xml(e1),expr2xml(e2)]);
		default: throw "ERROR: expression expected in place of <ex>";
	}
	return element(namespace("bgf","http://planet-sl.org/bgf"),"expression",[e]);
}
