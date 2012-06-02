@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::ReadBGF

import IO;
import syntax::BGF;
import lang::xml::DOM;

public BGFGrammar readBGF(loc f)
{
	if (document(element(namespace(_,"http://planet-sl.org/bgf"),"grammar",L)) := parseXMLDOMTrim(readFile(f)))
		return grammar([s | element(none(),"root",[charData(s)]) <- L],[mapprod(p) | p <- L, element(namespace(_,"http://planet-sl.org/bgf"),name,kids) := p]);
	else
		throw "<f> is not a proper BGF file";
}

public BGFProduction mapprod(Node n)
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
		throw "ERROR in mapprod:\n<n>";
}

public BGFExpression mapexpr(Node n)
{
	switch(n)
	{
		// just in case
		case element(namespace(_,"http://planet-sl.org/bgf"),"expression",[e]): return mapexpr(e);
		// regular alternatives
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
		case element(none(),"seplistplus",[e1,e2]): return seplistplus(mapexpr(e1),mapexpr(e2));
		case element(none(),"sepliststar",[e1,e2]): return sepliststar(mapexpr(e1),mapexpr(e2));
		default: throw "ERROR in mapexpr:\n<n>";
	}
}
