module XBGF

import IO;
import XBGFSyntax;
import lang::xml::DOM;

public void main()
{
	Node N = parseXMLDOMTrim(readFile(|project://xbgf/tests/long.xbgf|));
	XBGFSequence xbgfs = [];
	//XBGFCommand x;
	if (document(element(namespace(_,"http://planet-sl.org/xbgf"),"sequence",L)) := N)
	 	for (el <- L)
	 		if (element(namespace(_,"http://planet-sl.org/xbgf"),_,_) := el)
	 		{
		 		switch(el)
		 		{
			 		case element(_,"abridge",[prod]): xbgfs += abridge(mapprod(prod));
			 		case element(_,"add",[element(none(),"vertical",[prod])]): xbgfs += addV(mapprod(prod));
			 		case element(_,"narrow",[e1,e2]): xbgfs += narrow(mapexpr(e1),mapexpr(e2),globally());
			 		case element(_,"narrow",[e1,e2,w]): xbgfs += narrow(mapexpr(e1),mapexpr(e2),mapcontext(w));
			 		case element(_,"replace",[e1,e2]): xbgfs += replace(mapexpr(e1),mapexpr(e2),globally());
			 		case element(_,"replace",[e1,e2,w]): xbgfs += replace(mapexpr(e1),mapexpr(e2),mapcontext(w));
			 		case element(_,"yaccify",[p1,p2]): xbgfs += yaccify(mapprod(p1),mapprod(p2));
			 		// default
			 		case element(_,elname,elkids):
			 			{
			 				println(elname);
			 				println(elkids);
			 			}
		 		}
	 		}
	println(xbgfs);
}

BGFContext mapcontext(Node n)
{return globally();}

BGFProduction mapprod(Node n)
{
	str label = "";
	str lhs = "";
	BGFExpression rhs;
	if (element(namespace("bgf","http://planet-sl.org/bgf"),"production",kids) := n)
	{
		for (k <- kids)
			switch (k)
			{
				case element(none(),"label",[charData(str s)]) : label = s;
				case element(none(),"nonterminal",[charData(str s)]) : lhs = s;
				case element(namespace("bgf","http://planet-sl.org/bgf"),"expression",[expr]): rhs = mapexpr(expr);
			}
		return production (label, lhs, rhs);
	}
	else
		return; 
}

BGFExpression mapexpr(Node n)
{
	switch(n)
	{
		case element(namespace("bgf","http://planet-sl.org/bgf"),"expression",[e]): return mapexpr(e);
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
		default: {println("ERROR in mapexpr");return epsilon();}
	}
}

//	
//<xbgf:sequence
//  xmlns:bgf="http://planet-sl.org/bgf"
//  xmlns:xbgf="http://planet-sl.org/xbgf">
//  <xbgf:abridge>
//    <bgf:production>
//      <label>bracket</label>
//      <nonterminal>expr</nonterminal>
//      <bgf:expression>
//        <nonterminal>expr</nonterminal>
//      </bgf:expression>
//    </bgf:production>
//  </xbgf:abridge>
//</xbgf:sequence>
