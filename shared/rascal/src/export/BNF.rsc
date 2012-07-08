@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::BNF

import lib::Rascalware;
import syntax::BGF;

public str pp(BGFGrammar bgf)
	= (isEmpty(bgf.roots) ? "" : "Roots: <bgf.roots>\n")
	+ pp(bgf.prods);

public str pp(BGFProdList ps) = joinStrings([pp(p) | p <- ps], "\n");
public str pp(set[BGFProduction] ps) = joinStrings([pp(p) | p <- ps], "\n");

public str pp(BGFProduction p)
	= (p.label!="" ? "[<p.label>] " : "")
	+ "<p.lhs> ::= <pp(p.rhs)> ;";

public str pp(BGFExprList es) = joinStrings([pp(e) | e <- es], " ");

public str ppc(BGFExprList es) = joinStrings([pp(e) | e <- es], " | ");

public str pp(epsilon()) = "EPSILON";
public str pp(empty()) = "EMPTY";
public str pp(val(string())) = "STR";
public str pp(val(integer())) = "INT";
public str pp(anything()) = "ANYTHING";
public str pp(terminal(str s)) = "\"<s>\"";
public str pp(nonterminal(str s)) = "<s>";
public str pp(selectable(s,e)) = "<s>::<pp(e)>";
public str pp(sequence(L)) = "(<pp(L)>)";
public str pp(choice(L)) = "(<ppc(L)>)";
public str pp(marked(e)) = "\<<pp(e)>\>";
public str pp(optional(e)) = "<pp(e)>?";
public str pp(plus(e)) = "<pp(e)>+";
public str pp(star(e)) = "<pp(e)>*";
public str pp(seplistplus(e1,e2)) = "{<pp(e1)> <pp(e2)>}+";
public str pp(sepliststar(e1,e2)) = "{<pp(e1)> <pp(e2)>}*";
public default str pp(BGFExpression e) = "UNKNOWN";
