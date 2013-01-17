@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::BNF

import lib::Rascalware;
import syntax::BGF;

public str pp(grammar([],BGFProdList ps)) = "<pp(ps)>\n";
public default str pp(BGFGrammar bgf) = "Roots: <bgf.roots>\n<pp(bgf.prods)>\n";

public str pp(BGFProdList ps) = mapjoin(pp,ps,"\n");

public str pp(set[BGFProduction] ps) = mapjoin(pp,ps,"\n");

public str pp(production("",str lhs, BGFExpression rhs)) = "<lhs> ::= <pptop(rhs)> ;";
public default str pp(BGFProduction p) = "[<p.label>] <p.lhs> ::= <pptop(p.rhs)> ;";

str pptop(sequence(BGFExprList es)) = pp(es);
default str pptop(BGFExpression e) = pp(e);

public str pp(BGFExprList es) = mapjoin(pp,es," ");

public str pp(epsilon()) = "EPSILON";
public str pp(empty()) = "EMPTY";
public str pp(val(string())) = "STR";
public str pp(val(integer())) = "INT";
public str pp(anything()) = "ANYTHING";
public str pp(terminal(str s)) = "\"<s>\"";
public str pp(nonterminal(str s)) = "<s>";
public str pp(selectable(s,e)) = "<s>::<ppost(e)>";
public str pp(sequence(L)) = "(<pp(L)>)";
public str pp(choice(L)) = "(<mapjoin(pp,L," | ")>)";
public str pp(allof(L)) = "(<mapjoin(pp,L," & ")>)";
public str pp(marked(e)) = "\<<pp(e)>\>";
public str pp(optional(e)) = "<pppre(e)>?";
public str pp(not(e)) = "¬<ppost(e)>";
public str pp(plus(e)) = "<pppre(e)>+";
public str pp(star(e)) = "<pppre(e)>*";
public str pp(seplistplus(e1,e2)) = "{<pp(e1)> <pp(e2)>}+";
public str pp(sepliststar(e1,e2)) = "{<pp(e1)> <pp(e2)>}*";
public default str pp(BGFExpression e) = "UNKNOWN";

// pretty-print prefix metasymbols in parenthesis, others as usual
str pppre(BGFExpression e:not(e1)) = "(<pp(e)>)";
str pppre(BGFExpression e:selectable(s,e)) = "(<pp(e)>)";
default str pppre(BGFExpression e) = pp(e);

// pretty-print postfix metasymbols in parenthesis, others as usual
str ppost(BGFExpression e:optional(e1)) = "(<pp(e)>)";
str ppost(BGFExpression e:plus(e1)) = "(<pp(e)>)";
str ppost(BGFExpression e:star(e1)) = "(<pp(e)>)";
default str ppost(BGFExpression e) = pp(e);
