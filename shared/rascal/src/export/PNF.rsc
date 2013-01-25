@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::PNF

import lib::Rascalware;
import syntax::BGF;

public str ppnf(BGFGrammar bgf) = "<ppnf(ps)>\n";

public str ppnf(BGFProdList ps) = mapjoin(ppnf,ps,"\n");

public str ppnf(set[BGFProduction] ps) = mapjoin(ppnf,ps,"\n");

public str ppnf(BGFProduction p) = "\\p(\\textit{<p.lhs>}, <ppnf(p.rhs)> ) ;";

// public str ppnf(BGFExprList es) = mapjoin(ppnf,es,", ");

public str ppnf(epsilon()) = "\\varepsilon";
public str ppnf(empty()) = "\\varphi";
public str ppnf(val(string())) = "\\mathit{str}";
public str ppnf(val(integer())) = "\\mathit{int}";
public str ppnf(anything()) = "\\alpha";
public str ppnf(terminal(str s)) = "\\te(\\textit{<s>})";
public str ppnf(nonterminal(str s)) = "\\n(\\textit{<s>})";
public str ppnf(selectable(s,e)) = "\\s(<s>, <ppnfost(e)>)";
public str ppnf(sequence(L)) = "<mapjoin(ppnf,L," \\cdot ")>";
public str ppnf(choice(L)) = "\\dis([<mapjoin(ppnf,L,"; ")>])";
public str ppnf(allof(L)) = "\\con([<mapjoin(ppnf,L,"; ")>])";
public str ppnf(marked(e)) = "\\langle<ppnf(e)>\\rangle";
public str ppnf(optional(e)) = "?(<ppnfpre(e)>)";
public str ppnf(not(e)) = "\\neg(<ppnfost(e)>)";
public str ppnf(plus(e)) = "+(<ppnfpre(e)>)";
public str ppnf(star(e)) = "*(<ppnfpre(e)>)";
public str ppnf(seplistplus(e1,e2)) = "\\seplistplus(<ppnf(e1)>,<ppnf(e2)>)";
public str ppnf(sepliststar(e1,e2)) = "\\sepliststar(<ppnf(e1)>,<ppnf(e2)>)";
public default str ppnf(BGFExpression e) = "UNKNOWN";

// pretty-print prefix metasymbols in parenthesis, others as usual
str ppnfpre(BGFExpression e:not(e1)) = "(<ppnf(e)>)";
str ppnfpre(BGFExpression e:selectable(s,e)) = "(<ppnf(e)>)";
default str ppnfpre(BGFExpression e) = ppnf(e);

// pretty-print postfix metasymbols in parenthesis, others as usual
str ppnfost(BGFExpression e:optional(e1)) = "(<ppnf(e)>)";
str ppnfost(BGFExpression e:plus(e1)) = "(<ppnf(e)>)";
str ppnfost(BGFExpression e:star(e1)) = "(<ppnf(e)>)";
default str ppnfost(BGFExpression e) = ppnf(e);
