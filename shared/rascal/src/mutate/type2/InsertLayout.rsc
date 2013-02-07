@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{InsertLayout}
module mutate::type2::InsertLayout

import syntax::BGF;
import normal::BGF;

// TODO: this mutation does exactly what it says on the tin, but quite a bit more than it says in its Type II spec
BGFGrammar InsertLayout(BGFGrammar g) = grammar (g.roots, normalise([production(p.label, p.lhs, insertWS2expr(p.rhs)) | p<-g.prods]));

BGFExpression insertWS2expr(sequence(BGFExprList es)) = sequence(([es[0]] | it + [optional(nonterminal(WS)),insertWS2expr(e)] | e <- tail(es)));
BGFExpression insertWS2expr(choice(BGFExprList es)) = choice(mapper(es,insertWS2expr));
BGFExpression insertWS2expr(plus(BGFExpression e)) = seplistplus(e,optional(nonterminal(WS)));
BGFExpression insertWS2expr(star(BGFExpression e)) = sepliststar(e,optional(nonterminal(WS)));
BGFExpression insertWS2expr(seplistplus(e,s)) = seplistplus(e,sequence([optional(nonterminal(WS)),s,optional(nonterminal(WS))]));
BGFExpression insertWS2expr(sepliststar(e,s)) = sepliststar(e,sequence([optional(nonterminal(WS)),s,optional(nonterminal(WS))]));
default BGFExpression insertWS2expr(BGFExpression e) = e;
