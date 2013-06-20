@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RascalSyntax2BGF}
module extract::RascalSyntax2BGF

import lang::rascal::\syntax::RascalRascal;
import lang::rascal::grammar::definition::Modules;
import export::BNF;
import Grammar;
import ParseTree;
import language::BGF;
import io::WriteBGF;
import normal::BGF;
import String;

import IO;

BGFGrammar extractBGF(loc z)
{
	str gs = trim(readFile(z));
	str name = split("\n",split("module ",gs)[1])[0];
	BGFGrammar G = normalise(grammar2grammar(modules2grammar(name,{parse(#Module,gs)})));
	println("Extraction completed.");
	return G;
}

public void main(list[str] args) = writeBGF(extractBGF(|cwd:///|+args[0]),|cwd:///|+args[1]);

public void main() = main(|project://fl/src/Concrete.rsc|);

public void main(loc z) = println(pp(extractBGF(z)));

public void debug(loc z)
{
	str gs = trim(readFile(z));
	str name = split("\n",split("module ",gs)[1])[0];
	println("Extraction completed.");
	g = modules2grammar(name,{parse(#Module,gs)});
	iprintln(g);
	println(pp(normalise(grammar2grammar(g))));
}

BGFGrammar grammar2grammar(Grammar::\grammar(set[Symbol] starts, map[Symbol sort, Production def] rules))
 = language::BGF::grammar ([symbol2str(s) | s <- starts], [*rule2prods(rules[s]) | s <- rules, sort(_) := s]);

str symbol2str(lex(str s)) = "<s>"; 
str symbol2str(\start(sort(str s))) = "<s>";
str symbol2str(sort(str s)) = "<s>";
str symbol2str(layouts(str s)) = "";
str symbol2str(keywords(str s)) = "";
default str symbol2str(Symbol s) = "<s>";

list[BGFProduction] rule2prods(ParseTree::choice(lex(str s), _)) = [];
list[BGFProduction] rule2prods(ParseTree::choice(sort(str s), set[Production] ps))
	= [mappedProd | /Production p <- ps, BGFProduction mappedProd := prod2prod(p), production("","?",_) !:= mappedProd];
// ^^^^^^^^^^^ a little bit hacky to avoid the hassle of defining the real constraint, which is:
// "map all production rules, except for those inside of a choice"

default list[BGFProduction] rule2prods(Production def)
	= [production("", "UNKNOWN", language::BGF::terminal("<def>"))];

BGFProduction prod2prod(prod(label(str lab, sort(str nt)), list[Symbol] rhs, _))
	= production(lab, nt, rhs2expr(rhs));

BGFProduction prod2prod(prod(sort(str nt), list[Symbol] rhs, _))
	= production("", nt, rhs2expr(rhs));

default BGFProduction prod2prod(Production def) = production("", "?", language::BGF::epsilon());
//{
//	println("Cannot map:");
//	iprintln(def);
//	return production("", "?", language::BGF::epsilon());
//}

BGFExpression rhs2expr([Symbol s]) = symbol2expr(s);
BGFExpression rhs2expr(list[Symbol] ss) = language::BGF::sequence([symbol2expr(s) | s <- ss, layouts(_) !:= s]);

BGFExpression symbol2expr(label(str x, Symbol s)) = language::BGF::selectable(x,symbol2expr(s));
BGFExpression symbol2expr(\sort(str x)) = language::BGF::nonterminal(x);
BGFExpression symbol2expr(conditional(\sort(str x),{except(_)})) = language::BGF::nonterminal(x); // cannot represent better in BGF
BGFExpression symbol2expr(\lex(str x)) = language::BGF::nonterminal(x);
BGFExpression symbol2expr(ParseTree::\lit("\n")) = language::BGF::terminal("\\n"); //hack?
BGFExpression symbol2expr(ParseTree::\lit(str x)) = language::BGF::terminal(x);
BGFExpression symbol2expr(\opt(Symbol s)) = language::BGF::optional(symbol2expr(s));
BGFExpression symbol2expr(\iter-seps(Symbol item, list[Symbol] seps)) = iterplusseps2expr(item,[s | s <- seps, layouts(_) !:= s]);
BGFExpression symbol2expr(\iter-star-seps(Symbol item, list[Symbol] seps)) = iterstarseps2expr(item,[s | s <- seps, layouts(_) !:= s]);
BGFExpression symbol2expr(\seq(list[Symbol] ss)) = rhs2expr(ss);
default BGFExpression symbol2expr(Symbol s) = epsilon();

BGFExpression iterplusseps2expr(Symbol item, []) = language::BGF::plus(symbol2expr(item));
BGFExpression iterplusseps2expr(Symbol item, [Symbol sep]) = seplistplus(symbol2expr(item), symbol2expr(sep));
default BGFExpression iterplusseps2expr(Symbol item, list[Symbol] seps) = seplistplus(symbol2expr(item), language::BGF::sequence([symbol2expr(sep) | sep <- seps]));

BGFExpression iterstarseps2expr(Symbol item, []) = language::BGF::star(symbol2expr(item));
BGFExpression iterstarseps2expr(Symbol item, [Symbol sep]) = sepliststar(symbol2expr(item), symbol2expr(sep));
default BGFExpression iterstarseps2expr(Symbol item, list[Symbol] seps) = sepliststar(symbol2expr(item), language::BGF::sequence([symbol2expr(sep) | sep <- seps]));
