@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module extract::Python2BGF

import ValueIO;
import IO;
import List;
import String;
import ParseTree;
import syntax::BGF;
import io::WriteBGF;
import extract::PyParsing;
import normal::BGF;

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	writeBGF(normalise(pypafile2bgf(parse(#PyPaFile,|cwd:///|+args[0]))),|cwd:///|+args[1]);
	println("Extraction completed.");
}

BGFGrammar pypafile2bgf((PyPaFile)`<PyPaFragment+ pps>`)
{
	list[BGFProduction] ps = [];
	for ((PyPaFragment)`<PyPaLHS lhs><PyPaDef d><PyPaExpr2 rhs>;` <- pps)
		if ("<lhs>" == "defaultWhitespace")
			;
		elseif ("<rhs>" == "Forward()")
			;
		elseif ((PyPaLHS)`<PyPaExpr2 lhs1>` := lhs)
			ps += production("","<lhs1>",expr2expr(rhs));
		elseif ((PyPaLHS)`<{PyPaExpr2 ","}+ lhss>` := lhs && (PyPaExpr2)`(Keyword(<Id x>).suppress() for <x> in "<QChars t>".split())` := rhs)
			{
				nts = ["<z>" | z <- lhss];
				vals = [terminal(c) | c <- split(" ","<t>")];
				if (size(nts) != size(vals))
					throw "Impossible to match (un)packing.";
				for (i <- [0..size(nts)-1])
				ps += production("",nts[i],vals[i]);
			}
		else
			throw "Give up.";
	return grammar([],ps);
}

BGFExpression expr2expr((PyPaExpr2)`<Id n>`) = nonterminal("<n>");
BGFExpression expr2expr((PyPaExpr2)`<Id n>.copy()`) = nonterminal("<n>"); // pyparsing idiosyncrasy
BGFExpression expr2expr((PyPaExpr2)`"<QChars t>"`) = terminal("<t>");
//BGFExpression expr2expr((PyPaExpr2)`StringEnd()`) = terminal("\n"); // this works correctly, but the next line is more general
BGFExpression expr2expr((PyPaExpr2)`<Id f>()`) = nonterminal("<f>");
BGFExpression expr2expr((PyPaExpr2)`Suppress("<QChars t>")`) = terminal("<t>"); // not for AST, but we don't care
BGFExpression expr2expr((PyPaExpr2)`(<PyPaExpr2 e>)`) = expr2expr(e);
BGFExpression expr2expr((PyPaExpr2)`<PyPaExpr2 e>.setParseAction(<PyPaExpr2 _>)`) = expr2expr(e); // disregard parsing actions
BGFExpression expr2expr((PyPaExpr2)`<PyPaExpr2 e>.setWhitespaceChars(<PyPaExpr2 _>)`) = expr2expr(e); // disregard layout variations
BGFExpression expr2expr((PyPaExpr2)`<PyPaExpr2 e1>+<PyPaExpr2 e2>`) = sequence([expr2expr(e1),expr2expr(e2)]);
BGFExpression expr2expr((PyPaExpr2)`<PyPaExpr2 e1>^<PyPaExpr2 e2>`) = choice([expr2expr(e1),expr2expr(e2)]);
BGFExpression expr2expr((PyPaExpr2)`NotAny(<PyPaExpr2 _>)`) = epsilon(); // no way to represent negative production rules in BGF
BGFExpression expr2expr((PyPaExpr2)`Word(alphas)`) = val(string());
BGFExpression expr2expr((PyPaExpr2)`Word(nums)`) = val(integer());
BGFExpression expr2expr((PyPaExpr2)`Optional(<PyPaExpr2 e>)`) = optional(expr2expr(e));
BGFExpression expr2expr((PyPaExpr2)`oneOf("<QChars t>")`) = choice([terminal(c) | c <- split(" ","<t>")]);
BGFExpression expr2expr((PyPaExpr2)`ZeroOrMore(<PyPaExpr2 e>)`) = star(expr2expr(e));
BGFExpression expr2expr((PyPaExpr2)`Group(OneOrMore(<PyPaExpr2 e>))`) = plus(expr2expr(e));
BGFExpression expr2expr((PyPaExpr2)`OneOrMore(<PyPaExpr2 e>)`) = plus(expr2expr(e));

default BGFExpression expr2expr(PyPaExpr2 rhs)
{
	iprintln(rhs);
	println("What to do with <rhs>?");
}
