@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::LaTeX

import IO;
import List;
import String;
import syntax::BGF;
//import io::ReadBGF;
import converge::Guided;

public str pp(BGFGrammar g)
{
	str s = "";
	for (p <- g.prods)
		s += "<pp(p)> & $<ppsig(p)>$ \\\\\n";
	return s;
}

str ppsig(BGFProduction p) = "<ppsig(converge::Guided::makeSig(p.rhs))>";

str ppsig(map[str,str] sig)
{
	s = "\\{";
	str n;
	for (k <- sig)
	{
		if (k=="STRING")
			n = "str";
		elseif (k=="INTEGER")
			n = "int";
		else
			n = "\\textit{<k>}";
		s += "\\langle <n>, <ppsig(sig[k])> \\rangle";
	}
	return "<s>\\}";
}

str ppsig(str z)
{
	str s = "";
	for (c <- chars(z))
		s += "{<stringChars([c])>}";
	return s;
}

str pp(BGFProduction p) = "prod(`<p.label>\', \\emph{<p.lhs>}, <pp(p.rhs)>)";
//{
	// 	prod(`', \emph{program}, plus(nt(\emph{function})))	& $\{\langle \textit{function}, {+}\rangle\}$\\
	
//}
str pp(epsilon()) = "\\varepsilon";
str pp(empty()) = "\\varphi";
str pp(anything()) = "\\alpha";
str pp(val(string())) = "str";
str pp(val(integer())) = "int";
str pp(terminal(s)) = "`<s>\'";
str pp(nonterminal("STRING")) = "str";
str pp(nonterminal("INTEGER")) = "int";
str pp(nonterminal(s)) = "\\emph{<s>}";
str pp(sequence(L))
{
	str s = pp(L[0]);
	for (e <- tail(L))
		s += ", <pp(e)>";
	return "seq([<s>])";
}
str pp(choice(L))
{
	str s = pp(L[0]);
	for (e <- tail(L))
		s += ", <pp(e)>";
	return "choice([<s>])";
}
str pp(optional(e)) = "{?}(<pp(e)>)";
str pp(star(e)) = "{*}(<pp(e)>)";
str pp(plus(e)) = "{+}(<pp(e)>)";
default str pp(BGFExpression e) = "?<e>?";
// not specified: selectable, marked, seplistplus, sepliststar 

public void main()
{
	//<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/xsd.normal.bgf|);
	println(pp(g));
}