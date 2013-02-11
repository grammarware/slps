@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::LaTeX

import IO;
import List;
import String;
import language::BGF;
import language::XBGF;
import language::CBGF;
import language::XScope;
import language::Prodsig;
//import io::ReadBGF;
import converge::Guided;
import lib::Rascalware;
import analyse::Prodsigs;

public str ppl(str section, CBGFSequence c)
{
	if (isEmpty(c))
		return "";
	else
		return "\\section{<section>}\n<ppl(c)>";
}

public str ppl(CBGFSequence c)
{
	if (isEmpty(c)) return "(no transformation steps)";
	str s = "{\\footnotesize\\begin{itemize}\n";
	for (CBGFCommand step <- c)
		s += "\\item <ppl(step)>\n";
	return s+"\\end{itemize}}";
}

// simple instruments to streamline pretty-printing of CBGF
str ppl_namebare(str c) = "\\textbf{<replaceAll(split("(",c)[0],"_","-")>}";
str ppl_name(str c) = ppl_namebare(c)+"\\\\";
str ppl_namescopebare(str c, XBGFScope w) = "\\textbf{<replaceAll(split("(",c)[0],"_","-")>} <ppl(w)>";
str ppl_namescope(str c, XBGFScope w) = ppl_namescopebare(c,w)+"\\\\";
str ppl_namentscope(str c, str s, XBGFScope w) = "\\textbf{<replaceAll(split("(",c)[0],"_","-")>} $<s>$ <ppl(w)>\\\\";
str ppl_prod(BGFProduction p) = "$<ppl(p)>$";
str ppl_prodL(BGFProduction p) = "$\\mathrm{p}\\left(\\fbox{\\text{`<p.label>\'}},<ppnt(p.lhs)>,<ppl(p.rhs)>\\right)$";
str ppl_prods(BGFProdList ps) = joinStrings(["$<ppl(p)>$" | p <- ps],"\\\\");
str ppl_exprs(BGFExpression e1, BGFExpression e2) = "$<ppl(e1)>$\\\\$<ppl(e2)>$";

// CBGF
str ppl(c:abridge_detour(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:abstractize_concretize(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:addH_removeH(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:addV_removeV(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:anonymize_deanonymize(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:appear_disapper(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:assoc_iterate(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:chain_unchain(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:concretize_abstractize(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:deanonymize_anonymize(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:define_undefine(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
str ppl(c:designate_unlabel(BGFProduction p)) = ppl_name("<c>")+ppl_prodL(p);
str ppl(c:detour_abridge(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:deyaccify_yaccify(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
str ppl(c:disappear_appear(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:downgrade_upgrade(BGFProduction p1, BGFProduction p2)) = ppl_name("<c>")+ppl_prods([p1,p2]);
str ppl(c:eliminate_introduce(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
str ppl(c:extract_inline(BGFProduction p, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_prod(p);
str ppl(c:factor_factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_exprs(e1,e2);
str ppl(c:fold_unfold(str s, XBGFScope w)) = ppl_namentscope("<c>",s,w);
str ppl(c:horizontal_vertical(XBGFScope w)) = ppl_namescopebare("<c>",w);
str ppl(c:inject_project(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:inline_extract(BGFProduction p, globally())) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:inline_extract(BGFProduction p, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_prod(p);
str ppl(c:introduce_eliminate(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
str ppl(c:iterate_assoc(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:massage_massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_exprs(e1,e2);
str ppl(c:narrow_widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_exprs(e1,e2);
str ppl(c:permute_permute(BGFProduction p1, BGFProduction p2)) = ppl_name("<c>")+ppl_prods([p1,p2]);
str ppl(c:project_inject(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:removeH_addH(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:removeV_addV(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:renameN_renameN(str s1, str s2)) = ppl_namebare("<c>")+" $<ppnt(s1)>$ to $<ppnt(s2)>$";
str ppl(c:replace_replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_exprs(e1,e2);
str ppl(c:reroot_reroot(list[str] xs1, list[str] xs2)) = ppl_namebare("<c>")+" $<ppls(xs1)>$ to $<ppls(xs2)>$";
str ppl(c:unchain_chain(BGFProduction p)) = ppl_name("<c>")+ppl_prod(p);
str ppl(c:undefine_define(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
str ppl(c:unite_splitN(str x, BGFProdList ps, XBGFScope w)) = ppl_namentscope("<c>",x,w)+ppl_prods(ps); 
str ppl(c:unfold_fold(str s, XBGFScope w)) = ppl_namentscope("<c>",s,w);
str ppl(c:unlabel_designate(BGFProduction p)) = ppl_name("<c>")+ppl_prodL(p);
str ppl(c:upgrade_downgrade(BGFProduction p1, BGFProduction p2)) = ppl_name("<c>")+ppl_prods([p1,p2]);
str ppl(c:vertical_horizontal(XBGFScope w)) = ppl_namescopebare("<c>",w);
str ppl(c:widen_narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = ppl_namescope("<c>",w)+ppl_exprs(e1,e2);
str ppl(c:yaccify_deyaccify(BGFProdList ps)) = ppl_name("<c>")+ppl_prods(ps);
default str ppl(CBGFCommand step) = "?<step>?";

str ppl(BGFProdList ps) = joinStrings([ppl(p) | p <- ps],"\n");

str ppls(list[str] ss) = "\\left[<joinStrings([ppnt(s) | s <-ss],", ")>\\right]";
str ppl(inlabel(str s)) = " in $`<s>\'$";
str ppl(innt(str s)) = " in $<ppnt(s)>$";
str ppl(globally()) = "";

str ppnt("") = "\\omega";
str ppnt("STRING") = "str";
str ppnt("INTEGER") = "int";
default str ppnt(str n) = "\\mathit{<replace(n,("_":"\\_ ","1":"_1","2":"_2","3":"_3","4":"_4","5":"_5"))>}";

public str ppl(BGFProduction p) = "\\mathrm{p}\\left(\\text{`<p.label>\'},<ppnt(p.lhs)>,<ppl(p.rhs)>\\right)";

public str ppl(BGFGrammar g)
{
	str s =
	"\\footnotesize\\begin{center}\\begin{tabular}{|l|}\\hline
	'\\multicolumn{1}{|\>{\\columncolor[gray]{.9}}c|}{\\footnotesize \\textbf{Production rules}}
	'\\\\\\hline
	'";
	for (p <- g.prods)
		s += "$<pplv(p)>$	\\\\\n";
	return s + "\\hline\\end{tabular}\\end{center}";
}

public str ppls(BGFGrammar g)
{
	str s =
	"\\footnotesize\\begin{center}\\begin{tabular}{|l|c|}\\hline
	'\\multicolumn{1}{|\>{\\columncolor[gray]{.9}}c|}{\\footnotesize \\textbf{Production rule}} &
	'\\multicolumn{1}{\>{\\columncolor[gray]{.9}}c|}{\\footnotesize \\textbf{Production signature}}
	'\\\\\\hline
	'";
	for (p <- g.prods)
		s += "$<ppl(p)>$	&	$<ppl(analyse::Prodsigs::makesig(p))>$\\\\\n";
	return s + "\\hline\\end{tabular}\\end{center}";
}



// legacy
str pp(BGFGrammar g)
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

// legacy
//str pp(BGFProduction p) = "prod(`<p.label>\', \\emph{<p.lhs>}, <pp(p.rhs)>)";
//{
	// 	prod(`', \emph{program}, plus(nt(\emph{function})))	& $\{\langle \textit{function}, {+}\rangle\}$\\
	
//}
str ppl(epsilon()) = "\\varepsilon";
str ppl(empty()) = "\\varphi";
str ppl(anything()) = "\\alpha";
str ppl(val(string())) = "str";
str ppl(val(integer())) = "int";
str ppl(terminal("\\n")) = "{\\swarrow}";
str ppl(terminal(s)) = "\\text{`<s>\'}";
str ppl(nonterminal("STRING")) = "str";
str ppl(nonterminal("INTEGER")) = "int";
str ppl(nonterminal(s)) = ppnt(s);
str ppl(sequence(L)) = "\\mathrm{seq}\\left(\\left[<joinStrings([ppl(e) | e <- L],", ")>\\right]\\right)";
str ppl(choice(L)) = "\\mathrm{choice}\\left(\\left[<joinStrings([ppl(e) | e <- L],", ")>\\right]\\right)";

str pplv(BGFProduction p) = "\\mathrm{p}(\\text{`<p.label>\'},<ppnt(p.lhs)>,<pplv(p.rhs)>)";
str pplv(choice(L)) = "\\mathrm{choice}([<joinStrings([pplv(e) | e <- L],",$\\\\$\\qquad\\qquad")>])";
default str pplv(BGFExpression e) = ppl(e);

str ppl(optional(e)) = "\\opt \\left(<ppl(e)>\\right)";
str ppl(star(e)) = "\\star \\left(<ppl(e)>\\right)";
str ppl(plus(e)) = "\\plus \\left(<ppl(e)>\\right)";
default str ppl(BGFExpression e) = "?<e>?";

str ppl(selectable(s,e)) = "\\mathrm{sel}\\left(\\text{`<s>\'},<ppl(e)>\\right)";
str ppl(marked(e)) = "\\fbox{$<ppl(e)>$}";
str ppl(seplistplus(e1,e2)) = "\\mathrm{s}{+}\\left(<ppl(e1)>,<ppl(e2)>\\right)";
str ppl(sepliststar(e1,e2)) = "\\mathrm{s}{*}\\left(<ppl(e1)>,<ppl(e2)>\\right)";

public void main()
{
	//<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/xsd.normal.bgf|);
	println(pp(g));
}