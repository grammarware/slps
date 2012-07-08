@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::LaTeX

import IO;
import List;
import String;
import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
//import io::ReadBGF;
import converge::Guided;
import lib::Rascalware;
import analyse::Prodsigs;

public str ppl(CBGFSequence c)
{
	if (isEmpty(c)) return "(no transformation steps)";
	str s = "{\\footnotesize\\begin{itemize}\n";
	for (CBGFCommand step <- c)
		s += "\\item $<ppl(step)>$\n";
	return s+"\\end{itemize}}";
}

// CBGF
str ppl(abridge_detour(BGFProduction p)) = "\\mathbf{abridge\\text{-}detour}(<ppl(p)>)";
str ppl(abstractize_concretize(BGFProduction p)) = "\\mathbf{abstractize\\text{-}concretize}(<ppl(p)>)";
str ppl(addH_removeH(BGFProduction p)) = "\\mathbf{addH\\text{-}removeH}(<ppl(p)>)";
str ppl(addV_removeV(BGFProduction p)) = "\\mathbf{addV\\text{-}removeV}(<ppl(p)>)";
str ppl(anonymize_deanonymize(BGFProduction p)) = "\\mathbf{anonymize\\text{-}deanonymize}(<ppl(p)>)";
str ppl(appear_disapper(BGFProduction p)) = "\\mathbf{appear\\text{-}disapper}(<ppl(p)>)";
str ppl(chain_unchain(BGFProduction p)) = "\\mathbf{chain\\text{-}unchain}(<ppl(p)>)";
str ppl(concretize_abstractize(BGFProduction p)) = "\\mathbf{concretize\\text{-}abstractize}(<ppl(p)>)";
str ppl(deanonymize_anonymize(BGFProduction p)) = "\\mathbf{deanonymize\\text{-}anonymize}(<ppl(p)>)";
str ppl(define_undefine(list[BGFProduction] ps)) = "\\mathbf{define\\text{-}undefine}(<ppl(ps)>)";
str ppl(designate_unlabel(BGFProduction p)) = "\\mathbf{designate\\text{-}unlabel}(<ppl(p)>)";
str ppl(detour_abridge(BGFProduction p)) = "\\mathbf{detour\\text{-}abridge}(<ppl(p)>)";
str ppl(deyaccify_yaccify(list[BGFProduction] ps)) = "\\mathbf{deyaccify\\text{-}yaccify}(<ppl(ps)>)";
str ppl(disappear_appear(BGFProduction p)) = "\\mathbf{disappear\\text{-}appear}(<ppl(p)>)";
str ppl(downgrade_upgrade(BGFProduction p1,BGFProduction p2)) = "\\mathbf{downgrade\\text{-}upgrade}(<ppl(p1)>,<ppl(p2)>)";
str ppl(eliminate_introduce(list[BGFProduction] ps)) = "\\mathbf{eliminate\\text{-}introduce}(<ppl(ps)>)";
str ppl(extract_inline(BGFProduction p, XBGFScope w)) = "\\mathbf{extract\\text{-}inline}(<ppl(p)>,<ppl(w)>)";
str ppl(factor_factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{factor\\text{-}factor}(<ppl(e1)>,<ppl(e2)>,<ppl(w)>)";
str ppl(fold_unfold(str s, globally())) = "\\mathbf{fold\\text{-}unfold}(<ppnt(s)>)";
str ppl(fold_unfold(str s, XBGFScope w)) = "\\mathbf{fold\\text{-}unfold}(<ppnt(s)>,<ppl(w)>)";
str ppl(horizontal_vertical(XBGFScope w)) = "\\mathbf{horizontal\\text{-}vertical}(<ppl(w)>)";
str ppl(inject_project(BGFProduction p)) = "\\mathbf{inject\\text{-}project}(<ppl(p)>)";
str ppl(inline_extract(BGFProduction p, globally())) = "\\mathbf{inline\\text{-}extract}(<ppl(p)>)";
str ppl(inline_extract(BGFProduction p, XBGFScope w)) = "\\mathbf{inline\\text{-}extract}(<ppl(p)>,<ppl(w)>)";
str ppl(introduce_eliminate(list[BGFProduction] ps)) = "\\mathbf{introduce\\text{-}eliminate}(<ppl(ps)>)";
str ppl(iterate_assoc(BGFProduction p)) = "\\mathbf{iterate\\text{-}assoc}(<ppl(p)>)";
str ppl(assoc_iterate(BGFProduction p)) = "\\mathbf{assoc\\text{-}iterate}(<ppl(p)>)";
str ppl(massage_massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{massage\\text{-}massage}(<ppl(e1)>,<ppl(e2)>,<ppl(w)>)";
str ppl(narrow_widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{narrow\\text{-}widen}(<ppl(e1)>,<ppl(e2)>,<ppl(w)>)";
str ppl(permute_permute(BGFProduction p1, BGFProduction p2)) = "\\mathbf{permute\\text{-}permute}(<ppl(p1)>,\\\\<ppl(p2)>)";
str ppl(project_inject(BGFProduction p)) = "\\mathbf{project\\text{-}inject}(<ppl(p)>)";
str ppl(removeH_addH(BGFProduction p)) = "\\mathbf{removeH\\text{-}addH}(<ppl(p)>)";
str ppl(removeV_addV(BGFProduction p)) = "\\mathbf{removeV\\text{-}addV}(<ppl(p)>)";
str ppl(renameN_renameN(str s1, str s2)) = "\\mathbf{renameN\\text{-}renameN}(<ppnt(s1)>,<ppnt(s2)>)";
str ppl(replace_replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{replace\\text{-}replace}(<ppl(e1)>,<ppl(e2)>,<ppl(w)>)";
str ppl(reroot_reroot(list[str] xs1, list[str] xs2)) = "\\mathbf{reroot\\text{-}reroot}(<ppls(xs1)>,<ppls(xs2)>)";
str ppl(unchain_chain(BGFProduction p)) = "\\mathbf{unchain\\text{-}chain}(<ppl(p)>)";
str ppl(undefine_define(list[BGFProduction] ps)) = "\\mathbf{undefine\\text{-}define}(<ppl(ps)>)";
str ppl(unite_splitN(str x, list[BGFProduction] ps, XBGFScope w)) = "\\mathbf{unite\\text{-}splitN}(<ppnt(x)>,<joinStrings([ppl(p)|p<-ps],"\\\\")>)";
str ppl(unfold_fold(str s, globally())) = "\\mathbf{unfold\\text{-}fold}(<ppnt(s)>)";
str ppl(unfold_fold(str s, XBGFScope w)) = "\\mathbf{unfold\\text{-}fold}(<ppnt(s)>,<ppl(w)>)";
str ppl(unlabel_designate(BGFProduction p)) = "\\mathbf{unlabel\\text{-}designate}(<ppl(p)>)";
str ppl(upgrade_downgrade(BGFProduction p1, BGFProduction p2)) = "\\mathbf{upgrade\\text{-}downgrade}(<ppl(p1)>,<ppl(p2)>)";
str ppl(vertical_horizontal(XBGFScope w)) = "\\mathbf{vertical\\text{-}horizontal}(<ppl(w)>)";
str ppl(widen_narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{widen\\text{-}narrow}(<ppl(e1)>,<ppl(e2)>,<ppl(w)>)";
str ppl(yaccify_deyaccify(list[BGFProduction] ps)) = "\\mathbf{yaccify\\text{-}deyaccify}(<ppl(ps)>)";
default str ppl(CBGFCommand step) = "?<step>?";

str ppl(BGFProdList ps) = joinStrings([ppl(p) | p <- ps],"\n");

str ppls(list[str] ss) = "[<joinStrings([ppnt(s) | s <-ss],", ")>]";
str ppl(inlabel(str s)) = "\\mathrm{in}(`<s>\')";
str ppl(innt(str s)) = "\\mathrm{in}(<s>)";
str ppl(globally()) = "";

str ppnt(str n) = "\\mathit{<replace(n,("_":"\\_ ","1":"_1","2":"_2","3":"_3","4":"_4","5":"_5"))>}";

public str ppl(BGFProduction p) = "\\mathrm{p}(\\text{`<p.label>\'},<ppnt(p.lhs)>,<ppl(p.rhs)>)";

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
str ppl(sequence(L))
{
	str s = ppl(L[0]);
	for (e <- tail(L))
		s += ", <ppl(e)>";
	return "\\mathrm{seq}([<s>])";
}
str ppl(choice(L)) = "\\mathrm{choice}(<joinStrings([ppl(e) | e <- L],", ")>)";

str pplv(BGFProduction p) = "\\mathrm{p}(\\text{`<p.label>\'},<ppnt(p.lhs)>,<pplv(p.rhs)>)";
str pplv(choice(L)) = "\\mathrm{choice}(<joinStrings([pplv(e) | e <- L],"$\\\\$\\qquad\\qquad")>)";
default str pplv(BGFExpression e) = ppl(e);

str ppl(optional(e)) = "{?}(<ppl(e)>)";
str ppl(star(e)) = "{*}(<ppl(e)>)";
str ppl(plus(e)) = "{+}(<ppl(e)>)";
default str ppl(BGFExpression e) = "?<e>?";

str ppl(selectable(s,e)) = "\\mathrm{sel}(\\text{`<s>\'},<ppl(e)>)";
str ppl(marked(e)) = "\\langle <ppl(e)> \\rangle";
str ppl(seplistplus(e1,e2)) = "\\mathrm{s}{+}(<ppl(e1)>,<ppl(e2)>)";
str ppl(sepliststar(e1,e2)) = "\\mathrm{s}{*}(<ppl(e1)>,<ppl(e2)>)";

public void main()
{
	//<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/master.bgf|);
	<_,g> = converge::Guided::loadSimpleGrammar(|home:///projects/slps/topics/convergence/guided/bgf/xsd.normal.bgf|);
	println(pp(g));
}