@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{WriteΞBGF}
module io::WriteCBGF

import IO;
import language::BGF;
import language::XBGF;
import language::CBGF;
import lang::xml::DOM;
import io::WriteBGF;

public void writeCBGF(CBGFSequence cbgf, loc f)
{
	list[Node] xml = [cbgf2xml(x) | x <- cbgf];
	writeFile(f,xmlRaw(document(element(namespace("cbgf","http://planet-sl.org/cbgf"),"relationship",xml))));
}

Node cbgf2xml(CBGFCommand step)
{
	switch(step)
	{
		// TODO: commented lines produce code incompatible with SLPS
		case abridge_detour(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"abridge-detour",[prod2xml(p)]);
		case abstractize_concretize(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"abstractize-concretize",[prod2xml(p)]);
		case addH_removeH(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"addH-removeH",[prod2xml(p)]);
		case addV_removeV(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"addV-removeV",[prod2xml(p)]);
		case anonymize_deanonymize(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"anonymize-deanonymize",[prod2xml(p)]);
		case appear_disapper(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"appear-disapper",[prod2xml(p)]);
		case chain_unchain(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"chain-unchain",[prod2xml(p)]);
		//case clone_equate(str x, str y, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"clone-equate",[prod2xml(prod)]);
		//case concatT_splitT(str y, list[str] xs, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"concatT-splitT",[prod2xml(prod)]);
		case concretize_abstractize(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"concretize-abstractize",[prod2xml(p)]);
		case deanonymize_anonymize(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"deanonymize-anonymize",[prod2xml(p)]);
		case define_undefine(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"define-undefine",[prod2xml(p) | p <- ps]);
		case designate_unlabel(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"designate-unlabel",[prod2xml(p)]);
		case detour_abridge(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"detour-abridge",[prod2xml(p)]);
		case deyaccify_yaccify(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"deyaccify-yaccify",[prod2xml(p) | p <- ps]);
		case disappear_appear(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"disappear-appear",[prod2xml(p)]);
		case downgrade_upgrade(BGFProduction p1,BGFProduction p2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"downgrade-upgrade",[prod2xml(p1),prod2xml(p2)]);
		case eliminate_introduce(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"eliminate-introduce",[prod2xml(p) | p <- ps]);
		//case equate_clone(str x, str y, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"equate-clone",[prod2xml(prod)]);
		case extract_inline(BGFProduction p, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"extract-inline",[prod2xml(p),element(none(),"in",[context2xml(w)])]);
		case factor_factor(BGFExpression e1, BGFExpression e2, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"factor-factor",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case fold_unfold(str s, globally()): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"fold-unfold",[element(none(),"nonterminal",[charData(s)])]);
		case fold_unfold(str s, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"fold-unfold",[element(none(),"nonterminal",[charData(s)]),element(none(),"in",[context2xml(w)])]);
		case horizontal_vertical(XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"horizontal-vertical",[context2xml(w)]);
		case inject_project(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"inject-project",[prod2xml(p)]);
		case inline_extract(BGFProduction p, globally()): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"inline-extract",[prod2xml(p)]);
		case inline_extract(BGFProduction p, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"inline-extract",[prod2xml(p),element(none(),"in",[context2xml(w)])]);
		case introduce_eliminate(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"introduce-eliminate",[prod2xml(p) | p <- ps]);
		case iterate_assoc(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"iterate-assoc",[prod2xml(p)]);
		case assoc_iterate(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"assoc-iterate",[prod2xml(p)]);
		case massage_massage(BGFExpression e1, BGFExpression e2, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"massage-massage",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case narrow_widen(BGFExpression e1, BGFExpression e2, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"narrow-widen",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case permute_permute(BGFProduction p1, BGFProduction p2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"permute-permute",[prod2xml(p1),prod2xml(p2)]);
		case project_inject(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"project-inject",[prod2xml(p)]);
		//case redefine_redefine(list[BGFProduction] ps1, list[BGFProduction] ps2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"redefine-redefine",[prod2xml(prod)]);
		case removeH_addH(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"removeH-addH",[prod2xml(p)]);
		case removeV_addV(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"removeV-addV",[prod2xml(p)]);
		//case renameL_renameL(str x, str y): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"renameL-renameL",[prod2xml(prod)]);
		case renameN_renameN(str s1, str s2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"rename-rename",[element(none(),"nonterminal",[element(none(),"from",[charData(s1)]),element(none(),"to",[charData(s2)])])]);
		//case renameS_renameS(str x, str y, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"renameS-renameS",[prod2xml(prod),element(none(),"in",[context2xml(w)])]);
		//case renameT_renameT(str x, str y): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"renameT-renameT",[prod2xml(prod)]);
		case replace_replace(BGFExpression e1, BGFExpression e2, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"replace-replace",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case reroot_reroot(list[str] xs1, list[str] xs2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"reroot-reroot",[element(none(),"from",[element(none(),"root",[charData(r)]) | r <- xs1]),element(none(),"to",[element(none(),"root",[charData(r)]) | r <- xs2])]);
		//case splitN_unite(str x, list[BGFProduction] ps, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"splitN-unite",[prod2xml(prod),element(none(),"in",[context2xml(w)])]);
		//case splitT_concatT(str x, list[str] ys, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"splitT-concatT",[prod2xml(prod),element(none(),"in",[context2xml(w)])]);
		case unchain_chain(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"unchain-chain",[prod2xml(p)]);
		case undefine_define(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"undefine-define",[prod2xml(p) | p <- ps]);
		case unfold_fold(str s, globally()): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"unfold-fold",[element(none(),"nonterminal",[charData(s)])]);
		case unfold_fold(str s, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"unfold-fold",[element(none(),"nonterminal",[charData(s)]),element(none(),"in",[context2xml(w)])]);
		//case unite_splitN(str x, list[BGFProduction] ps, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"unite-splitN",[prod2xml(prod),element(none(),"in",[context2xml(w)])]);
		case unlabel_designate(BGFProduction p): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"unlabel-designate",[prod2xml(p)]);
		case upgrade_downgrade(BGFProduction p1, BGFProduction p2): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"upgrade-downgrade",[prod2xml(p1),prod2xml(p2)]);
		case vertical_horizontal(XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"vertical-horizontal",[context2xml(w)]);
		case widen_narrow(BGFExpression e1, BGFExpression e2, XBGFScope w): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"widen-narrow",[expr2xml(e1),expr2xml(e2),element(none(),"in",[context2xml(w)])]);
		case yaccify_deyaccify(list[BGFProduction] ps): return element(namespace("cbgf","http://planet-sl.org/cbgf"),"yaccify-deyaccify",[prod2xml(p) | p <- ps]);
		// default
		default:
			throw "ERROR: <step>";
	}
}

Node context2xml(XBGFScope w)
{
	switch(w)
	{
		case inlabel(str s): return element(none(),"label",[charData(s)]);
		case innt(str s): return element(none(),"nonterminal",[charData(s)]);
		case globally(): return comment("globally");
		default: throw "ERROR in context: <w>";
	}
}
