@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::CBGF // should be ΞBGF

import syntax::BGF;
import syntax::XBGF;
import syntax::CBGF;
import analyse::Metrics;
import Set;

// forward execution
public XBGFSequence forward(CBGFSequence cbgf) = [forward(step) | step <- cbgf];

XBGFCommand forward(abridge_detour(BGFProduction p)) = abridge(p);
XBGFCommand forward(abstractize_concretize(BGFProduction p)) = abstractize(p);
XBGFCommand forward(addH_removeH(BGFProduction p)) = addH(p);
XBGFCommand forward(addV_removeV(BGFProduction p)) = addV(p);
XBGFCommand forward(anonymize_deanonymize(BGFProduction p)) = anonymize(p);
XBGFCommand forward(appear_disapper(BGFProduction p)) = appear(p);
XBGFCommand forward(chain_unchain(BGFProduction p)) = chain(p);
XBGFCommand forward(clone_equate(str x, str y, XBGFScope w)) = clone(x,y,w);
XBGFCommand forward(concatT_splitT(str y, list[str] xs, XBGFScope w)) = concatT(xs,y,w);
XBGFCommand forward(concretize_abstractize(BGFProduction p)) = concretize(p);
XBGFCommand forward(deanonymize_anonymize(BGFProduction p)) = deanonymize(p);
XBGFCommand forward(define_undefine(list[BGFProduction] ps)) = define(p);
XBGFCommand forward(designate_unlabel(BGFProduction p)) = designate(p);
XBGFCommand forward(detour_abridge(BGFProduction p)) = detour(p);
XBGFCommand forward(deyaccify_yaccify(list[BGFProduction] ps))
	{
		if ({str x} := analyse::Metrics::definedNs(ps)) return deyaccify(x);
		else throw "<ps> must concern one nonterminal";
	}
XBGFCommand forward(disappear_appear(BGFProduction p)) = disappear(p);
XBGFCommand forward(downgrade_upgrade(BGFProduction p1,BGFProduction p2)) = downgrade(p1,p2);
XBGFCommand forward(eliminate_introduce(list[BGFProduction] ps))
	{
		if ({str x} := analyse::Metrics::definedNs(ps)) return eliminate(x);
		else throw "<ps> must concern one nonterminal";
	}
XBGFCommand forward(equate_clone(str x, str y, XBGFScope w)) = equate(x,y);
XBGFCommand forward(extract_inline(BGFProduction p, XBGFScope w)) = extract(p,w);
XBGFCommand forward(factor_factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = factor(e1,e2,w);
XBGFCommand forward(fold_unfold(str x, XBGFScope w)) = fold(x,w);
XBGFCommand forward(horizontal_vertical(XBGFScope w)) = horizontal(w);
XBGFCommand forward(inject_project(BGFProduction p)) = inject(p);
XBGFCommand forward(inline_extract(BGFProduction p, XBGFScope w)) = inline(p.lhs);
XBGFCommand forward(introduce_eliminate(list[BGFProduction] ps)) = introduce(ps);
XBGFCommand forward(iterate_assoc(BGFProduction p)) = iterate(p);
XBGFCommand forward(assoc_iterate(BGFProduction p)) = rassoc(p); // TODO
XBGFCommand forward(massage_massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = massage(e1,e2,w);
XBGFCommand forward(narrow_widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = narrow(e1,e2,w);
XBGFCommand forward(permute_permute(BGFProduction p1,BGFProduction p2)) = permute(p2);
XBGFCommand forward(project_inject(BGFProduction p)) = project(p);
XBGFCommand forward(redefine_redefine(_,list[BGFProduction] ps)) = redefine(ps);
XBGFCommand forward(removeH_addH(BGFProduction p)) = removeH(p);
XBGFCommand forward(removeV_addV(BGFProduction p)) = removeV(p);
XBGFCommand forward(renameL_renameL(str x, str y)) = renameL(x,y);
XBGFCommand forward(renameN_renameN(str x, str y)) = renameN(x,y);
XBGFCommand forward(renameS_renameS(str x, str y, XBGFScope w)) = renameS(x,y,w);
XBGFCommand forward(renameT_renameT(str x, str y)) = renameT(x,y);
XBGFCommand forward(replace_replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = replace(e1,e2,w);
XBGFCommand forward(reroot_reroot(_,list[str] xs)) = reroot(xs);
XBGFCommand forward(splitN_unite(str x, list[BGFProduction] ps, XBGFScope w)) = splitN(x,ps,w);
XBGFCommand forward(splitT_concatT(str x, list[str] ys, XBGFScope w)) = splitT(x,ys,w);
XBGFCommand forward(unchain_chain(BGFProduction p)) = unchain(p);
XBGFCommand forward(undefine_define(list[BGFProduction] ps)) = undefine(toList(analyse::Metrics::definedNs(ps)));
XBGFCommand forward(unfold_fold(str x, XBGFScope w)) = unfold(x,w);
XBGFCommand forward(unite_splitN(str x, list[BGFProduction] ps, XBGFScope w))
	{
		// TODO: w
		// TODO: check that x != y
		if ({str y} := analyse::Metrics::definedNs(ps)) return unite(x,y);
		else throw "<ps> must concern one nonterminal";
	}
XBGFCommand forward(unlabel_designate(production(str l,_,_))) = unlabel(l);
XBGFCommand forward(upgrade_downgrade(BGFProduction p1, BGFProduction p2)) = upgrade(p1,p2);
XBGFCommand forward(vertical_horizontal(XBGFScope w)) = vertical(w);
XBGFCommand forward(widen_narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = widen(e1,e2,w);
XBGFCommand forward(yaccify_deyaccify(list[BGFProduction] ps)) = yaccify(ps);
default XBGFCommand forward(CBGFCommand step) {throw "Unknown CBGF command: <step>";}

// reverse execution
public XBGFSequence reverse(CBGFSequence cbgf) = [reverse(step) | step <- cbgf];

XBGFCommand reverse(abridge_detour(BGFProduction p)) = detour(p);
XBGFCommand reverse(abstractize_concretize(BGFProduction p)) = concretize(p);
XBGFCommand reverse(addH_removeH(BGFProduction p)) = removeH(p);
XBGFCommand reverse(addV_removeV(BGFProduction p)) = removeV(p);
XBGFCommand reverse(anonymize_deanonymize(BGFProduction p)) = deanonymize(p);
XBGFCommand reverse(appear_disapper(BGFProduction p)) = disappear(p);
XBGFCommand reverse(chain_unchain(BGFProduction p)) = unchain(p);
XBGFCommand reverse(clone_equate(str x, str y, XBGFScope w)) = equate(x,y);
XBGFCommand reverse(concatT_splitT(str y, list[str] xs, XBGFScope w)) = splitT(y,xs,w);
XBGFCommand reverse(concretize_abstractize(BGFProduction p)) = abstractize(p);
XBGFCommand reverse(deanonymize_anonymize(BGFProduction p)) = anonymize(p);
XBGFCommand reverse(define_undefine(list[BGFProduction] ps)) = undefine(toList(analyse::Metrics::definedNs(ps)));
XBGFCommand reverse(designate_unlabel(production(str l,_,_))) = unlabel(l);
XBGFCommand reverse(detour_abridge(BGFProduction p)) = abridge(p);
XBGFCommand reverse(deyaccify_yaccify(list[BGFProduction] ps)) = yaccify(ps);
XBGFCommand reverse(disappear_appear(BGFProduction p)) = appear(p);
XBGFCommand reverse(downgrade_upgrade(BGFProduction p1,BGFProduction p2)) = upgrade(p1,p2);
XBGFCommand reverse(eliminate_introduce(list[BGFProduction] ps)) = introduce(ps);
XBGFCommand reverse(equate_clone(str x, str y, XBGFScope w)) = clone(x,y,w);
XBGFCommand reverse(extract_inline(BGFProduction p, XBGFScope w)) = inline(p.lhs);
XBGFCommand reverse(factor_factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = factor(e2,e1,w);
XBGFCommand reverse(fold_unfold(str x, XBGFScope w)) = unfold(x,w);
XBGFCommand reverse(horizontal_vertical(XBGFScope w)) = vertical(w);
XBGFCommand reverse(inject_project(BGFProduction p)) = project(p);
XBGFCommand reverse(inline_extract(BGFProduction p, XBGFScope w)) = extract(p,w);
XBGFCommand reverse(introduce_eliminate(list[BGFProduction] ps))
	{
		if ({str x} := analyse::Metrics::definedNs(ps)) return eliminate(x);
		else throw "<ps> must concern one nonterminal";
	}
XBGFCommand reverse(iterate_assoc(BGFProduction p)) = rassoc(p); // TODO
XBGFCommand reverse(assoc_iterate(BGFProduction p)) = iterate(p);
XBGFCommand reverse(massage_massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = massage(e2,e1,w);
XBGFCommand reverse(narrow_widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = narrow(e2,e1,w);
XBGFCommand reverse(permute_permute(BGFProduction p,_)) = permute(p);
XBGFCommand reverse(project_inject(BGFProduction p)) = inject(p);
XBGFCommand reverse(redefine_redefine(list[BGFProduction] ps,_)) = redefine(ps);
XBGFCommand reverse(removeH_addH(BGFProduction p)) = addH(p);
XBGFCommand reverse(removeV_addV(BGFProduction p)) = addV(p);
XBGFCommand reverse(renameL_renameL(str x, str y)) = renameL(y,x);
XBGFCommand reverse(renameN_renameN(str x, str y)) = renameN(y,x);
XBGFCommand reverse(renameS_renameS(str x, str y, XBGFScope w)) = renameS(y,x,w);
XBGFCommand reverse(renameT_renameT(str x, str y)) = renameT(y,x);
XBGFCommand reverse(replace_replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = replace(e2,e1,w);
XBGFCommand reverse(reroot_reroot(list[str] xs,_)) = reroot(xs);
XBGFCommand reverse(splitN_unite(str x, list[BGFProduction] ps, XBGFScope w))
	{
		// TODO: w
		// TODO: check that x != y
		if ({str y} := analyse::Metrics::definedNs(ps)) return unite(x,y);
		else throw "<ps> must concern one nonterminal";
	}
XBGFCommand reverse(splitT_concatT(str x, list[str] ys, XBGFScope w)) = splitT(x,ys,w);
XBGFCommand reverse(unchain_chain(BGFProduction p)) = chain(p);
XBGFCommand reverse(undefine_define(list[BGFProduction] ps)) = define(ps);
XBGFCommand reverse(unfold_fold(str x, XBGFScope w)) = fold(x,w);
XBGFCommand reverse(unite_splitN(str x, list[BGFProduction] ps, XBGFScope w)) = splitN(x,ps,w);
XBGFCommand reverse(unlabel_designate(BGFProduction p)) = designate(p);
XBGFCommand reverse(upgrade_downgrade(BGFProduction p1, BGFProduction p2)) = downgrade(p1,p2);
XBGFCommand reverse(vertical_horizontal(XBGFScope w)) = horizontal(w);
XBGFCommand reverse(widen_narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = narrow(e2,e1,w);
XBGFCommand reverse(yaccify_deyaccify(list[BGFProduction] ps))
	{
		if ({str x} := analyse::Metrics::definedNs(ps)) return deyaccify(x);
		else throw "<ps> must concern one nonterminal";
	}
default XBGFCommand reverse(CBGFCommand step) {throw "Unknown CBGF command: <step>";}
