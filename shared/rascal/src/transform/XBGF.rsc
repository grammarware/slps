@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{XBGF}
module transform::XBGF

import syntax::BGF;
import syntax::XBGF;
import normal::BGF;
import transform::library::Associativity; // assoc, iterate
import transform::library::Brutal; // replace
import transform::library::Chaining; // abridge, detour, chain, unchain
import transform::library::Factoring; // factor, distribute
import transform::library::Folding; // fold, unfold, extract, inline, downgrade, upgrade
import transform::library::Intermittent; // addH, removeH, vertical, horizontal
import transform::library::Labels; // renameL, unlabel, designate; renameS, anonymize, deanonimize
import transform::library::Massage; // massage
import transform::library::Nonterminals; // renameN, equate, splitN, clone, reroot, unite
import transform::library::Productions; // addV, removeV, define, undefine, redefine, eliminate, introduce, import
import transform::library::Sequential; // appear, disappear, inject, permute, project
import transform::library::Terminals; // renameT, splitT, concatT, abstractize, concretize
import transform::library::Width; // narrow, widen
import transform::library::Yacc; // yaccify, deyaccify
import transform::library::Util;
import transform::Results;
import IO;
import export::XBNF;

public XBGFResult transform(abridge(BGFProduction p), BGFGrammar g)
	= transform::library::Chaining::runAbridge(p,g);
public XBGFResult transform(abstractize(BGFProduction p), BGFGrammar g)
	= transform::library::Terminals::runAbstractize(p,g);
public XBGFResult transform(addH(BGFProduction p), BGFGrammar g)
	= transform::library::Intermittent::runAddH(p,g);
public XBGFResult transform(addV(BGFProduction p), BGFGrammar g)
	= transform::library::Productions::runAddV(p,g);
public XBGFResult transform(anonymize(BGFProduction p), BGFGrammar g)
	= transform::library::Labels::runAnonymize(p,g);
public XBGFResult transform(appear(BGFProduction p), BGFGrammar g)
	= transform::library::Sequential::runAppear(p,g);
public XBGFResult transform(bypass(), BGFGrammar g)
	= <ok(),g>;
public XBGFResult transform(chain(BGFProduction p), BGFGrammar g)
	= transform::library::Chaining::runChain(p,g);
public XBGFResult transform(clone(str x, str y, XBGFScope w), BGFGrammar g)
	= transform::library::Nonterminals::runClone(x,y,w,g);
public XBGFResult transform(concatT(list[str] xs, str y, XBGFScope w), BGFGrammar g)
	= transform::library::Terminals::runConcatT(xs,y,w,g);
public XBGFResult transform(concretize(BGFProduction p), BGFGrammar g)
	= transform::library::Terminals::runConcretize(p,g);
public XBGFResult transform(deanonymize(BGFProduction p), BGFGrammar g)
	= transform::library::Labels::runDeanonymize(p,g);
public XBGFResult transform(define(list[BGFProduction] ps), BGFGrammar g)
	= transform::library::Productions::runDefine(ps,g);
public XBGFResult transform(designate(BGFProduction p), BGFGrammar g)
	= transform::library::Labels::runDesignate(p,g);
public XBGFResult transform(detour(BGFProduction p), BGFGrammar g)
	= transform::library::Chaining::runDetour(p,g);
public XBGFResult transform(deyaccify(str x), BGFGrammar g)
	= transform::library::Yacc::runDeyaccify(x,g);
public XBGFResult transform(disappear(BGFProduction p), BGFGrammar g)
	= transform::library::Sequential::runDisappear(p,g);
public XBGFResult transform(distribute(XBGFScope w), BGFGrammar g)
	= transform::library::Factoring::runDistribute(w,g);
public XBGFResult transform(downgrade(BGFProduction p1,BGFProduction p2), BGFGrammar g)
	= transform::library::Folding::runDowngrade(p1,p2,g);
public XBGFResult transform(eliminate(str x), BGFGrammar g)
	= transform::library::Productions::runEliminate(x,g);
public XBGFResult transform(equate(str x, str y), BGFGrammar g)
	= transform::library::Nonterminals::runEquate(x,y,g);
public XBGFResult transform(extract(BGFProduction p, XBGFScope w), BGFGrammar g)
	= transform::library::Folding::runExtract(p,w,g);
public XBGFResult transform(factor(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::library::Factoring::runFactor(e1,e2,w,g);
public XBGFResult transform(fold(str x, XBGFScope w), BGFGrammar g)
	= transform::library::Folding::runFold(x,w,g);
public XBGFResult transform(horizontal(XBGFScope w), BGFGrammar g)
	= transform::library::Intermittent::runHorizontal(w,g);
public XBGFResult transform(importG(list[BGFProduction] ps), BGFGrammar g)
	= transform::library::Productions::runImportG(ps,g);
public XBGFResult transform(inject(BGFProduction p), BGFGrammar g)
	= transform::library::Sequential::runInject(p,g);
public XBGFResult transform(inline(str x), BGFGrammar g)
	= transform::library::Folding::runInline(x,g);
public XBGFResult transform(introduce(list[BGFProduction] ps), BGFGrammar g)
	= transform::library::Productions::runIntroduce(ps,g);
public XBGFResult transform(iterate(BGFProduction p), BGFGrammar g)
	= transform::library::Associativity::runIterate(p,g);
public XBGFResult transform(lassoc(BGFProduction p), BGFGrammar g)
	= transform::library::Associativity::runLAssoc(p,g);
public XBGFResult transform(massage(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::library::Massage::runMassage(e1,e2,w,g);
public XBGFResult transform(narrow(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::library::Width::runNarrow(e1,e2,w,g);
public XBGFResult transform(permute(BGFProduction p), BGFGrammar g)
	= transform::library::Sequential::runPermute(p,g);
public XBGFResult transform(project(BGFProduction p), BGFGrammar g)
	= transform::library::Sequential::runProject(p,g);
public XBGFResult transform(rassoc(BGFProduction p), BGFGrammar g)
	= transform::library::Associativity::runRAssoc(p,g);
public XBGFResult transform(redefine(list[BGFProduction] ps), BGFGrammar g)
	= transform::library::Productions::runRedefine(ps,g);
public XBGFResult transform(removeH(BGFProduction p), BGFGrammar g)
	= transform::library::Intermittent::runRemoveH(p,g);
public XBGFResult transform(removeV(BGFProduction p), BGFGrammar g)
	= transform::library::Productions::runRemoveV(p,g);
public XBGFResult transform(renameL(str x, str y), BGFGrammar g)
	= transform::library::Labels::runRenameL(x,y,g);
public XBGFResult transform(renameN(str x, str y), BGFGrammar g)
	= transform::library::Nonterminals::runRenameN(x,y,g);
public XBGFResult transform(renameS(str x, str y, XBGFScope w), BGFGrammar g)
	= transform::library::Labels::runRenameS(x,y,w,g);
public XBGFResult transform(renameT(str x, str y), BGFGrammar g)
	= transform::library::Terminals::runRenameT(x,y,g);
public XBGFResult transform(XBGFCommand::replace(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::library::Brutal::runReplace(e1,e2,w,g);
public XBGFResult transform(reroot(list[str] xs), BGFGrammar g)
	= transform::library::Nonterminals::runReroot(xs,g);
public XBGFResult transform(splitN(str x, list[BGFProduction] ps, XBGFScope w), BGFGrammar g)
	= transform::library::Nonterminals::runSplitN(x,ps,w,g);
public XBGFResult transform(splitT(str x, list[str] ys, XBGFScope w), BGFGrammar g)
	= transform::library::Terminals::runSplitT(x,ys,w,g);
public XBGFResult transform(unchain(BGFProduction p), BGFGrammar g)
	= transform::library::Chaining::runUnchain(p,g);
public XBGFResult transform(undefine(list[str] xs), BGFGrammar g)
	= transform::library::Productions::runUndefine(xs,g);
public XBGFResult transform(unfold(str x, XBGFScope w), BGFGrammar g)
	= transform::library::Folding::runUnfold(x,w,g);
public XBGFResult transform(unite(str x, str y), BGFGrammar g)
	= transform::library::Nonterminals::runUnite(x,y,g);
public XBGFResult transform(unlabel(str x), BGFGrammar g)
	= transform::library::Labels::runUnlabel(x,g);
public XBGFResult transform(upgrade(BGFProduction p1, BGFProduction p2), BGFGrammar g)
	= transform::library::Folding::runUpgrade(p1,p2,g);
public XBGFResult transform(vertical(XBGFScope w), BGFGrammar g)
	= transform::library::Intermittent::runVertical(w,g);
public XBGFResult transform(widen(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::library::Width::runWiden(e1,e2,w,g);
public XBGFResult transform(yaccify(list[BGFProduction] ps), BGFGrammar g)
	= transform::library::Yacc::runYaccify(ps,g);
public XBGFResult transform(atomic(list[XBGFCommand] steps), BGFGrammar g)
	= transform(steps,g); // NB: different from the rest
public XBGFResult transform(strip(str a), BGFGrammar g)
	= runStrip(a,g); // semi-deprecated
public default XBGFResult transform(XBGFCommand x, BGFGrammar g) {throw "Unknown XBGF command <x>";}

public BGFGrammar transform(XBGFSequence xbgf, BGFGrammar g)
{
	XBGFResult out = <ok(),normalise(g)>;
	for (XBGFCommand step <- xbgf)
	{
		out = transform(step,out.g);
		thw(out.r);
		out.g = normalise(out.g);
	}
	return out.g;
}

// TODO: later redo with keyword parameters?
public XBGFResult vtransform(XBGFCommand x, BGFGrammar g)
{
	println("[XBGF] <ppx(x)>");
	return transform(x,g);
}

public BGFGrammar vtransform(XBGFSequence xbgf, BGFGrammar g)
{
	XBGFResult out = <ok(),normalise(g)>;
	for (XBGFCommand step <- xbgf)
	{
		out = vtransform(step,out.g);
		thw(out.r);
		out.g = normalise(out.g);
	}
	return out.g;
}



// legacy code
XBGFResult runStrip(str a, BGFGrammar g)
{
	// TODO: semi-deprecated
	list[BGFProduction] ps2;
	if (a=="allLabels")
		ps2 = visit(g.prods){case production(_,str x,BGFExpression e) => production("",x,e)}
	elseif (a=="allSelectors")
		ps2 = visit(g.prods){case selectable(_,BGFExpression e) => e}
	elseif (a=="allTerminals")
		{ // deprecated, please use a mutation that generates abstractize commands
			ps2 = visit(g.prods){case terminal(_) => epsilon()};
			ps2 = normalise(ps2);
		}
	else
		return <problemStr("Unknown strip parameter",a),g>;
	return <ok(),grammar(g.roots,ps2)>;
}
