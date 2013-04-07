@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{XBGF}
module transform::XBGF

import language::BGF;
import language::XBGF;
import language::XScope;
import language::XOutcome;
import transform::Library;
import normal::BGF;
import export::XBNF;
import IO;

public XBGFResult transform(abridge(BGFProduction p), BGFGrammar g)
	= transform::Library::runAbridge(p,g);
public XBGFResult transform(abstractize(BGFProduction p), BGFGrammar g)
	= transform::Library::runAbstractize(p,g);
public XBGFResult transform(addC(BGFProduction p), BGFGrammar g)
	= transform::Library::runAddC(p,g);
public XBGFResult transform(addH(BGFProduction p), BGFGrammar g)
	= transform::Library::runAddH(p,g);
public XBGFResult transform(addV(BGFProduction p), BGFGrammar g)
	= transform::Library::runAddV(p,g);
public XBGFResult transform(anonymize(BGFProduction p), BGFGrammar g)
	= transform::Library::runAnonymize(p,g);
public XBGFResult transform(appear(BGFProduction p), BGFGrammar g)
	= transform::Library::runAppear(p,g);
public XBGFResult transform(bypass(), BGFGrammar g)
	= <ok(),g>;
public XBGFResult transform(chain(BGFProduction p), BGFGrammar g)
	= transform::Library::runChain(p,g);
public XBGFResult transform(clone(str x, str y, XBGFScope w), BGFGrammar g)
	= transform::Library::runClone(x,y,w,g);
public XBGFResult transform(concatT(list[str] xs, str y, XBGFScope w), BGFGrammar g)
	= transform::Library::runConcatT(xs,y,w,g);
public XBGFResult transform(concretize(BGFProduction p), BGFGrammar g)
	= transform::Library::runConcretize(p,g);
public XBGFResult transform(deanonymize(BGFProduction p), BGFGrammar g)
	= transform::Library::runDeanonymize(p,g);
public XBGFResult transform(define(list[BGFProduction] ps), BGFGrammar g)
	= transform::Library::runDefine(ps,g);
public XBGFResult transform(designate(BGFProduction p), BGFGrammar g)
	= transform::Library::runDesignate(p,g);
public XBGFResult transform(detour(BGFProduction p), BGFGrammar g)
	= transform::Library::runDetour(p,g);
public XBGFResult transform(deyaccify(str x), BGFGrammar g)
	= transform::Library::runDeyaccify(x,g);
public XBGFResult transform(disappear(BGFProduction p), BGFGrammar g)
	= transform::Library::runDisappear(p,g);
public XBGFResult transform(distribute(XBGFScope w), BGFGrammar g)
	= transform::Library::runDistribute(w,g);
public XBGFResult transform(downgrade(BGFProduction p1,BGFProduction p2), BGFGrammar g)
	= transform::Library::runDowngrade(p1,p2,g);
public XBGFResult transform(eliminate(str x), BGFGrammar g)
	= transform::Library::runEliminate(x,g);
public XBGFResult transform(equate(str x, str y), BGFGrammar g)
	= transform::Library::runEquate(x,y,g);
public XBGFResult transform(extract(BGFProduction p, XBGFScope w), BGFGrammar g)
	= transform::Library::runExtract(p,w,g);
public XBGFResult transform(factor(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::Library::runFactor(e1,e2,w,g);
public XBGFResult transform(fold(str x, XBGFScope w), BGFGrammar g)
	= transform::Library::runFold(x,w,g);
public XBGFResult transform(horizontal(XBGFScope w), BGFGrammar g)
	= transform::Library::runHorizontal(w,g);
public XBGFResult transform(importG(list[BGFProduction] ps), BGFGrammar g)
	= transform::Library::runImportG(ps,g);
public XBGFResult transform(inject(BGFProduction p), BGFGrammar g)
	= transform::Library::runInject(p,g);
public XBGFResult transform(inline(str x), BGFGrammar g)
	= transform::Library::runInline(x,g);
public XBGFResult transform(introduce(list[BGFProduction] ps), BGFGrammar g)
	= transform::Library::runIntroduce(ps,g);
public XBGFResult transform(iterate(BGFProduction p), BGFGrammar g)
	= transform::Library::runIterate(p,g);
public XBGFResult transform(lassoc(BGFProduction p), BGFGrammar g)
	= transform::Library::runLAssoc(p,g);
public XBGFResult transform(massage(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::Library::runMassage(e1,e2,w,g);
public XBGFResult transform(narrow(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::Library::runNarrow(e1,e2,w,g);
public XBGFResult transform(permute(BGFProduction p), BGFGrammar g)
	= transform::Library::runPermute(p,g);
public XBGFResult transform(project(BGFProduction p), BGFGrammar g)
	= transform::Library::runProject(p,g);
public XBGFResult transform(rassoc(BGFProduction p), BGFGrammar g)
	= transform::Library::runRAssoc(p,g);
public XBGFResult transform(redefine(list[BGFProduction] ps), BGFGrammar g)
	= transform::Library::runRedefine(ps,g);
public XBGFResult transform(removeH(BGFProduction p), BGFGrammar g)
	= transform::Library::runRemoveH(p,g);
public XBGFResult transform(removeV(BGFProduction p), BGFGrammar g)
	= transform::Library::runRemoveV(p,g);
public XBGFResult transform(renameL(str x, str y), BGFGrammar g)
	= transform::Library::runRenameL(x,y,g);
public XBGFResult transform(renameN(str x, str y), BGFGrammar g)
	= transform::Library::runRenameN(x,y,g);
public XBGFResult transform(renameS(str x, str y, XBGFScope w), BGFGrammar g)
	= transform::Library::runRenameS(x,y,w,g);
public XBGFResult transform(renameT(str x, str y), BGFGrammar g)
	= transform::Library::runRenameT(x,y,g);
public XBGFResult transform(XBGFCommand::replace(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::Library::runReplace(e1,e2,w,g);
public XBGFResult transform(reroot(list[str] xs), BGFGrammar g)
	= transform::Library::runReroot(xs,g);
public XBGFResult transform(splitN(str x, list[BGFProduction] ps, XBGFScope w), BGFGrammar g)
	= transform::Library::runSplitN(x,ps,w,g);
public XBGFResult transform(splitT(str x, list[str] ys, XBGFScope w), BGFGrammar g)
	= transform::Library::runSplitT(x,ys,w,g);
public XBGFResult transform(unchain(BGFProduction p), BGFGrammar g)
	= transform::Library::runUnchain(p,g);
public XBGFResult transform(undefine(list[str] xs), BGFGrammar g)
	= transform::Library::runUndefine(xs,g);
public XBGFResult transform(unfold(str x, XBGFScope w), BGFGrammar g)
	= transform::Library::runUnfold(x,w,g);
public XBGFResult transform(unite(str x, str y), BGFGrammar g)
	= transform::Library::runUnite(x,y,g);
public XBGFResult transform(unlabel(str x), BGFGrammar g)
	= transform::Library::runUnlabel(x,g);
public XBGFResult transform(upgrade(BGFProduction p1, BGFProduction p2), BGFGrammar g)
	= transform::Library::runUpgrade(p1,p2,g);
public XBGFResult transform(vertical(XBGFScope w), BGFGrammar g)
	= transform::Library::runVertical(w,g);
public XBGFResult transform(widen(BGFExpression e1, BGFExpression e2, XBGFScope w), BGFGrammar g)
	= transform::Library::runWiden(e1,e2,w,g);
public XBGFResult transform(yaccify(list[BGFProduction] ps), BGFGrammar g)
	= transform::Library::runYaccify(ps,g);
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
