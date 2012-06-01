@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::XBGF

import IO;
import syntax::BGF;
import syntax::XBGF;
import normal::BGF;

public BGFGrammar transform(XBGFSequence xbgf, BGFGrammar g)
{
	BGFGrammar g1 = normalise(g);
	for (XBGFCommand step <- xbgf)
	{
		switch(step)
		{
			case abridge(BGFProduction p): g1 = runAbridge(p,g);
			case abstractize(BGFProduction p): g1 = runAbstractize(p,g);
			case addH(BGFProduction p): g1 = runAddH(p,g);
			case addV(BGFProduction p): g1 = runAddV(p,g);
			case anonymize(BGFProduction p): g1 = runAnonymize(p,g);
			case appear(BGFProduction p): g1 = runAppear(p,g);
			case chain(BGFProduction p): g1 = runChain(p,g);
			case clone(str x, str y, XBGFContext w): g1 = runClone(x,y,w,g);
			case concatT(list[str] xs, str y, XBGFContext w): g1 = runConcatT(xs,y,w,g);
			case concretize(BGFProduction p): g1 = runConcretize(p,g);
			case deanonymize(BGFProduction p): g1 = runDeanonymize(p,g);
			case define(list[BGFProduction] ps): g1 = runDefine(ps,g);
			case designate(BGFProduction p): g1 = runDesignate(p,g);
			case detour(BGFProduction p): g1 = runDetour(p,g);
			case deyaccify(str x): g1 = runDeyaccify(x,g);
			case disappear(BGFProduction p): g1 = runDisappear(p,g);
			case distribute(XBGFContext w): g1 = runDistribute(w,g);
			case downgrade(BGFProduction p1,BGFProduction p2): g1 = runDowngrade(p1,p2,g);
			case eliminate(str x): g1 = runEliminate(x,g);
			case equate(str x, str y): g1 = runEquate(x,y,g);
			case extract(BGFProduction p, XBGFContext w): g1 = runExtract(p,w,g);
			case factor(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runFactor(e1,e2,w,g);
			case fold(str x, XBGFContext w): g1 = runFold(x,w,g);
			case horizontal(XBGFContext w): g1 = runHorizontal(w,g);
			case \import(list[BGFProduction] ps): g1 = runImport(ps,g);
			case inject(BGFProduction p): g1 = runInject(p,g);
			case inline(str x): g1 = runInline(x,g);
			case introduce(list[BGFProduction] ps): g1 = runIntroduce(ps,g);
			case iterate(BGFProduction p): g1 = runIterate(p,g);
			case lassoc(BGFProduction p): g1 = runLAssoc(p,g);
			case massage(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runMassage(e1,e2,w,g);
			case narrow(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runNarrow(e1,e2,w,g);
			case permute(BGFProduction p): g1 = runPermute(p,g);
			case project(BGFProduction p): g1 = runProject(p,g);
			case rassoc(BGFProduction p): g1 = runRAssoc(p,g);
			case redefine(list[BGFProduction] ps): g1 = runRedefine(ps,g);
			case removeH(BGFProduction p): g1 = runRemoveH(p,g);
			case removeV(BGFProduction p): g1 = runRemoveV(p,g);
			case renameL(str x, str y, XBGFContext w): g1 = runRenameL(x,y,w,g);
			case renameN(str x, str y, XBGFContext w): g1 = runRenameN(x,y,w,g);
			case renameS(str x, str y, XBGFContext w): g1 = runRenameS(x,y,w,g);
			case renameT(str x, str y, XBGFContext w): g1 = runRenameT(x,y,w,g);
			case replace(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runReplace(e1,e2,w,g);
			case reroot(list[str] xs): g1 = runReroot(xs,g);
			case splitN(str x, list[BGFProduction] ps, XBGFContext w): g1 = runSplitN(x,ps,w,g);
			case splitT(str x, list[str] ys, XBGFContext w): g1 = runSplitT(x,ys,w,g);
			case unchain(BGFProduction p): g1 = runUnchain(p,g);
			case undefine(list[str] xs): g1 = runUndefine(xs,g);
			case unfold(str x, XBGFContext w): g1 = runUnfold(x,w,g);
			case unite(str x, str y): g1 = runUnite(x,y,g);
			case unlabel(str x): g1 = runUnlabel(x,g);
			case upgrade(BGFProduction p1, BGFProduction p2): g1 = runUpgrade(p1,p2,g);
			case vertical(XBGFContext w): g1 = runVertical(w,g);
			case widen(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runWiden(e1,e2,w,g);
			case yaccify(list[BGFProduction] ps): g1 = runYaccify(ps,g);
			case atomic(list[XBGFCommand] steps): g1 = runAtomic(steps,g);
			case strip(str a): g1 = runStrip(a,g);
			default: throw "Unknown XBGF command <step>";
		}
	}
	return g1;
}

BGFGrammar runAbridge(BGFProduction prod, grammar(roots,prods))
{
	if (production(_,x,nonterminal(x)) !:= prod)
		throw "Production <prod> cannot be abridged.";
	if (prod notin prods)
		throw "Production <prod> not found.";
	return grammar(roots, prods - prod);
}

BGFGrammar runAbstractize(BGFProduction p1, grammar(roots,ps))
{
	if (unmark(p1) notin ps)
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Abstractize only works with marked terminals, use project instead.";
	return runProject(p1,grammar(roots, ps));
}

BGFGrammar runAddH(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runAddV(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runAnonymize(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runAppear(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runChain(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runClone(str x, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runConcatT(list[str] xs, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runConcretize(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDeanonymize(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDefine(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDesignate(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDetour(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDeyaccify(str x, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDisappear(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDistribute(XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runDowngrade(BGFProduction p1,BGFProduction p2, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runEliminate(str x, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runEquate(str x, str y, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runExtract(BGFProduction p, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runFactor(BGFExpression e1, BGFExpression e2, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runFold(str x, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runHorizontal(XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runImport(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runInject(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runInline(str x, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runIntroduce(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runIterate(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runLAssoc(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runMassage(BGFExpression e1, BGFExpression e2, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runNarrow(BGFExpression e1, BGFExpression e2, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runPermute(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runProject(BGFProduction p1, grammar(roots, ps))
{
	if (/marked(e) !:= p1)
		throw "<p1> must contain markers.";
	p2 = unmark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	p3 = demark(p1);
	return grammar(roots, ps - p2 + p3);
}

BGFGrammar runRAssoc(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRedefine(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRemoveH(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRemoveV(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRenameL(str x, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRenameN(str x, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRenameS(str x, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runRenameT(str x, str y, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runReplace(BGFExpression e1, BGFExpression e2, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runReroot(list[str] xs, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runSplitN(str x, list[BGFProduction] ps, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runSplitT(str x, list[str] ys, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUnchain(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUndefine(list[str] xs, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUnfold(str x, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUnite(str x, str y, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUnlabel(str x, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runUpgrade(BGFProduction p1, BGFProduction p2, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runVertical(XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runWiden(BGFExpression e1, BGFExpression e2, XBGFContext w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runYaccify(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runAtomic(list[XBGFCommand] steps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runStrip(str a, BGFGrammar g)
{
	// TODO
	return g;
}

// END OF XBGF
BGFProduction unmark (BGFProduction p) = innermost visit(p)
	{
		case marked(BGFExpression e) => e
	};

BGFProduction demark (BGFProduction p) 
{
	p2 = innermost visit(p)
	{
		case marked(BGFExpression e) => epsilon()
	}
	return normalise(p2);
}

 