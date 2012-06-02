@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::XBGF

import IO;
import List;
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
			case abridge(BGFProduction p): g1 = runAbridge(p,g1);
			case abstractize(BGFProduction p): g1 = runAbstractize(p,g1);
			case addH(BGFProduction p): g1 = runAddH(p,g1);
			case addV(BGFProduction p): g1 = runAddV(p,g1);
			case anonymize(BGFProduction p): g1 = runAnonymize(p,g1);
			case appear(BGFProduction p): g1 = runAppear(p,g1);
			case chain(BGFProduction p): g1 = runChain(p,g1);
			case clone(str x, str y, XBGFContext w): g1 = runClone(x,y,w,g1);
			case concatT(list[str] xs, str y, XBGFContext w): g1 = runConcatT(xs,y,w,g1);
			case concretize(BGFProduction p): g1 = runConcretize(p,g1);
			case deanonymize(BGFProduction p): g1 = runDeanonymize(p,g1);
			case define(list[BGFProduction] ps): g1 = runDefine(ps,g1);
			case designate(BGFProduction p): g1 = runDesignate(p,g1);
			case detour(BGFProduction p): g1 = runDetour(p,g1);
			case deyaccify(str x): g1 = runDeyaccify(x,g1);
			case disappear(BGFProduction p): g1 = runDisappear(p,g1);
			case distribute(XBGFContext w): g1 = runDistribute(w,g1);
			case downgrade(BGFProduction p1,BGFProduction p2): g1 = runDowngrade(p1,p2,g1);
			case eliminate(str x): g1 = runEliminate(x,g1);
			case equate(str x, str y): g1 = runEquate(x,y,g1);
			case extract(BGFProduction p, XBGFContext w): g1 = runExtract(p,w,g1);
			case factor(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runFactor(e1,e2,w,g1);
			case fold(str x, XBGFContext w): g1 = runFold(x,w,g1);
			case horizontal(XBGFContext w): g1 = runHorizontal(w,g1);
			case \import(list[BGFProduction] ps): g1 = runImport(ps,g1);
			case inject(BGFProduction p): g1 = runInject(p,g1);
			case inline(str x): g1 = runInline(x,g1);
			case introduce(list[BGFProduction] ps): g1 = runIntroduce(ps,g1);
			case iterate(BGFProduction p): g1 = runIterate(p,g1);
			case lassoc(BGFProduction p): g1 = runLAssoc(p,g1);
			case massage(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runMassage(e1,e2,w,g1);
			case narrow(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runNarrow(e1,e2,w,g1);
			case permute(BGFProduction p): g1 = runPermute(p,g1);
			case project(BGFProduction p): g1 = runProject(p,g1);
			case rassoc(BGFProduction p): g1 = runRAssoc(p,g1);
			case redefine(list[BGFProduction] ps): g1 = runRedefine(ps,g1);
			case removeH(BGFProduction p): g1 = runRemoveH(p,g1);
			case removeV(BGFProduction p): g1 = runRemoveV(p,g1);
			case renameL(str x, str y, XBGFContext w): g1 = runRenameL(x,y,w,g1);
			case renameN(str x, str y, XBGFContext w): g1 = runRenameN(x,y,w,g1);
			case renameS(str x, str y, XBGFContext w): g1 = runRenameS(x,y,w,g1);
			case renameT(str x, str y, XBGFContext w): g1 = runRenameT(x,y,w,g1);
			case replace(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runReplace(e1,e2,w,g1);
			case reroot(list[str] xs): g1 = runReroot(xs,g1);
			case splitN(str x, list[BGFProduction] ps, XBGFContext w): g1 = runSplitN(x,ps,w,g1);
			case splitT(str x, list[str] ys, XBGFContext w): g1 = runSplitT(x,ys,w,g1);
			case unchain(BGFProduction p): g1 = runUnchain(p,g1);
			case undefine(list[str] xs): g1 = runUndefine(xs,g1);
			case unfold(str x, XBGFContext w): g1 = runUnfold(x,w,g1);
			case unite(str x, str y): g1 = runUnite(x,y,g1);
			case unlabel(str x): g1 = runUnlabel(x,g1);
			case upgrade(BGFProduction p1, BGFProduction p2): g1 = runUpgrade(p1,p2,g1);
			case vertical(XBGFContext w): g1 = runVertical(w,g1);
			case widen(BGFExpression e1, BGFExpression e2, XBGFContext w): g1 = runWiden(e1,e2,w,g1);
			case yaccify(list[BGFProduction] ps): g1 = runYaccify(ps,g1);
			case atomic(list[XBGFCommand] steps): g1 = runAtomic(steps,g1);
			case strip(str a): g1 = runStrip(a,g1);
			default: throw "Unknown XBGF command <step>";
		}
	}
	return g1;
}

BGFGrammar runAbridge(BGFProduction prod, grammar(roots, ps))
{
	if (production(_,x,nonterminal(x)) !:= prod)
		throw "Production <prod> cannot be abridged.";
	if (prod notin ps)
		throw "Production <prod> not found.";
	return grammar(roots, ps - prod);
}

BGFGrammar runAbstractize(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Abstractize only works with marked terminals, use project instead.";
	return runProject(p1,grammar(roots, ps));
}

BGFGrammar runAddH(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	p3 = demark(p1);
	if (p3 notin ps)
		throw "Production rule <p3> not found.";
	return grammar(roots, ps - p3 + p2);
}

BGFGrammar runAddV(BGFProduction p1, grammar(roots, ps))
{
	if (production(_,str x,_) := p1)
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		//ps2 = [p | p <- ps, production(_,x,_) := p];
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (p1 in ps2)
			throw "Production rule <p1> is already present.";
		if (production(str l,_,_) := p1 && l != "")
			if (production(str l,_,_) <- ps)
				throw "Another production rule with label <l> is already present.";
		return grammar(roots, ps1 + ps2 + p1 + ps3);
	}
}

BGFGrammar runAnonymize(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (p2 notin ps)
		throw "Production rule <p1> not found.";
	return grammar(roots, ps - p2 + p3);
}

BGFGrammar runAppear(BGFProduction p1, grammar(roots, ps))
{
	p2 = demark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return grammar(roots, ps - p2 + unmark(p1));
}

BGFGrammar runChain(BGFProduction p, grammar(roots, ps))
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not introduce reflexive chain productions with chain, use detour instead.";
			if (n2 in allNs(ps)) throw "Nonterminal <n2> must be fresh.";
			list[BGFProduction] ps1,ps2,ps3;
			if (l != "") <ps1,ps2,ps3> = splitPbyW(ps,inlabel(l));
			else <ps1,ps2,ps3> = splitPbyW(ps,innt(n1));
			if ([production(l,n1,e)] := ps2) return grammar(roots, ps1 + p + production("",n2,e) + ps3);
			else throw "Production rule <ps2> has unexpected form.";
		}
	else throw "Production <p> must be a chain production.";
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

BGFGrammar runConcretize(BGFProduction p1, grammar(roots, ps))
{
	p2 = demark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Concretize only works with marked terminals, use inject instead.";
	return runInject(p1,grammar(roots, ps));
}

BGFGrammar runDeanonymize(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (p3 notin ps)
		throw "Production rule <p1> not found.";
	return grammar(roots, ps - p3 + p2);
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

BGFGrammar runDisappear(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return grammar(roots, ps - p2 + demark(p1));
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

BGFGrammar runInject(BGFProduction p1, grammar(roots, ps))
{
	p2 = demark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	p3 = unmark(p1);
	return grammar(roots, ps - p2 + p3);
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

BGFGrammar runNarrow(BGFExpression e1, BGFExpression e2, XBGFContext w, g)
{
	if (!narrowing(e1,e2))
		throw "<e1> and <e2> are not in narrowing relation.";
	return runReplace(e1,e2,w,g); 
}

BGFGrammar runPermute(BGFProduction p, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runProject(BGFProduction p1, grammar(roots, ps))
{
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

BGFGrammar runRemoveH(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	p3 = demark(p1);
	if (p2 notin ps)
		throw "Production rule <p2> not found.";
	return grammar(roots, ps - p2 + p3);
}

BGFGrammar runRemoveV(BGFProduction p1, grammar(roots, ps))
{
	if (production(_,str x,_) := p1)
	{
		<_,ps2,_> = splitPbyW(ps,innt(x));
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (p1 notin ps2)
			throw "Production rule <p1> is not in the grammar.";
		if ([p1] == ps2)
			throw "Cannot remove the last production rule of <x> with removeV, use undefine or eliminate.";
		return grammar(roots, ps - p1);
	}
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

BGFGrammar runReplace(BGFExpression e1, BGFExpression e2, XBGFContext w, grammar(roots, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	e3 = normalise(e1);
	e4 = normalise(e2);
	//println(ps);
	ps4 = visit(ps2){case e3 => e4};
	if (ps2 == ps4)
		throw "Vacuous replace of <e3> by <e4> in context <w>.";
	return grammar(roots, ps1 + ps4 + ps3);
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

BGFGrammar runUnchain(BGFProduction p, grammar(roots, ps))
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not remove reflexive chain productions with chain, use abridge instead.";
			if (n2 in roots) throw "Nonterminal <n2> must not be root.";
			if (p notin ps) throw "<p> not found.";
			//if (n2 in allNs(ps)) throw "Nonterminal <n2> must be fresh.";
			list[BGFProduction] ps1,ps2,ps3;
			<ps1,ps2,ps3> = splitPbyW(ps - p,innt(n2));
			if (size(ps2) != 1) throw "Nonterminal <n2> must occur exactly once.";
			if (l == "") l = n2;
			if ([production(_,n2,e)] := ps2) return grammar(roots, ps1 + production(l,n1,e) + ps3);
			else throw "Production rule <ps2> has unexpected form.";
		}
	else throw "Production <p> must be a chain production.";
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
	if (!narrowing(e2,e1))
		throw "<e2> and <e1> are not in widening relation.";
	return runReplace(e1,e2,w,g); 
}

BGFGrammar runYaccify(list[BGFProduction] ps, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runAtomic(list[XBGFCommand] steps, BGFGrammar g) = transform(steps,g);

BGFGrammar runStrip(str a, BGFGrammar g)
{
	// TODO
	return g;
}

// END OF XBGF
BGFProduction unmark (BGFProduction p1)
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case marked(BGFExpression e) => e
	};
	return normalise(p2);
}

BGFProduction demark (BGFProduction p1) 
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case sequence([L1*,marked(BGFExpression e),L2*]) => sequence(L1 + L2)
		case choice([L1*,marked(BGFExpression e),L2*]) => choice(L1 + L2)
	}
	return normalise(p2);
}

// remove selectors from marked subexpressions
BGFProduction demarkS (BGFProduction p1) 
{
	if (/marked(_) !:= p1)
		throw "<p1> must contain markers.";
	p2 = innermost visit(p1)
	{
		case marked(selectable(str selector, BGFExpression expr)) => expr
	}
	return normalise(p2);
}

//tuple[list[BGFProduction],list[BGFProduction],list[BGFProduction]] splitN(list[BGFProduction] ps, str x)
//{
//	bool hit = false;
//	list[BGFProduction] ps1 = [];
//	list[BGFProduction] ps2 = [];
//	list[BGFProduction] ps3 = [];
//	for (p <- ps)
//		if (production (_,x,_) := p)
//			{
//				hit = true;
//				ps2 += p;
//			}
//		elseif (hit)
//			ps3 += p;
//		else
//			ps1 += p;
//	return <ps1,ps2,ps3>;
//}

// order-preserving splitting of production rules
// returns <prods before context; prods in context; prods after context> 
tuple[list[BGFProduction],list[BGFProduction],list[BGFProduction]] splitPbyW(list[BGFProduction] ps, XBGFContext w)
{
	if (globally() := w)
		return <[],ps,[]>;
	if (inlabel(str x) := w && x == "")
		throw "Empty label is not a proper context.";
	bool hit = false;
	list[BGFProduction] ps1 = [], ps2 = [], ps3 = [];
	if (innt(str x) := w)
		for (p <- ps)
			if (production (_,x,_) := p)
				{
					hit = true;
					ps2 += p;
				}
			elseif (hit)
				ps3 += p;
			else
				ps1 += p;
	elseif (inlabel(str x) := w)
		for (p <- ps)
			if (production (x,_,_) := p)
				{
					hit = true;
					ps2 += p;
				}
			elseif (hit)
				ps3 += p;
			else
				ps1 += p;
	else throw "Unknown context <w>.";
	if (isEmpty(ps2))
		throw "Context <w> not found.";
	return <ps1,ps2,ps3>;
}

// narrow-equivalence (the reverse, widen-equivalence, is hereby also implicitly defined)
bool narrowing(anything(),_) = true;
bool narrowing(star(e),plus(e)) = true;
bool narrowing(star(e),optional(e)) = true;
bool narrowing(star(e),e) = true;
bool narrowing(plus(e),e) = true;
bool narrowing(optional(e),e) = true;
bool narrowing(_,_) = false;

list[str] allNs(list[BGFProduction] ps)
{
	list[str] ns = [];
	// defined
	for (production(_,str s,_) <- ps)
		if (s notin ns) ns += s;
	// used
	for (/nonterminal(str s) := ps)
		if (s notin ns) ns += s;
	return ns;
}

