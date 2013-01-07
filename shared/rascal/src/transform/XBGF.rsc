@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::XBGF

import lib::Rascalware;
import syntax::BGF;
import syntax::XBGF;
import normal::BGF;
import diff::GDT;
import transform::library::Associativity;
import transform::library::Factoring;
import transform::library::Massage;
import transform::library::Util;
import transform::library::Core;
import transform::library::Width;
import transform::library::Yacc;
import export::BNF;
import transform::Results;

public XBGFResult transform(XBGFCommand x, BGFGrammar g)
{
	switch(x)
	{
		case abridge(BGFProduction p): return runAbridge(p,g);
		case abstractize(BGFProduction p): return runAbstractize(p,g);
		case addH(BGFProduction p): return runAddH(p,g);
		case addV(BGFProduction p): return runAddV(p,g);
		case anonymize(BGFProduction p): return runAnonymize(p,g);
		case appear(BGFProduction p): return runAppear(p,g);
		case chain(BGFProduction p): return runChain(p,g);
		case clone(str x, str y, XBGFScope w): return runClone(x,y,w,g);
		case concatT(list[str] xs, str y, XBGFScope w): return runConcatT(xs,y,w,g);
		case concretize(BGFProduction p): return runConcretize(p,g);
		case deanonymize(BGFProduction p): return runDeanonymize(p,g);
		case define(list[BGFProduction] ps): return runDefine(ps,g);
		case designate(BGFProduction p): return runDesignate(p,g);
		case detour(BGFProduction p): return runDetour(p,g);
		case deyaccify(str x): return runDeyaccify(x,g);
		case disappear(BGFProduction p): return runDisappear(p,g);
		case distribute(XBGFScope w): return runDistribute(w,g);
		case downgrade(BGFProduction p1,BGFProduction p2): return runDowngrade(p1,p2,g);
		case eliminate(str x): return runEliminate(x,g);
		case equate(str x, str y): return runEquate(x,y,g);
		case extract(BGFProduction p, XBGFScope w): return runExtract(p,w,g);
		case factor(BGFExpression e1, BGFExpression e2, XBGFScope w): return runFactor(e1,e2,w,g);
		case fold(str x, XBGFScope w): return runFold(x,w,g);
		case horizontal(XBGFScope w): return runHorizontal(w,g);
		case importG(list[BGFProduction] ps): return runImportG(ps,g);
		case inject(BGFProduction p): return runInject(p,g);
		case inline(str x): return runInline(x,g);
		case introduce(list[BGFProduction] ps): return runIntroduce(ps,g);
		case iterate(BGFProduction p): return runIterate(p,g);
		case lassoc(BGFProduction p): return runAssoc(p,g);
		case massage(BGFExpression e1, BGFExpression e2, XBGFScope w): return runMassage(e1,e2,w,g);
		case narrow(BGFExpression e1, BGFExpression e2, XBGFScope w): return runNarrow(e1,e2,w,g);
		case permute(BGFProduction p): return runPermute(p,g);
		case project(BGFProduction p): return runProject(p,g);
		case rassoc(BGFProduction p): return runAssoc(p,g);
		case redefine(list[BGFProduction] ps): return runRedefine(ps,g);
		case removeH(BGFProduction p): return runRemoveH(p,g);
		case removeV(BGFProduction p): return runRemoveV(p,g);
		case renameL(str x, str y): return runRenameL(x,y,g);
		case renameN(str x, str y): return runRenameN(x,y,g);
		case renameS(str x, str y, XBGFScope w): return runRenameS(x,y,w,g);
		case renameT(str x, str y): return runRenameT(x,y,g);
		case replace(BGFExpression e1, BGFExpression e2, XBGFScope w): return runReplace(e1,e2,w,g);
		case reroot(list[str] xs): return runReroot(xs,g);
		case splitN(str x, list[BGFProduction] ps, XBGFScope w): return runSplitN(x,ps,w,g);
		case splitT(str x, list[str] ys, XBGFScope w): return runSplitT(x,ys,w,g);
		case unchain(BGFProduction p): return runUnchain(p,g);
		case undefine(list[str] xs): return runUndefine(xs,g);
		case unfold(str x, XBGFScope w): return runUnfold(x,w,g);
		case unite(str x, str y): return runUnite(x,y,g);
		case unlabel(str x): return runUnlabel(x,g);
		case upgrade(BGFProduction p1, BGFProduction p2): return runUpgrade(p1,p2,g);
		case vertical(XBGFScope w): return runVertical(w,g);
		case widen(BGFExpression e1, BGFExpression e2, XBGFScope w): return runWiden(e1,e2,w,g);
		case yaccify(list[BGFProduction] ps): return runYaccify(ps,g);
		
		case atomic(list[XBGFCommand] steps): return transform(steps,g);
		case strip(str a): return runStrip(a,g);
		
		default: throw "Unknown XBGF command <x>";
	}
}

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

//case removeH(BGFProduction p): g1 = runRemoveH(p,g1);

XBGFResult runAbridge(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(_,x,nonterminal(x)) !:= p1)
		r = add(r,problemProd("Production cannot be abridged.",p1));
	if (!inProds(p1,g.prods))
		r = add(r,problemProd("Production not found.",p1));
	return <r,grammar(g.roots, g.prods - p1)>;
}

XBGFResult runAbstractize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,g.prods))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Abstractize only works with marked terminals, use project instead.";
	return add(r,runProject(p1,grammar(g.roots, g.prods)));
}

XBGFResult runAddH(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	p3 = demarkH(p1);
	if (!inProds(p3,g.prods))
		throw "Production rule <p3> not found.";
	return <r,grammar(g.roots, replaceP(g.prods,p3,p2))>;
}

XBGFResult runAddV(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (production(_,str x,_) := p1)
	{
		<ps1,ps2,ps3> = splitPbyW(g.prods,innt(x));
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (p1 in ps2)
			throw "Production rule <p1> is already present.";
		if (production(str l,_,_) := p1 && l != "")
			if (production(str l,_,_) <- ps)
				throw "Another production rule with label <l> is already present.";
		return <r,grammar(g.roots, ps1 + ps2 + p1 + ps3)>;
	}
}

XBGFResult runAnonymize(BGFProduction p1, BGFGrammar g)
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p2,g.prods))
		throw "Production rule <p1> not found.";
	return <r,grammar(g.roots, replaceP(g.prods,p2,p3))>;
}

XBGFResult runAppear(BGFProduction p1, grammar(roots, ps))
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return <r,grammar(roots, replaceP(ps,p2,unmark(p1)))>;
}

XBGFResult runAssoc(production(str l, str x, BGFExpression e1), BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (transform::library::Associativity::admit(e1,e2))
			return <r,grammar(g.roots,ps1 + production(l, x, e1) + ps3)>;
		else throw "<production(l,x,e1)> must admit associativity transformation.";
	else throw "Cannot find the right production rule to match <production(l,x,e1)> in <ps2>.";
}

XBGFResult runChain(BGFProduction p, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not introduce reflexive chain productions with chain, use detour instead.";
			if (n2 in allNs(ps)) throw "Nonterminal <n2> must be fresh.";
			list[BGFProduction] ps1,ps2,ps3;
			if (l != "") <ps1,ps2,ps3> = splitPbyW(ps,inlabel(l));
			else <ps1,ps2,ps3> = splitPbyW(ps,innt(n1));
			if ([production(l,n1,e)] := ps2) return <r,grammar(rs, ps1 + p + production("",n2,e) + ps3)>;
			else throw "Production rule <ps2> has unexpected form.";
		}
	else throw "Production <p> must be a chain production.";
}

XBGFResult runClone(str x, str y, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	// TODO
	return <r,g>;
}

XBGFResult runConcatT(list[str] xs, str y, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	// TODO
	return <r,g>;
}

XBGFResult runConcretize(BGFProduction p1, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Concretize only works with marked terminals, use inject instead.";
	return add(r,runInject(p1,grammar(rs, ps)));
}

XBGFResult runDeanonymize(BGFProduction p1, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p3,ps))
		throw "Production rule <p1> not found.";
	return <r,grammar(rs, replaceP(ps,p3,p2))>;
}

XBGFResult runDefine(list[BGFProduction] ps1, grammar(rs, ps2))
{
	XBGFOutcome r = ok();
	if ({str n} := definedNs(ps1))
	{
		if (n notin usedNs(ps2)) throw "Nonterminal <n> must not be fresh, use introduce instead.";
		return <r,grammar(rs, ps2 + ps1)>;
	}
	else throw "Multiple defined nonterminals found.";
}

XBGFResult runDesignate(production(str l,str n,BGFExpression e), grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (l == "") throw "Production <production(l,n,e)> must be labeled.";
	if (production("",n,e) notin ps) throw "Production rule defining <n> as <e> not found.";
	return <r,grammar(rs,replaceP(ps,production("",n,e),production(l,n,e)))>;
}

XBGFResult runDetour(BGFProduction p, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (production(_,x,nonterminal(x)) := p)
	{
		// xbgf1.pro only aksed for x to be used, not necessarily defined; we're more strict here
		if (x notin definedNs(ps)) throw "Nonterminal <x> must already be defined.";
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		return <r,grammar(rs, ps1 + ps2 + p + ps3)>;
	}
	else
		throw "Production <p> is not a reflexive chain production.";
}

XBGFResult runDeyaccify(str n, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (n notin definedNs(ps)) throw "Nonterminal <n> is not defined.";
	<ps1,ps2,ps3> = splitPbyW(ps,innt(n));
	if (len(ps2) == 1) throw "Nonterminal <n> must be defined vertically for deyaccification to work.";
	if (len(ps2) > 2) throw "No deyaccification patterns for <len(ps2)> production rules known.";
	return <r,grammar(rs, ps1 + transform::library::Yacc::deyaccify(toSet(ps2)) + ps3)>;
}

XBGFResult runDisappear(BGFProduction p1, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return <r,grammar(rs, replaceP(ps,p2,demark(p1)))>;
}

XBGFResult runDistribute(XBGFScope w, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	if (/choice(_) !:= ps2) throw "No choices found in the context <w>, nothing to distribute";
	return <r,grammar(rs,ps1 + normalise([transform::library::Factoring::makeDistributed(p) | p <- ps2]) + ps3)>;
}

XBGFResult runDowngrade(BGFProduction p1, BGFProduction p2, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			return <r,grammar(rs,replaceP(ps,unmark(p1),normalise(p3)))>;
		}
		else throw "<p1> and <p2> do not agree on nonterminal.";
	else throw "<p1> does not have a single nonterminal marked.";
}

XBGFResult runEliminate(str x, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	// TODO: can we eliminate root?
	if (x in rs) throw "Cannot eliminate root nonterminal <x>";
	if (x notin definedNs(ps)) throw "Nonterminal <x> must be defined.";
	<ps1,_,ps3> = splitPbyW(ps,innt(x));
	if (x in usedNs(ps1+ps3)) throw "Nonterminal <x> must not be used.";
	return <r,grammar(rs, ps1 + ps3)>;
}

XBGFResult runEquate(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (x == y)
		r = problemNT("Nonterminal is already equated with itself.",x);
	<ps1x,ps2x,ps3x> = splitPbyW(g.prods,innt(x));
	<_,ps2y,_> = splitPbyW(g.prods,innt(y));
	XBGFResult rep = runRenameN(x,y,grammar([],ps2x));
	r = add(r,rep.r);
	gxy = rep.g;
	gyy = grammar([],ps2y);
	if (!gdts(gxy,gyy))
		r = add(r,problemNT2("Definitions of nonterminals must be equal.",x,y));
	if (x in usedNs(ps1x + ps3x))
		return add(r,runReplace(nonterminal(x),nonterminal(y),globally(),grammar(g.roots - x,ps1x + ps3x)));
	else
		return <r,grammar(g.roots - x,ps1x + ps3x)>;
}

XBGFResult runExtract(production(str l, str x, BGFExpression rhs), XBGFScope w, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (x in definedNs(ps))
		r = problemNT("Nonterminal already defined.",x);
	// TODO hard to check if rhs occurs in the grammar; it was somehow done in xbgf1.pro 
	XBGFResult rep = runReplace(rhs,nonterminal(x),w,grammar(rs,ps));
	return <add(r,rep.r),grammar(rep.g.roots,rep.g.prods + production(l,x,rhs))>;
}

XBGFResult runFactor(BGFExpression e1, BGFExpression e2, XBGFScope w, g)
{
	XBGFOutcome r = ok();
	e3 = normalise(transform::library::Factoring::makeDistributed(e1));
	e4 = normalise(transform::library::Factoring::makeDistributed(e2));
	if (!eqE(e3, e4))
		r = problemExpr2("Expressions must be related by distribution.",e1,e2);
	return add(r,runReplace(e1,e2,w,g));
}

XBGFResult runFold(str x, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
		return add(r,runReplace(rhs,nonterminal(x),comboscope(notinnt(x),w),g));
	else 
		return <problemNT("Nonterminal must be defined horizontally prior to folding.",x),g>;
}

XBGFResult runHorizontal(XBGFScope w, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	// For xbgf1.pro, the context must be strictly vertical. Here we are more relaxed. 
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	list[BGFExpression] es4 = [];
	for (production(str l, str x, BGFExpression e) <- ps2)
		if (choice(L) := e)
			es4 += L;
		elseif (l == "")
			es4 += e;
		else
			es4 += selectable(l,e);
	if (innt(str x) := w)
		return <r,grammar(rs,ps1 + production("",x,choice(es4)) + ps3)>;
	else throw "Scope for horizontal must be a nonterminal.";
}

XBGFResult runImportG(list[BGFProduction] ps1, grammar(rs, ps2))
{
	XBGFOutcome r = ok();
	defs1 = definedNs(ps1);
	defs12 = defs1 & definedNs(ps2);
	du12 = defs1 & usedNs(ps2);
	if (!isEmpty(defs12)) throw "Import clashes with existing definitions <defs12>.";
	if (!isEmpty(du12)) throw "Import clashes with existing definitions <du12>.";
	return <r,grammar(rs, ps1 + ps2)>;
}

XBGFResult runInject(BGFProduction p1, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return <r,grammar(rs, replaceP(ps,p2,unmark(p1)))>;
}

XBGFResult runInline(str x, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<ps1,[production(_, x, BGFExpression rhs)],ps2> := splitPbyW(g.prods,innt(x)))
		return add(r,runReplace(nonterminal(x),rhs,globally(),grammar(g.roots,ps1+ps2)));
	else 
		return <problemNT("Nonterminal must be defined horizontally prior to inlining.",x),g>;
}

XBGFResult runIntroduce(list[BGFProduction] ps1, grammar(roots, ps2))
{
	XBGFOutcome r = ok();
	if ({str n} := definedNs(ps1))
	{
		if (n in usedNs(ps2)) throw "Nonterminal <n> must be fresh, use define instead.";
		if (n in definedNs(ps2)) throw "Definition for <n> clashes with existing definition.";
		return <r,grammar(roots, ps2 + ps1)>;
	}
	else throw "Multiple defined nonterminals found.";
}

XBGFResult runIterate(production(str l, str x, BGFExpression e1), grammar(rs, ps))
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(ps,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (transform::library::Associativity::admit(e2,e1))
			return <r,grammar(rs,ps1 + production(l, x, e1) + ps3)>;
		else throw "<production(l,x,e1)> must admit associativity transformation.";
	else throw "Cannot find the right production rule to match <production(l,x,e1)> in <ps2>.";
}

XBGFResult runMassage(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (transform::library::Massage::massage_eq({e1,e2}))
		return add(r,runReplace(e1,e2,w,g));
	else
		return <problemExpr2("Expressions are not massage-equivalent.",e1,e2),g>;
}

XBGFResult runNarrow(BGFExpression e1, BGFExpression e2, XBGFScope w, g)
{
	XBGFOutcome r = ok();
	if (!transform::library::Width::narrowing(e1,e2))
		return <problemExpr2("Expressions are not in narrowing relation.",e1,e2),g>;
	else
		return add(r,runReplace(e1,e2,w,g)); 
}

XBGFResult runPermute(BGFProduction p, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (production(str l, str n, sequence(L1)) := p)
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(n));
		if ([production(_, n, sequence(L2))] := ps2)
		{
			if (toSet(L1) == toSet(L2))
				return <r,grammar(rs, ps1 + p + ps3)>;
			else
				throw "Phrases <L1> and <L2> must be permutations of each other.";
		}
		else throw "Permutation requires a single sequence instead of <ps2>.";
	}
	else throw "Permutation parameter requires a sequence instead of <p>.";
	return <r,g>;
}

XBGFResult runProject(BGFProduction p1, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return <r,grammar(rs, replaceP(ps,p2,demark(p1)))>;
}

XBGFResult runRedefine(list[BGFProduction] ps1, grammar(list[str] rs, ps2))
{
	XBGFOutcome r = ok();
	// inlined superposition of undefine and define, with two exceptional details:
	// (1) if ps1.n is a root, undefine would have stripped it from this status
	// (2) redefine preserves original order of production rules
	if ({str x} := definedNs(ps1))
	{
		if (x notin definedNs(ps2)) throw "Nonterminal <x> must be defined.";
		if (x notin usedNs(ps2)) throw "Nonterminal <x> must be used.";
		<ps3,_,ps4> = splitPbyW(ps2,innt(x));
		return <r,grammar(rs,ps3 + ps1 + ps4)>; 
	}
}

XBGFResult runRemoveH(BGFProduction p1, grammar(roots, ps))
{
	XBGFOutcome r = ok();
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return <r,grammar(roots, replaceP(ps,p2,demarkH(p1)))>;
}

XBGFResult runRemoveV(BGFProduction p1, grammar(roots, ps))
{
	XBGFOutcome r = ok();
	if (production(_,str x,_) := p1)
	{
		<_,ps2,_> = splitPbyW(ps,innt(x));
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (!inProds(p1,ps2))
			throw "Production rule <p1> is not in the grammar.";
		if ([p1] == ps2)
			throw "Cannot remove the last production rule of <x> with removeV, use undefine or eliminate.";
		return <r,grammar(roots, ps - p1)>;
	}
}

XBGFResult runRenameL(str x, str y, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (x == "") throw "Source label must not be empty for renaming, use designate.";
	if (y == "") throw "Target label must not be empty for renaming, use unlabel.";
	if (len([p | p <- ps, production(x, _, _) := p]) != 1)
		throw "Source name <x> for renaming must be fresh and unique.";
	if (len([p | p <- ps, production(y, _, _) := p]) != 0)
		throw "Target name <y> for renaming must be fresh.";
	<ps1,ps2,ps3> = splitPbyW(ps,inlabel(x));
	if ([production(x, str n, BGFExpression e)] := ps2)
		return <r,grammar(rs, ps1 + production(y, n, e) + ps3)>;
	else
		throw "Label <x> is not found or not unique"; // the latter should never happen
}

XBGFResult runRenameN(str x, str y, BGFGrammar g)
{
	XBGFOutcome r = ok();
	ns = allNs(g.prods);
	if (x notin ns)
		throw "Source name <x> for renaming must not be fresh.";
	if (y in ns)
		throw "Target name <y> for renaming must be fresh.";
	return
		<r,transform::library::Core::performRenameN(x,y,g)>;
}

XBGFResult runRenameS(str x, str y, XBGFScope w, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	if (/selectable(x,_) !:= ps2) throw "Source name <x> for renaming must not be fresh.";
	if (/selectable(y,_) := ps2) throw "Target name <y> for renaming must be fresh.";
	ps4 = visit(ps2){case selectable(x,BGFExpression e) => selectable(y,e)}
	return <r,grammar(rs, ps1 + ps4 + ps3)>;
}

XBGFResult runRenameT(str x, str y, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	ts = allTs(ps);
	if (x notin ts)
		r = add(r, problemNT("Source name for renaming must not be fresh.",x));
	if (y in ts)
		r = add(r, problemNT("Target name for renaming must be fresh.",y));
	return add(r,runReplace(terminal(x),terminal(y),globally(),grammar(rs,ps)));
}

XBGFResult runReplace(BGFExpression e1, BGFExpression e2, XBGFScope w, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	list[BGFProduction] ps1,ps2,ps3,ps4;
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	ps4 = transform::library::Core::performReplace(e1,e2,ps2);
	if (ps2 == ps4)
		{
			ps4 = transform::library::Core::performReplace(normalise(e1),normalise(e2),ps2); // TODO check if needed
			if (ps2 == ps4)
				r = add(r,problemExpr2("Vacuous replace",e1,e2));
		}
	return <r,grammar(rs, ps1 + normalise(ps4) + ps3)>;
}

XBGFResult runReroot(list[str] xs, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (toSet(xs) == toSet(rs)) throw "Vacuous reroot of <xs>.";
	// xbgf1.pro only asked for it to be a subset of allNs, not definedNs; we're more strict here
	if (toSet(xs) <= definedNs(ps)) return <r,grammar(xs,ps)>;
	else throw "Not all nonterminals in <xs> are defined.";
}

XBGFResult runSplitN(str x, list[BGFProduction] ps0, XBGFScope w, grammar(rs1, ps1))
{
	XBGFOutcome r = ok();
	if ({str y} := definedNs(ps0))
	{
		if (x notin definedNs(ps1)) throw "Source name <x> for splitting must not be undefined.";
		if (y in allNs(ps1)) throw "Target name <y> for splitting must be fresh.";
		<ps2,ps3,ps4> = splitPbyW(ps1,innt(x));
		list[BGFProduction] ps5 = [production(l,x,e) | p <- ps0, production(str l,y,BGFExpression e) := p];
		if (x in rs1) rs2 = rs1 + y; else rs2 = rs1;
		g = grammar(rs2,ps2 + (ps3 - ps5) + ps0 + ps4);
		if (nowhere() := w)
			return <r,g>;
		else
			return add(r,runReplace(nonterminal(x),nonterminal(y),w,g));
	}
	else throw "Splitting into more than two nonterminals not supported.";
}

XBGFResult runSplitT(str x, list[str] ys, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(g.prods, w);
	BGFGrammar g2 = runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2));
	XBGFResult repl = runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2));
	r = add(r,repl.r);
	if (grammar(_, ps4) := repl.g)
		return <r,grammar(g.roots,ps1 + normalise(ps2) + ps3)>;
}

XBGFResult runUnchain(BGFProduction p, grammar(roots, ps))
{
	XBGFOutcome r = ok();
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not remove reflexive chain productions with chain, use abridge instead.";
			if (n2 in roots) throw "Nonterminal <n2> must not be root.";
			if (!inProds(p,ps)) throw "<p> not found.";
			//if (n2 in allNs(ps)) throw "Nonterminal <n2> must be fresh.";
			list[BGFProduction] ps1,ps2,ps3;
			<ps1,ps2,ps3> = splitPbyW(ps - p,innt(n2));
			if (len(ps2) != 1) throw "Nonterminal <n2> must occur exactly once.";
			if (l == "") l = n2;
			if ([production(_,n2,e)] := ps2) return <r,grammar(roots, ps1 + production(l,n1,e) + ps3)>;
			else throw "Production rule <ps2> has unexpected form.";
		}
	else throw "Production <p> must be a chain production.";
}

//TODO: undefine only one nonterminal per transformation
XBGFResult runUndefine(list[str] xs, BGFGrammar g)
{
	XBGFOutcome r = ok();
	list[BGFProduction] myps = g.prods;
	list[str] rs = g.roots;
	for (str x <- xs)
	{
		if (x notin definedNs(g.prods))
			r = add(r,"Nonterminal must be defined.",x);
		if (x notin usedNs(g.prods))
			r = add(r,"Nonterminal must be used.",x);
		//throw "Cannot undefine root nonterminal <x>."; // check was not in xbgf1.pro
		rs -= x;
		<ps1,_,ps3> = splitPbyW(myps,innt(x));
		myps = ps1 + ps3;
	}
	return <r,grammar(g.roots,myps)>;
}

XBGFResult runUnfold(str x, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(g.prods,innt(x)))
		return add(r,runReplace(nonterminal(x),rhs,comboscope(notinnt(x),w),g));
	else
		return <problemNT("Nonterminal must be defined horizontally prior to unfolding.",x),g>;
}

XBGFResult runUnite(str x, str y, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (x == y) throw "Nonterminal <x> is already united with itself.";
	used = allNs(ps);
	if (x notin used || y notin used) throw "Both nonterminals <x> and <y> must not be fresh.";
	<ps1x,ps2x,ps3x> = splitPbyW(ps,innt(x));
	list[BGFProduction] ps4x = ps1x + [production(l,y,e) | p <- ps2x, production(str l,x,BGFExpression e) := p] + ps3x;
	if (x in usedNs(ps4x))
		return <r,runReplace(nonterminal(x),nonterminal(y),globally(),grammar(rs,ps4x))>;
	else
		return <r,grammar(rs,ps4x)>;
}

XBGFResult runUnlabel(str x, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	if (x == "") throw "Please specify which label to unlabel.";
	<ps1,ps2,ps3> = splitPbyW(ps,inlabel(x));
	if ([production(str l, str x, BGFExpression e)] := ps2)
		return <r,grammar(rs, ps1 + production("", x, e) + ps3)>;
	else
		throw "Label <x> is not found or not unique"; // the latter should never happen
}

XBGFResult runUpgrade(BGFProduction p1, BGFProduction p2, grammar(rs, ps))
{
	XBGFOutcome r = ok();
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			p3 = normalise(p3);
			return <r,grammar(rs,replaceP(ps,p3,unmark(p1)))>;
		}
		else throw "<p1> and <p2> do not agree on nonterminal.";
	else throw "<p1> does not have a single nonterminal marked.";
}

XBGFResult runVertical(XBGFScope w, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	ps4 = [];
	for (production(str l, str x, BGFExpression e) <- ps2)
		if (choice(L) := e)
			for (se <- L)
				if (selectable(str s, BGFExpression e2) := se)
					if (/production(s,_,_) := ps)
						throw "Outermost selector <s> clashes with an existing label.";
					elseif (/production(s,_,_) := ps4)
						throw "Outermost selectors ambiguous at <s>.";
					else
						ps4 += production(s,x,e2);
				else
					ps4 += production("",x,se);
		else ps4 += production(l,x,e);
	return <r,grammar(rs,ps1 + ps4 + ps3)>;
}

XBGFResult runWiden(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	XBGFOutcome r = ok();
	if (!transform::library::Width::narrowing(e2,e1))
		r = problemExpr2("Expressions are not in widening relation",e2,e1);
	return add(r,runReplace(e1,e2,w,g)); 
}

XBGFResult runYaccify(list[BGFProduction] ps1, grammar(rs,ps2))
{
	XBGFOutcome r = ok();
	if ({str x} := definedNs(ps1))
	{
		<ps3,ps4,ps5> = splitPbyW(ps2,innt(x));
		if ([dyp1] := ps4 && [yp1,yp2] := ps1 && transform::library::Yacc::yaccification(dyp1,{yp1,yp2}))
			return <r,grammar(rs,ps3 + ps1 + ps5)>;
		else
			throw "<ps1> are not suitable as yaccification of <ps4>.";
	}
	else throw "Production rules must define just one nonterminal.";
}

XBGFResult runStrip(str a, grammar(rs,ps))
{
	XBGFOutcome r = ok();
	// TODO: semi-deprecated
	list[BGFProduction] ps2;
	if (a=="allLabels")
		ps2 = visit(ps){case production(_,str x,BGFExpression e) => production("",x,e)}
	elseif (a=="allSelectors")
		ps2 = visit(ps){case selectable(_,BGFExpression e) => e}
	elseif (a=="allTerminals")
		{ // deprecated, please use a mutation that generates abstractize commands
			ps2 = visit(ps){case terminal(_) => epsilon()};
			ps2 = normalise(ps2);
		}
	else
		throw "Unknown strip <a> parameter.";
	return <r,grammar(rs,ps2)>;
}
