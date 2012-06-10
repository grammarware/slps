@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::XBGF

import IO;
import List;
import Set; // isEmpty
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
			case clone(str x, str y, XBGFScope w): g1 = runClone(x,y,w,g1);
			case concatT(list[str] xs, str y, XBGFScope w): g1 = runConcatT(xs,y,w,g1);
			case concretize(BGFProduction p): g1 = runConcretize(p,g1);
			case deanonymize(BGFProduction p): g1 = runDeanonymize(p,g1);
			case define(list[BGFProduction] ps): g1 = runDefine(ps,g1);
			case designate(BGFProduction p): g1 = runDesignate(p,g1);
			case detour(BGFProduction p): g1 = runDetour(p,g1);
			case deyaccify(str x): g1 = runDeyaccify(x,g1);
			case disappear(BGFProduction p): g1 = runDisappear(p,g1);
			case distribute(XBGFScope w): g1 = runDistribute(w,g1);
			case downgrade(BGFProduction p1,BGFProduction p2): g1 = runDowngrade(p1,p2,g1);
			case eliminate(str x): g1 = runEliminate(x,g1);
			case equate(str x, str y): g1 = runEquate(x,y,g1);
			case extract(BGFProduction p, XBGFScope w): g1 = runExtract(p,w,g1);
			case factor(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = runFactor(e1,e2,w,g1);
			case fold(str x, XBGFScope w): g1 = runFold(x,w,g1);
			case horizontal(XBGFScope w): g1 = runHorizontal(w,g1);
			case importG(list[BGFProduction] ps): g1 = runImportG(ps,g1);
			case inject(BGFProduction p): g1 = runInject(p,g1);
			case inline(str x): g1 = runInline(x,g1);
			case introduce(list[BGFProduction] ps): g1 = runIntroduce(ps,g1);
			case iterate(BGFProduction p): g1 = runIterate(p,g1);
			case lassoc(BGFProduction p): g1 = runAssoc(p,g1);
			case massage(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = runMassage(e1,e2,w,g1);
			case narrow(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = runNarrow(e1,e2,w,g1);
			case permute(BGFProduction p): g1 = runPermute(p,g1);
			case project(BGFProduction p): g1 = runProject(p,g1);
			case rassoc(BGFProduction p): g1 = runAssoc(p,g1);
			case redefine(list[BGFProduction] ps): g1 = runRedefine(ps,g1);
			case removeH(BGFProduction p): g1 = runRemoveH(p,g1);
			case removeV(BGFProduction p): g1 = runRemoveV(p,g1);
			case renameL(str x, str y): g1 = runRenameL(x,y,g1);
			case renameN(str x, str y): g1 = runRenameN(x,y,g1);
			case renameS(str x, str y, XBGFScope w): g1 = runRenameS(x,y,w,g1);
			case renameT(str x, str y): g1 = runRenameT(x,y,g1);
			case replace(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = runReplace(e1,e2,w,g1);
			case reroot(list[str] xs): g1 = runReroot(xs,g1);
			case splitN(str x, list[BGFProduction] ps, XBGFScope w): g1 = runSplitN(x,ps,w,g1);
			case splitT(str x, list[str] ys, XBGFScope w): g1 = runSplitT(x,ys,w,g1);
			case unchain(BGFProduction p): g1 = runUnchain(p,g1);
			case undefine(list[str] xs): g1 = runUndefine(xs,g1);
			case unfold(str x, XBGFScope w): g1 = runUnfold(x,w,g1);
			case unite(str x, str y): g1 = runUnite(x,y,g1);
			case unlabel(str x): g1 = runUnlabel(x,g1);
			case upgrade(BGFProduction p1, BGFProduction p2): g1 = runUpgrade(p1,p2,g1);
			case vertical(XBGFScope w): g1 = runVertical(w,g1);
			case widen(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = runWiden(e1,e2,w,g1);
			case yaccify(list[BGFProduction] ps): g1 = runYaccify(ps,g1);
			case atomic(list[XBGFCommand] steps): g1 = runAtomic(steps,g1);
			case strip(str a): g1 = runStrip(a,g1);
			default: throw "Unknown XBGF command <step>";
		}
	}
	return normalise(g1);
}

BGFGrammar runAbridge(BGFProduction prod, grammar(rs, ps))
{
	if (production(_,x,nonterminal(x)) !:= prod)
		throw "Production <prod> cannot be abridged.";
	if (!inProds(prod,ps))
		throw "Production <prod> not found.";
	return grammar(rs, ps - prod);
}

BGFGrammar runAbstractize(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Abstractize only works with marked terminals, use project instead.";
	return runProject(p1,grammar(roots, ps));
}

BGFGrammar runAddH(BGFProduction p1, grammar(rs, ps))
{
	p2 = unmark(p1);
	p3 = demark(p1);
	if (!inProds(p3,ps))
		throw "Production rule <p3> not found.";
	return grammar(rs, replaceP(ps,p3,p2));
}

BGFGrammar runAddV(BGFProduction p1, grammar(rs, ps))
{
	if (production(_,str x,_) := p1)
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (p1 in ps2)
			throw "Production rule <p1> is already present.";
		if (production(str l,_,_) := p1 && l != "")
			if (production(str l,_,_) <- ps)
				throw "Another production rule with label <l> is already present.";
		return grammar(rs, ps1 + ps2 + p1 + ps3);
	}
}

bool inProds(BGFProduction p, []) = false;
bool inProds(BGFProduction p, list[BGFProduction] ps)
{
	if (eqP(normalise(p),normalise(ps[0]))) return true;
	else return inProds(p,slice(ps,1,size(ps)-1));
}

BGFGrammar runAnonymize(BGFProduction p1, grammar(rs, ps))
{
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p1> not found.";
	return grammar(rs, replaceP(ps,p2,p3));
}

BGFGrammar runAppear(BGFProduction p1, grammar(roots, ps))
{
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return grammar(roots, replaceP(ps,p2,unmark(p1)));
}

BGFGrammar runAssoc(production(str l, str x, BGFExpression e1), grammar(rs, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (transform::library::Associativity::admit(e1,e2))
			return grammar(rs,ps1 + production(l, x, e1) + ps3);
		else throw "<production(l,x,e1)> must admit associativity transformation.";
	else throw "Cannot find the right production rule to match <production(l,x,e1)> in <ps2>.";
}

BGFGrammar runChain(BGFProduction p, grammar(rs, ps))
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not introduce reflexive chain productions with chain, use detour instead.";
			if (n2 in allNs(ps)) throw "Nonterminal <n2> must be fresh.";
			list[BGFProduction] ps1,ps2,ps3;
			if (l != "") <ps1,ps2,ps3> = splitPbyW(ps,inlabel(l));
			else <ps1,ps2,ps3> = splitPbyW(ps,innt(n1));
			if ([production(l,n1,e)] := ps2) return grammar(rs, ps1 + p + production("",n2,e) + ps3);
			else throw "Production rule <ps2> has unexpected form.";
		}
	else throw "Production <p> must be a chain production.";
}

BGFGrammar runClone(str x, str y, XBGFScope w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runConcatT(list[str] xs, str y, XBGFScope w, BGFGrammar g)
{
	// TODO
	return g;
}

BGFGrammar runConcretize(BGFProduction p1, grammar(rs, ps))
{
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (terminal(_) !:= e)
			throw "Concretize only works with marked terminals, use inject instead.";
	return runInject(p1,grammar(rs, ps));
}

BGFGrammar runDeanonymize(BGFProduction p1, grammar(rs, ps))
{
	p2 = unmark(p1);
	p3 = demarkS(p1);
	if (!inProds(p3,ps))
		throw "Production rule <p1> not found.";
	return grammar(rs, replaceP(ps,p3,p2));
}

BGFGrammar runDefine(list[BGFProduction] ps1, grammar(rs, ps2))
{
	if ({str n} := definedNs(ps1))
	{
		if (n notin usedNs(ps2)) throw "Nonterminal <n> must not be fresh, use introduce instead.";
		return grammar(rs, ps2 + ps1);
	}
	else throw "Multiple defined nonterminals found.";
}

BGFGrammar runDesignate(production(str l,str n,BGFExpression e), grammar(rs,ps))
{
	if (l == "") throw "Production <production(l,n,e)> must be labeled.";
	if (production("",n,e) notin ps) throw "Production rule defining <n> as <e> not found.";
	return grammar(rs,replaceP(ps,production("",n,e),production(l,n,e)));
}

BGFGrammar runDetour(BGFProduction p, grammar(rs, ps))
{
	if (production(_,x,nonterminal(x)) := p)
	{
		// xbgf1.pro only aksed for x to be used, not necessarily defined; we're more strict here
		if (x notin definedNs(ps)) throw "Nonterminal <x> must already be defined.";
		<ps1,ps2,ps3> = splitPbyW(ps,innt(x));
		return grammar(rs, ps1 + ps2 + p + ps3);
	}
	else
		throw "Production <p> is not a reflexive chain production.";
}

BGFGrammar runDeyaccify(str n, grammar(rs,ps))
{
	if (n notin definedNs(ps)) throw "Nonterminal <n> is not defined.";
	<ps1,ps2,ps3> = splitPbyW(ps,innt(n));
	if (size(ps2) == 1) throw "Nonterminal <n> must be defined vertically for deyaccification to work.";
	if (size(ps2) > 2) throw "No deyaccification patterns for <size(ps2)> production rules known.";
	return grammar(rs, ps1 + transform::library::Yacc::deyaccify(toSet(ps2)) + ps3);
}

BGFGrammar runDisappear(BGFProduction p1, grammar(rs, ps))
{
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	for (/marked(e) := p1)
		if (optional(_) !:= e && star(_) !:= e)
			throw "<p1> does not have an optional part marked.";
	return grammar(rs, replaceP(ps,p2,demark(p1)));
}

BGFGrammar runDistribute(XBGFScope w, grammar(rs, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	if (/choice(_) !:= ps2) throw "No choices found in the context <w>, nothing to distribute";
	return grammar(rs,ps1 + normalise([transform::library::Factoring::makeDistributed(p) | p <- ps2]) + ps3);
}

BGFGrammar runDowngrade(BGFProduction p1, BGFProduction p2, grammar(rs, ps))
{
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			return grammar(rs,replaceP(ps,unmark(p1),normalise(p3)));
		}
		else throw "<p1> and <p2> do not agree on nonterminal.";
	else throw "<p1> does not have a single nonterminal marked.";
}

BGFGrammar runEliminate(str x, grammar(rs, ps))
{
	// TODO: can we eliminate root?
	if (x in rs) throw "Cannot eliminate root nonterminal <x>";
	if (x notin definedNs(ps)) throw "Nonterminal <x> must be defined.";
	<ps1,_,ps3> = splitPbyW(ps,innt(x));
	if (x in usedNs(ps1+ps3)) throw "Nonterminal <x> must not be used.";
	return grammar(rs, ps1 + ps3);
}

BGFGrammar runEquate(str x, str y, grammar(rs, ps))
{
	if (x == y) throw "Nonterminal <x> is already equated with itself."; 
	<ps1x,ps2x,ps3x> = splitPbyW(ps,innt(x));
	<_,ps2y,_> = splitPbyW(ps,innt(y));
	gxy = runRenameN(x,y,grammar([],ps2x));
	gyy = grammar([],ps2y);
	if (!gdts(gxy,gyy)) throw "Definitions of nonterminals <x> and <y> must be equal.";
	if (x in usedNs(ps1x + ps3x))
		return runReplace(nonterminal(x),nonterminal(y),globally(),grammar(rs - x,ps1x + ps3x));
	else
		return grammar(rs - x,ps1x + ps3x);
}

BGFGrammar runExtract(production(str l, str x, BGFExpression rhs), XBGFScope w, grammar(rs,ps))
{
	if (x in definedNs(ps)) throw "Nonterminal <x> already defined.";
	// TODO hard to check if rhs occurs in the grammar; it was somehow done in xbgf1.pro 
	grammar(rs1,ps1) = runReplace(rhs,nonterminal(x),w,grammar(rs,ps));
	return grammar(rs1,ps1 + production(l,x,rhs));
}

BGFGrammar runFactor(BGFExpression e1, BGFExpression e2, XBGFScope w, g)
{
	e3 = normalise(transform::library::Factoring::makeDistributed(e1));
	e4 = normalise(transform::library::Factoring::makeDistributed(e2));
	if (!eqE(e3, e4))
		throw "Expressions <e1> and <e2> must be related by distribution.";
	return runReplace(e1,e2,w,g);
}

BGFGrammar runFold(str x, XBGFScope w, grammar(rs,ps))
{
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(ps,innt(x)))
		return runReplace(rhs,nonterminal(x),comboscope(notinnt(x),w),grammar(rs,ps));
	else throw "Nonterminal <x> must be defined horizontally prior to folding.";
}

BGFGrammar runHorizontal(XBGFScope w, grammar(rs,ps))
{
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
		return grammar(rs,ps1 + production("",x,choice(es4)) + ps3);
	else throw "Scope for horizontal must be a nonterminal.";
}

BGFGrammar runImportG(list[BGFProduction] ps1, grammar(rs, ps2))
{
	defs1 = definedNs(ps1);
	defs12 = defs1 & definedNs(ps2);
	du12 = defs1 & usedNs(ps2);
	if (!isEmpty(defs12)) throw "Import clashes with existing definitions <defs12>.";
	if (!isEmpty(du12)) throw "Import clashes with existing definitions <du12>.";
	return grammar(rs, ps1 + ps2);
}

BGFGrammar runInject(BGFProduction p1, grammar(rs, ps))
{
	p2 = demark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return grammar(rs, replaceP(ps,p2,unmark(p1)));
}

BGFGrammar runInline(str x, grammar(rs,ps))
{
	if (<ps1,[production(_, x, BGFExpression rhs)],ps2> := splitPbyW(ps,innt(x)))
		return runReplace(nonterminal(x),rhs,globally(),grammar(rs,ps1+ps2));
	else throw "Nonterminal <x> must be defined horizontally prior to inlining.";
}

BGFGrammar runIntroduce(list[BGFProduction] ps1, grammar(roots, ps2))
{
	if ({str n} := definedNs(ps1))
	{
		if (n in usedNs(ps2)) throw "Nonterminal <n> must be fresh, use define instead.";
		if (n in definedNs(ps2)) throw "Definition for <n> clashes with existing definition.";
		return grammar(roots, ps2 + ps1);
	}
	else throw "Multiple defined nonterminals found.";
}

BGFGrammar runIterate(production(str l, str x, BGFExpression e1), grammar(rs, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps,comboscope(inlabel(l),innt(x)));
	if ([production(l, x, BGFExpression e2)] := ps2)
		if (transform::library::Associativity::admit(e2,e1))
			return grammar(rs,ps1 + production(l, x, e1) + ps3);
		else throw "<production(l,x,e1)> must admit associativity transformation.";
	else throw "Cannot find the right production rule to match <production(l,x,e1)> in <ps2>.";
}

BGFGrammar runMassage(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	if (transform::library::Massage::massage_eq({e1,e2}))
		return runReplace(e1,e2,w,g);
	else
		throw "<e1> and <e2> are not massage-equivalent.";
}

BGFGrammar runNarrow(BGFExpression e1, BGFExpression e2, XBGFScope w, g)
{
	if (!transform::library::Width::narrowing(e1,e2))
		throw "<e1> and <e2> are not in narrowing relation.";
	return runReplace(e1,e2,w,g); 
}

BGFGrammar runPermute(BGFProduction p, grammar(rs, ps))
{
	if (production(str l, str n, sequence(L1)) := p)
	{
		<ps1,ps2,ps3> = splitPbyW(ps,innt(n));
		if ([production(_, n, sequence(L2))] := ps2)
		{
			if (toSet(L1) == toSet(L2))
				return grammar(rs, ps1 + p + ps3);
			else
				throw "Phrases <L1> and <L2> must be permutations of each other.";
		}
		else throw "Permutation requires a single sequence instead of <ps2>.";
	}
	else throw "Permutation parameter requires a sequence instead of <p>.";
	return g;
}

BGFGrammar runProject(BGFProduction p1, grammar(rs, ps))
{
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return grammar(rs, replaceP(ps,p2,demark(p1)));
}

BGFGrammar runRedefine(list[BGFProduction] ps1, grammar(list[str] rs, ps2))
{
	// inlined superposition of undefine and define, with two exceptional details:
	// (1) if ps1.n is a root, undefine would have stripped it from this status
	// (2) redefine preserves original order of production rules
	if ({str x} := definedNs(ps1))
	{
		if (x notin definedNs(ps2)) throw "Nonterminal <x> must be defined.";
		if (x notin usedNs(ps2)) throw "Nonterminal <x> must be used.";
		<ps3,_,ps4> = splitPbyW(ps2,innt(x));
		return grammar(rs,ps3 + ps1 + ps4); 
	}
}

BGFGrammar runRemoveH(BGFProduction p1, grammar(roots, ps))
{
	p2 = unmark(p1);
	if (!inProds(p2,ps))
		throw "Production rule <p2> not found.";
	return grammar(roots, replaceP(ps,p2,demark(p1)));
}

BGFGrammar runRemoveV(BGFProduction p1, grammar(roots, ps))
{
	if (production(_,str x,_) := p1)
	{
		<_,ps2,_> = splitPbyW(ps,innt(x));
		if (isEmpty(ps2))
			throw "Nonterminal <x> must be defined.";
		if (!inProds(p1,ps2))
			throw "Production rule <p1> is not in the grammar.";
		if ([p1] == ps2)
			throw "Cannot remove the last production rule of <x> with removeV, use undefine or eliminate.";
		return grammar(roots, ps - p1);
	}
}

BGFGrammar runRenameL(str x, str y, grammar(rs, ps))
{
	if (x == "") throw "Source label must not be empty for renaming, use designate.";
	if (y == "") throw "Target label must not be empty for renaming, use unlabel.";
	if (size([p | p <- ps, production(x, _, _) := p]) != 1)
		throw "Source name <x> for renaming must be fresh and unique.";
	if (size([p | p <- ps, production(y, _, _) := p]) != 0)
		throw "Target name <y> for renaming must be fresh.";
	<ps1,ps2,ps3> = splitPbyW(ps,inlabel(x));
	if ([production(x, str n, BGFExpression e)] := ps2)
		return grammar(rs, ps1 + production(y, n, e) + ps3);
	else
		throw "Label <x> is not found or not unique"; // the latter should never happen
}

BGFGrammar runRenameN(str x, str y, grammar(rs, ps))
{
	ns = allNs(ps);
	if (x notin ns) throw "Source name <x> for renaming must not be fresh.";
	if (y in ns) throw "Target name <y> for renaming must be fresh.";
	return transform::library::Core::performRenameN(x,y,grammar(rs,ps));
}

BGFGrammar runRenameS(str x, str y, XBGFScope w, grammar(rs, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	if (/selectable(x,_) !:= ps2) throw "Source name <x> for renaming must not be fresh.";
	if (/selectable(y,_) := ps2) throw "Target name <y> for renaming must be fresh.";
	ps4 = visit(ps2){case selectable(x,BGFExpression e) => selectable(y,e)}
	return grammar(rs, ps1 + ps4 + ps3);
}

BGFGrammar runRenameT(str x, str y, grammar(rs, ps))
{
	ts = allTs(ps);
	if (x notin ts) throw "Source name <x> for renaming must not be fresh.";
	if (y in ts) throw "Target name <x> for renaming must be fresh.";
	return runReplace(terminal(x),terminal(y),globally(),grammar(rs,ps));
}

BGFGrammar runReplace(BGFExpression e1, BGFExpression e2, XBGFScope w, grammar(rs, ps))
{
	list[BGFProduction] ps1,ps2,ps3,ps4;
	<ps1,ps2,ps3> = splitPbyW(ps,w);
	ps4 = transform::library::Core::performReplace(e1,e2,ps2);
	if (ps2 == ps4)
		{
			ps4 = transform::library::Core::performReplace(normalise(e1),normalise(e2),ps2); // TODO check if needed
			if (ps2 == ps4)
				throw "Vacuous replace of <e1> by <e2> in context <w>.";
		}
	return grammar(rs, ps1 + normalise(ps4) + ps3);
}

BGFGrammar runReroot(list[str] xs, grammar(rs, ps))
{
	if (toSet(xs) == toSet(rs)) throw "Vacuous reroot of <xs>.";
	// xbgf1.pro only asked for it to be a subset of allNs, not definedNs; we're more strict here
	if (toSet(xs) <= definedNs(ps)) return grammar(xs,ps);
	else throw "Not all nonterminals in <xs> are defined.";
}

BGFGrammar runSplitN(str x, list[BGFProduction] ps0, XBGFScope w, grammar(rs1, ps1))
{
	if ({str y} := definedNs(ps0))
	{
		if (x notin definedNs(ps1)) throw "Source name <x> for splitting must not be undefined.";
		if (y in allNs(ps1)) throw "Target name <y> for splitting must be fresh.";
		<ps2,ps3,ps4> = splitPbyW(ps1,innt(x));
		list[BGFProduction] ps5 = [production(l,x,e) | p <- ps0, production(str l,y,BGFExpression e) := p];
		if (x in rs1) rs2 = rs1 + y; else rs2 = rs1;
		g = grammar(rs2,ps2 + (ps3 - ps5) + ps0 + ps4);
		if (nowhere() := w)
			return g;
		else
			return runReplace(nonterminal(x),nonterminal(y),w,g);
	}
	else throw "Splitting into more than two nonterminals not supported.";
}

BGFGrammar runSplitT(str x, list[str] ys, XBGFScope w, grammar(rs, ps))
{
	<ps1,ps2,ps3> = splitPbyW(ps, w);
	BGFGrammar g2 = runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2));
	if (grammar(_, ps4) := runReplace(terminal(x),sequence([terminal(y) | y <- ys]),grammar([],ps2)))
		return grammar(rs,ps1 + normalise(ps2) + ps3);
}

BGFGrammar runUnchain(BGFProduction p, grammar(roots, ps))
{
	if (production(str l,str n1,nonterminal(str n2)) := p)
		{
			if (n1 == n2) throw "Do not remove reflexive chain productions with chain, use abridge instead.";
			if (n2 in roots) throw "Nonterminal <n2> must not be root.";
			if (!inProds(p,ps)) throw "<p> not found.";
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

//TODO: undefine only one nonterminal per transformation
BGFGrammar runUndefine(list[str] xs, grammar(roots, ps))
{
	list[BGFProduction] myps = ps;
	list[str] rs = roots;
	for (str x <- xs)
	{
		if (x notin definedNs(ps)) throw "Nonterminal <x> must be defined.";
		if (x notin usedNs(ps)) throw "Nonterminal <x> must be used.";
		//throw "Cannot undefine root nonterminal <x>."; // check was not in xbgf1.pro
		rs -= x;
		<ps1,_,ps3> = splitPbyW(myps,innt(x));
		myps = ps1 + ps3;
	}
	return grammar(roots,myps);
}

BGFGrammar runUnfold(str x, XBGFScope w, grammar(rs,ps))
{
	if (<_,[production(_, x, BGFExpression rhs)],_> := splitPbyW(ps,innt(x)))
		return runReplace(nonterminal(x),rhs,comboscope(notinnt(x),w),grammar(rs,ps));
	else throw "Nonterminal <x> must be defined horizontally prior to unfolding.";
}

BGFGrammar runUnite(str x, str y, grammar(rs,ps))
{
	if (x == y) throw "Nonterminal <x> is already united with itself.";
	used = allNs(ps);
	if (x notin used || y notin used) throw "Both nonterminals <x> and <y> must not be fresh.";
	<ps1x,ps2x,ps3x> = splitPbyW(ps,innt(x));
	list[BGFProduction] ps4x = ps1x + [production(l,y,e) | p <- ps2x, production(str l,x,BGFExpression e) := p] + ps3x;
	if (x in usedNs(ps4x))
		return runReplace(nonterminal(x),nonterminal(y),globally(),grammar(rs,ps4x));
	else
		return grammar(rs,ps4x);
}

BGFGrammar runUnlabel(str x, grammar(rs,ps))
{
	if (x == "") throw "Please specify which label to unlabel.";
	<ps1,ps2,ps3> = splitPbyW(ps,inlabel(x));
	if ([production(str l, str x, BGFExpression e)] := ps2)
		return grammar(rs, ps1 + production("", x, e) + ps3);
	else
		throw "Label <x> is not found or not unique"; // the latter should never happen
}

BGFGrammar runUpgrade(BGFProduction p1, BGFProduction p2, grammar(rs, ps))
{
	if (/marked(nonterminal(str x)) := p1)
		if (production(str l,x,BGFExpression e) := p2)
		{
			p3 = visit(p1){case marked(_) => e};
			p3 = normalise(p3);
			return grammar(rs,replaceP(ps,p3,unmark(p1)));
		}
		else throw "<p1> and <p2> do not agree on nonterminal.";
	else throw "<p1> does not have a single nonterminal marked.";
}

BGFGrammar runVertical(XBGFScope w, grammar(rs,ps))
{
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
	return grammar(rs,ps1 + ps4 + ps3);
}

BGFGrammar runWiden(BGFExpression e1, BGFExpression e2, XBGFScope w, BGFGrammar g)
{
	if (!transform::library::Width::narrowing(e2,e1))
		throw "<e2> and <e1> are not in widening relation.";
	return runReplace(e1,e2,w,g); 
}

BGFGrammar runYaccify(list[BGFProduction] ps1, grammar(rs,ps2))
{
	if ({str x} := definedNs(ps1))
	{
		<ps3,ps4,ps5> = splitPbyW(ps2,innt(x));
		if ([dyp1] := ps4 && [yp1,yp2] := ps1 && transform::library::Yacc::yaccification(dyp1,{yp1,yp2}))
			return grammar(rs,ps3 + ps1 + ps5);
		else
			throw "<ps1> are not suitable as yaccification of <ps4>.";
	}
	else throw "Production rules must define just one nonterminal.";
}

BGFGrammar runAtomic(list[XBGFCommand] steps, BGFGrammar g) = transform(steps,g);

BGFGrammar runStrip(str a, grammar(rs,ps))
{
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
	return grammar(rs,ps2);
}
