@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module transform::NegotiatedXBGF

import IO;
import syntax::BGF;
import syntax::XBGF;
//import diff::GDT;
import List;
import String;
import Integer;
import normal::BGF;
import transform::library::Core;
import transform::library::Util;
import transform::XBGF;

data Problem
	= noproblem()
	| error(str m);

data Advice
	= noadvice()
	| setadvice(str s, set[str] a);

public tuple[Problem,Advice,BGFGrammar] attemptTransform(XBGFSequence xbgf, BGFGrammar g)
{
	BGFGrammar g1 = normalise(g);
	Problem p = noproblem();
	Advice a = noadvice();
	for (XBGFCommand step <- xbgf)
	{
		switch(step)
		{
			case abridge(BGFProduction p): g1 = transform::XBGF::runAbridge(p,g1);
			case abstractize(BGFProduction p): g1 = transform::XBGF::runAbstractize(p,g1);
			case addH(BGFProduction p): g1 = transform::XBGF::runAddH(p,g1);
			case addV(BGFProduction p): g1 = transform::XBGF::runAddV(p,g1);
			case anonymize(BGFProduction p): g1 = transform::XBGF::runAnonymize(p,g1);
			case appear(BGFProduction p): g1 = transform::XBGF::runAppear(p,g1);
			case chain(BGFProduction p): g1 = transform::XBGF::runChain(p,g1);
			case clone(str x, str y, XBGFScope w): g1 = transform::XBGF::runClone(x,y,w,g1);
			case concatT(list[str] xs, str y, XBGFScope w): g1 = transform::XBGF::runConcatT(xs,y,w,g1);
			case concretize(BGFProduction p): g1 = transform::XBGF::runConcretize(p,g1);
			case deanonymize(BGFProduction p): g1 = transform::XBGF::runDeanonymize(p,g1);
			case define(list[BGFProduction] ps): g1 = transform::XBGF::runDefine(ps,g1);
			case designate(BGFProduction p): g1 = transform::XBGF::runDesignate(p,g1);
			case detour(BGFProduction p): g1 = transform::XBGF::runDetour(p,g1);
			case deyaccify(str x): g1 = transform::XBGF::runDeyaccify(x,g1);
			case disappear(BGFProduction p): g1 = transform::XBGF::runDisappear(p,g1);
			case distribute(XBGFScope w): g1 = transform::XBGF::runDistribute(w,g1);
			case downgrade(BGFProduction p1,BGFProduction p2): g1 = transform::XBGF::runDowngrade(p1,p2,g1);
			case eliminate(str x): g1 = transform::XBGF::runEliminate(x,g1);
			case equate(str x, str y): g1 = transform::XBGF::runEquate(x,y,g1);
			case extract(BGFProduction p, XBGFScope w): g1 = transform::XBGF::runExtract(p,w,g1);
			case factor(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = transform::XBGF::runFactor(e1,e2,w,g1);
			case fold(str x, XBGFScope w): g1 = transform::XBGF::runFold(x,w,g1);
			case horizontal(XBGFScope w): g1 = transform::XBGF::runHorizontal(w,g1);
			case importG(list[BGFProduction] ps): g1 = transform::XBGF::runImportG(ps,g1);
			case inject(BGFProduction p): g1 = transform::XBGF::runInject(p,g1);
			case inline(str x): g1 = transform::XBGF::runInline(x,g1);
			case introduce(list[BGFProduction] ps): g1 = transform::XBGF::runIntroduce(ps,g1);
			case iterate(BGFProduction p): g1 = transform::XBGF::runIterate(p,g1);
			case lassoc(BGFProduction p): g1 = transform::XBGF::runAssoc(p,g1);
			case massage(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = transform::XBGF::runMassage(e1,e2,w,g1);
			case narrow(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = transform::XBGF::runNarrow(e1,e2,w,g1);
			case permute(BGFProduction p): g1 = transform::XBGF::runPermute(p,g1);
			case project(BGFProduction p): g1 = transform::XBGF::runProject(p,g1);
			case rassoc(BGFProduction p): g1 = transform::XBGF::runAssoc(p,g1);
			case redefine(list[BGFProduction] ps): g1 = transform::XBGF::runRedefine(ps,g1);
			case removeH(BGFProduction p): g1 = transform::XBGF::runRemoveH(p,g1);
			case removeV(BGFProduction p): g1 = transform::XBGF::runRemoveV(p,g1);
			case renameL(str x, str y): g1 = transform::XBGF::runRenameL(x,y,g1);
			
			case renameN(str x, str y): <p,a,g1> = runRenameN(x,y,g1);
			
			case renameS(str x, str y, XBGFScope w): g1 = transform::XBGF::runRenameS(x,y,w,g1);
			case renameT(str x, str y): g1 = transform::XBGF::runRenameT(x,y,g1);
			case replace(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = transform::XBGF::runReplace(e1,e2,w,g1);
			case reroot(list[str] xs): g1 = transform::XBGF::runReroot(xs,g1);
			case splitN(str x, list[BGFProduction] ps, XBGFScope w): g1 = transform::XBGF::runSplitN(x,ps,w,g1);
			case splitT(str x, list[str] ys, XBGFScope w): g1 = transform::XBGF::runSplitT(x,ys,w,g1);
			case unchain(BGFProduction p): g1 = transform::XBGF::runUnchain(p,g1);
			case undefine(list[str] xs): g1 = transform::XBGF::runUndefine(xs,g1);
			case unfold(str x, XBGFScope w): g1 = transform::XBGF::runUnfold(x,w,g1);
			case unite(str x, str y): g1 = transform::XBGF::runUnite(x,y,g1);
			case unlabel(str x): g1 = transform::XBGF::runUnlabel(x,g1);
			case upgrade(BGFProduction p1, BGFProduction p2): g1 = transform::XBGF::runUpgrade(p1,p2,g1);
			case vertical(XBGFScope w): g1 = transform::XBGF::runVertical(w,g1);
			case widen(BGFExpression e1, BGFExpression e2, XBGFScope w): g1 = transform::XBGF::runWiden(e1,e2,w,g1);
			case yaccify(list[BGFProduction] ps): g1 = transform::XBGF::runYaccify(ps,g1);
			case atomic(list[XBGFCommand] steps): g1 = transform::XBGF::runAtomic(steps,g1);
			case strip(str a): g1 = transform::XBGF::runStrip(a,g1);
			default: throw "Unknown XBGF command <step>";
		}
		if (noproblem() !:= p)
			return <p,a,g1>;		
	}
	return <noproblem(),noadvice(),normalise(g1)>;
}

tuple[Problem,Advice,BGFGrammar] runAbridge(BGFProduction prod, grammar(rs, ps))
{
	if (production(_,x,nonterminal(x)) !:= prod)
		return
		<
			error("Production <prod> cannot be abridged."),
			adviseDetouredProd(prod,ps), // NOT IMPLEMENTED YET
			grammar(rs,ps)
		>;
	if (!inProds(prod,ps))
		return
		<
			error("Production <prod> not found."),
			setadvice("Consider the nonterminal <prod.lhs> already abridged.",[]),
			grammar(rs,ps)
		>;
	return <noproblem(),noadvice(),grammar(rs, ps - prod)>;
}


tuple[Problem,Advice,BGFGrammar] runRenameN(str x, str y, grammar(rs, ps))
{
	ns = allNs(ps);
	if (x notin ns)
		return
		<
			error("Source name <x> for renaming must not be fresh."),
			adviseUsedNonterminal(x,allNs(ps)),
			grammar(rs, ps)
		>;
	if (y in ns)
		return
		<
			error("Target name <y> for renaming must be fresh."),
			adviseFreshNonterminal(y,allNs(ps)),
			grammar(rs, ps)
		>;
	return <noproblem(),noadvice(),transform::library::Core::performRenameN(x,y,grammar(rs,ps))>;
}

Advice adviseUsedNonterminal(str x, set[str] nts)
{
	int minl = 9000;
	str mins = "";
	good = {z | z <- nts, levenshtein(z,x) == min([levenshtein(s,x) | s <- nts])};
	if (good == {})
		return noadvice();
	else
		return setadvice("Did you mean",good);
}

Advice adviseFreshNonterminal(str x, set[str] nts)
{
	list[str] low = ["a", "b", "c", "d", "e", "f", "g", "h", "i", "j", "k", "l", "m", "n", "o", "p", "q", "r", "s", "t", "u", "v", "w", "x", "y"];
	list[str] upp = ["A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y"];
	set[str] adv = {};
	int cx = 1;
	str s = x;
	// expr -> expr1
	while ("<x><cx>" in nts) cx+=1;
	adv += "<x><cx>";
	cx = 0;
	// expr -> expr_
	while (s in nts) s += "_"; 
	adv += s;
	// expr -> shjk
	s = "";
	for (c <- [stringChar(charAt(x,i)) | i <- [0..size(x)-1]])
		if (c in low)
			s += low[arbInt(size(low))];
		elseif (c in upp)
			s += upp[arbInt(size(upp))];
		else
			s += stringChar(c);
	adv += s;
	return setadvice("Did you mean",adv);
}