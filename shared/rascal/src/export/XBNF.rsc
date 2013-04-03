@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{ADT2PP}
module export::XBNF

import lib::Rascalware;
import language::XScope;
import language::XBGF;
import language::BGF;
import export::BNF;

public str ppxs(XBGFSequence xs) = mapjoin(ppx,xs,"\n");

public str ppx(XBGFCommand::abridge(BGFProduction p)) = "abridge(<pp(p)>)";
public str ppx(XBGFCommand::abstractize(BGFProduction p)) = "abstractize(<pp(p)>)";
public str ppx(XBGFCommand::addC(BGFProduction p)) = "addC(<pp(p)>)";
public str ppx(XBGFCommand::addH(BGFProduction p)) = "addH(<pp(p)>)";
public str ppx(XBGFCommand::addV(BGFProduction p)) = "addV(<pp(p)>)";
public str ppx(XBGFCommand::anonymize(BGFProduction p)) = "anonymize(<pp(p)>)";
public str ppx(XBGFCommand::appear(BGFProduction p)) = "appear(<pp(p)>)";
public str ppx(XBGFCommand::bypass()) = "bypass()";
public str ppx(XBGFCommand::chain(BGFProduction p)) = "chain(<pp(p)>)";
public str ppx(XBGFCommand::clone(str x, str y, XBGFScope w)) = "clone(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::concatT(list[str] xs, str y, XBGFScope w)) = "concatT(\"<joinStrings(xs,"\"+\"")>\",\"<y>\",<pp(w)>)";
public str ppx(XBGFCommand::concretize(BGFProduction p)) = "concretize(<pp(p)>)";
public str ppx(XBGFCommand::deanonymize(BGFProduction p)) = "deanonymize(<pp(p)>)";
public str ppx(XBGFCommand::define(list[BGFProduction] ps)) = "define(<pp(ps)>)";
public str ppx(XBGFCommand::designate(BGFProduction p)) = "designate(<pp(p)>)";
public str ppx(XBGFCommand::detour(BGFProduction p)) = "detour(<pp(p)>)";
public str ppx(XBGFCommand::deyaccify(str x)) = "deyaccify(<x>)";
public str ppx(XBGFCommand::disappear(BGFProduction p)) = "disappear(<pp(p)>)";
public str ppx(XBGFCommand::distribute(XBGFScope w)) = "distribute(<pp(w)>)";
public str ppx(XBGFCommand::downgrade(BGFProduction p1, BGFProduction p2)) = "downgrade(<pp(p1)>,<pp(p2)>)";
public str ppx(XBGFCommand::eliminate(str x)) = "eliminate(<x>)";
public str ppx(XBGFCommand::equate(str x, str y)) = "equate(<x>,<y>)";
public str ppx(XBGFCommand::extract(BGFProduction p, XBGFScope w)) = "extract(<pp(p)>,<pp(w)>)";
public str ppx(XBGFCommand::factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "factor(<pp(e1)>,<pp(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::fold(str x, XBGFScope w)) = "fold(<x>,<pp(w)>)";
public str ppx(XBGFCommand::horizontal(XBGFScope w)) = "horizontal(<pp(w)>)";
public str ppx(XBGFCommand::importG(list[BGFProduction] ps)) = "importG(<pp(ps)>)";
public str ppx(XBGFCommand::inject(BGFProduction p)) = "inject(<pp(p)>)";
public str ppx(XBGFCommand::inline(str x)) = "inline(<x>)";
public str ppx(XBGFCommand::introduce(list[BGFProduction] ps)) = "introduce(<pp(ps)>)";
public str ppx(XBGFCommand::iterate(BGFProduction p)) = "iterate(<pp(p)>)";
public str ppx(XBGFCommand::lassoc(BGFProduction p)) = "lassoc(<pp(p)>)";
public str ppx(XBGFCommand::massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "massage(<pp(e1)>,<pp(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "narrow(<pp(e1)>,<pp(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::permute(BGFProduction p)) = "permute(<pp(p)>)";
public str ppx(XBGFCommand::project(BGFProduction p)) = "project(<pp(p)>)";
public str ppx(XBGFCommand::rassoc(BGFProduction p)) = "rassoc(<pp(p)>)";
public str ppx(XBGFCommand::redefine(list[BGFProduction] ps)) = "redefine(<pp(ps)>)";
public str ppx(XBGFCommand::removeH(BGFProduction p)) = "removeH(<pp(p)>)";
public str ppx(XBGFCommand::removeV(BGFProduction p)) = "removeV(<pp(p)>)";
public str ppx(XBGFCommand::renameL(str x, str y)) = "renameL(<x>,<y>)";
public str ppx(XBGFCommand::renameN(str x, str y)) = "renameN(<x>,<y>)";
public str ppx(XBGFCommand::renameS(str x, str y, XBGFScope w)) = "renameS(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::renameT(str x, str y)) = "renameT(<x>,<y>)";
public str ppx(XBGFCommand::replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "replace(<pp(e1)>,<pp(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::reroot(list[str] xs)) = "reroot(<joinStrings(xs,",")>)";
public str ppx(XBGFCommand::splitN(str x, list[BGFProduction] ps, XBGFScope w)) = "splitN(<x>,<pp(ps)>,<pp(w)>)";
public str ppx(XBGFCommand::splitT(str x, list[str] ys, XBGFScope w)) = "splitT(\"<x>\",\"<joinStrings(ys,"\"+\"")>\",<pp(w)>)";
public str ppx(XBGFCommand::unchain(BGFProduction p)) = "unchain(<pp(p)>)";
public str ppx(XBGFCommand::undefine(list[str] xs)) = "undefine(<joinStrings(xs,",")>)";
public str ppx(XBGFCommand::unfold(str x, XBGFScope w)) = "unfold(<x>,<pp(w)>)";
public str ppx(XBGFCommand::unite(str x, str y)) = "unite(<x>,<y>)";
public str ppx(XBGFCommand::unlabel(str x)) = "unlabel(<x>)";
public str ppx(XBGFCommand::upgrade(BGFProduction p1, BGFProduction p2)) = "upgrade(<pp(p1)>,<pp(p2)>)";
public str ppx(XBGFCommand::vertical(XBGFScope w)) = "vertical(<pp(w)>)";
public str ppx(XBGFCommand::widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "widen(<pp(e1)>,<pp(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::yaccify(list[BGFProduction] ps)) = "yaccify(<pp(ps)>)";
public str ppx(XBGFCommand::atomic(list[XBGFCommand] steps)) = "atomic(<pp(steps)>)";
public str ppx(XBGFCommand::strip(str a)) = "strip(<a>)";
public default str ppx(XBGFCommand smth) = "??<smth>??";

str pp(XBGFScope::globally()) = "globally()";
str pp(XBGFScope::nowhere()) = "nowhere()";
str pp(XBGFScope::inlabel(str l)) = "inlabel(<l>)";
str pp(XBGFScope::notinlabel(str l)) = "notinlabel(<l>)";
str pp(XBGFScope::innt(str x)) = "innt(<x>)";
str pp(XBGFScope::notinnt(str x)) = "notinnt(<x>)";
str pp(XBGFScope::comboscope(XBGFScope w1, XBGFScope w2)) = "comboscope(<pp(w1)>,<pp(w2)>)";
default str pp(XBGFScope smth) = "??<smth>??";

//str pp(list[str] ss) = joinStrings(ss,",");
