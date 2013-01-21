@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{ADT2PP}
module export::XPNF

import lib::Rascalware;
import syntax::XBGF;
import syntax::BGF;
import export::PNF;

public str ppxs(XBGFSequence xs) = mapjoin(ppx,xs,"\n");

public str ppx(XBGFCommand::abridge(BGFProduction p)) = "abridge(<ppnf(p)>)";
public str ppx(XBGFCommand::abstractize(BGFProduction p)) = "abstractize(<ppnf(p)>)";
public str ppx(XBGFCommand::addH(BGFProduction p)) = "addH(<ppnf(p)>)";
public str ppx(XBGFCommand::addV(BGFProduction p)) = "addV(<ppnf(p)>)";
public str ppx(XBGFCommand::anonymize(BGFProduction p)) = "anonymize(<ppnf(p)>)";
public str ppx(XBGFCommand::appear(BGFProduction p)) = "appear(<ppnf(p)>)";
public str ppx(XBGFCommand::bypass()) = "bypass()";
public str ppx(XBGFCommand::chain(BGFProduction p)) = "chain(<ppnf(p)>)";
public str ppx(XBGFCommand::clone(str x, str y, XBGFScope w)) = "clone(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::concatT(list[str] xs, str y, XBGFScope w)) = "concatT(<pp(xs)>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::concretize(BGFProduction p)) = "concretize(<ppnf(p)>)";
public str ppx(XBGFCommand::deanonymize(BGFProduction p)) = "deanonymize(<ppnf(p)>)";
public str ppx(XBGFCommand::define(list[BGFProduction] ps)) = "define(<ppnf(ps)>)";
public str ppx(XBGFCommand::designate(BGFProduction p)) = "designate(<ppnf(p)>)";
public str ppx(XBGFCommand::detour(BGFProduction p)) = "detour(<ppnf(p)>)";
public str ppx(XBGFCommand::deyaccify(str x)) = "deyaccify(<x>)";
public str ppx(XBGFCommand::disappear(BGFProduction p)) = "disappear(<ppnf(p)>)";
public str ppx(XBGFCommand::distribute(XBGFScope w)) = "distribute(<pp(w)>)";
public str ppx(XBGFCommand::downgrade(BGFProduction p1, BGFProduction p2)) = "downgrade(<ppnf(p1)>,<ppnf(p2)>)";
public str ppx(XBGFCommand::eliminate(str x)) = "eliminate(<x>)";
public str ppx(XBGFCommand::equate(str x, str y)) = "equate(<x>,<y>)";
public str ppx(XBGFCommand::extract(BGFProduction p, XBGFScope w)) = "extract(<ppnf(p)>,<pp(w)>)";
public str ppx(XBGFCommand::factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "factor(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::fold(str x, XBGFScope w)) = "fold(<x>,<pp(w)>)";
public str ppx(XBGFCommand::horizontal(XBGFScope w)) = "horizontal(<pp(w)>)";
public str ppx(XBGFCommand::importG(list[BGFProduction] ps)) = "importG(<ppnf(ps)>)";
public str ppx(XBGFCommand::inject(BGFProduction p)) = "inject(<ppnf(p)>)";
public str ppx(XBGFCommand::inline(str x)) = "inline(<x>)";
public str ppx(XBGFCommand::introduce(list[BGFProduction] ps)) = "introduce(<ppnf(ps)>)";
public str ppx(XBGFCommand::iterate(BGFProduction p)) = "iterate(<ppnf(p)>)";
public str ppx(XBGFCommand::lassoc(BGFProduction p)) = "lassoc(<ppnf(p)>)";
public str ppx(XBGFCommand::massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "massage(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "narrow(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::permute(BGFProduction p)) = "permute(<ppnf(p)>)";
public str ppx(XBGFCommand::project(BGFProduction p)) = "project(<ppnf(p)>)";
public str ppx(XBGFCommand::rassoc(BGFProduction p)) = "rassoc(<ppnf(p)>)";
public str ppx(XBGFCommand::redefine(list[BGFProduction] ps)) = "redefine(<ppnf(ps)>)";
public str ppx(XBGFCommand::removeH(BGFProduction p)) = "removeH(<ppnf(p)>)";
public str ppx(XBGFCommand::removeV(BGFProduction p)) = "removeV(<ppnf(p)>)";
public str ppx(XBGFCommand::renameL(str x, str y)) = "renameL(<x>,<y>)";
public str ppx(XBGFCommand::renameN(str x, str y)) = "renameN(<x>,<y>)";
public str ppx(XBGFCommand::renameS(str x, str y, XBGFScope w)) = "renameS(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::renameT(str x, str y)) = "renameT(<x>,<y>)";
public str ppx(XBGFCommand::replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "replace(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::reroot(list[str] xs)) = "reroot(<pp(xs)>)";
public str ppx(XBGFCommand::splitN(str x, list[BGFProduction] ps, XBGFScope w)) = "splitN(<x>,<ppnf(ps)>,<pp(w)>)";
public str ppx(XBGFCommand::splitT(str x, list[str] ys, XBGFScope w)) = "splitT(<x>,<pp(ys)>,<pp(w)>)";
public str ppx(XBGFCommand::unchain(BGFProduction p)) = "unchain(<ppnf(p)>)";
public str ppx(XBGFCommand::undefine(list[str] xs)) = "undefine(<pp(xs)>)";
public str ppx(XBGFCommand::unfold(str x, XBGFScope w)) = "unfold(<x>,<pp(w)>)";
public str ppx(XBGFCommand::unite(str x, str y)) = "unite(<x>,<y>)";
public str ppx(XBGFCommand::unlabel(str x)) = "unlabel(<x>)";
public str ppx(XBGFCommand::upgrade(BGFProduction p1, BGFProduction p2)) = "upgrade(<ppnf(p1)>,<ppnf(p2)>)";
public str ppx(XBGFCommand::vertical(XBGFScope w)) = "vertical(<pp(w)>)";
public str ppx(XBGFCommand::widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "widen(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::yaccify(list[BGFProduction] ps)) = "yaccify(<ppnf(ps)>)";
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

str pp(list[str] ss) = joinStrings(ss,",");
