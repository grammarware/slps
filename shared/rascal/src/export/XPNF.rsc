@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{ADT2PP}
module export::XPNF

import lib::Rascalware;
import syntax::XBGF;
import syntax::BGF;
import export::PNF;

public str ppxs(XBGFSequence xs) = "\\begin{align*}\n&<mapjoin(ppx,xs,"; \\\\\n&")>\n\\end{align*}";

public str ppx(XBGFCommand::abridge(BGFProduction p)) = "\\mathbf{abridge}(<ppnf(p)>)";
public str ppx(XBGFCommand::abstractize(BGFProduction p)) = "\\mathbf{abstractize}(<ppnf(p)>)";
public str ppx(XBGFCommand::addC(BGFProduction p)) = "\\mathbf{addC}(<ppnf(p)>)";
public str ppx(XBGFCommand::addH(BGFProduction p)) = "\\mathbf{addH}(<ppnf(p)>)";
public str ppx(XBGFCommand::addV(BGFProduction p)) = "\\mathbf{addV}(<ppnf(p)>)";
public str ppx(XBGFCommand::anonymize(BGFProduction p)) = "\\mathbf{anonymize}(<ppnf(p)>)";
public str ppx(XBGFCommand::appear(BGFProduction p)) = "\\mathbf{appear}(<ppnf(p)>)";
public str ppx(XBGFCommand::bypass()) = "\\mathbf{bypass}()";
public str ppx(XBGFCommand::chain(BGFProduction p)) = "\\mathbf{chain}(<ppnf(p)>)";
public str ppx(XBGFCommand::clone(str x, str y, XBGFScope w)) = "\\mathbf{clone}(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::concatT(list[str] xs, str y, XBGFScope w)) = "\\mathbf{concatT}((<joinStrings(["\\te(<x>)" | x<-xs],",")>),\\te(<y>),<pp(w)>)";
public str ppx(XBGFCommand::concretize(BGFProduction p)) = "\\mathbf{concretize}(<ppnf(p)>)";
public str ppx(XBGFCommand::deanonymize(BGFProduction p)) = "\\mathbf{deanonymize}(<ppnf(p)>)";
public str ppx(XBGFCommand::define(list[BGFProduction] ps)) = "\\mathbf{define}(<ppnf(ps)>)";
public str ppx(XBGFCommand::designate(BGFProduction p)) = "\\mathbf{designate}(<ppnf(p)>)";
public str ppx(XBGFCommand::detour(BGFProduction p)) = "\\mathbf{detour}(<ppnf(p)>)";
public str ppx(XBGFCommand::deyaccify(str x)) = "\\mathbf{deyaccify}(<x>)";
public str ppx(XBGFCommand::disappear(BGFProduction p)) = "\\mathbf{disappear}(<ppnf(p)>)";
public str ppx(XBGFCommand::distribute(XBGFScope w)) = "\\mathbf{distribute}(<pp(w)>)";
public str ppx(XBGFCommand::downgrade(BGFProduction p1, BGFProduction p2)) = "\\mathbf{downgrade}(<ppnf(p1)>,<ppnf(p2)>)";
public str ppx(XBGFCommand::eliminate(str x)) = "\\mathbf{eliminate}(<x>)";
public str ppx(XBGFCommand::equate(str x, str y)) = "\\mathbf{equate}(<x>,<y>)";
public str ppx(XBGFCommand::extract(BGFProduction p, globally())) = "\\mathbf{extract}(<ppnf(p)>)";
public str ppx(XBGFCommand::extract(BGFProduction p, XBGFScope w)) = "\\mathbf{extract}(<ppnf(p)>,<pp(w)>)";
public str ppx(XBGFCommand::factor(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{factor}(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::fold(str x, XBGFScope w)) = "\\mathbf{fold}(<x>,<pp(w)>)";
public str ppx(XBGFCommand::horizontal(XBGFScope w)) = "\\mathbf{horizontal}(<pp(w)>)";
public str ppx(XBGFCommand::importG(list[BGFProduction] ps)) = "\\mathbf{importG}(<ppnf(ps)>)";
public str ppx(XBGFCommand::inject(BGFProduction p)) = "\\mathbf{inject}(<ppnf(p)>)";
public str ppx(XBGFCommand::inline(str x)) = "\\mathbf{inline}(\\textit{<x>})";
public str ppx(XBGFCommand::introduce(list[BGFProduction] ps)) = "\\mathbf{introduce}(<ppnf(ps)>)";
public str ppx(XBGFCommand::iterate(BGFProduction p)) = "\\mathbf{iterate}(<ppnf(p)>)";
public str ppx(XBGFCommand::lassoc(BGFProduction p)) = "\\mathbf{lassoc}(<ppnf(p)>)";
public str ppx(XBGFCommand::massage(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{massage}(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::narrow(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{narrow}(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::permute(BGFProduction p)) = "\\mathbf{permute}(<ppnf(p)>)";
public str ppx(XBGFCommand::project(BGFProduction p)) = "\\mathbf{project}(<ppnf(p)>)";
public str ppx(XBGFCommand::rassoc(BGFProduction p)) = "\\mathbf{rassoc}(<ppnf(p)>)";
public str ppx(XBGFCommand::redefine(list[BGFProduction] ps)) = "\\mathbf{redefine}(<ppnf(ps)>)";
public str ppx(XBGFCommand::removeH(BGFProduction p)) = "\\mathbf{removeH}(<ppnf(p)>)";
public str ppx(XBGFCommand::removeV(BGFProduction p)) = "\\mathbf{removeV}(<ppnf(p)>)";
public str ppx(XBGFCommand::renameL(str x, str y)) = "\\mathbf{renameL}(<x>,<y>)";
public str ppx(XBGFCommand::renameN(str x, str y)) = "\\mathbf{renameN}(\\textit{<x>},\\textit{<y>})";
public str ppx(XBGFCommand::renameS(str x, str y, XBGFScope w)) = "\\mathbf{renameS}(<x>,<y>,<pp(w)>)";
public str ppx(XBGFCommand::renameT(str x, str y)) = "\\mathbf{renameT}(<x>,<y>)";
public str ppx(XBGFCommand::replace(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{replace}(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::reroot(list[str] xs)) = "\\mathbf{reroot}(<pp(xs)>)";
public str ppx(XBGFCommand::splitN(str x, list[BGFProduction] ps, XBGFScope w)) = "\\mathbf{splitN}(<x>,<ppnf(ps)>,<pp(w)>)";
public str ppx(XBGFCommand::splitT(str x, list[str] ys, XBGFScope w)) = "\\mathbf{splitT}(\\te(<x>),(<joinStrings(["\\te(<y>)" | y<-ys],",")>),<pp(w)>)";
public str ppx(XBGFCommand::unchain(BGFProduction p)) = "\\mathbf{unchain}(<ppnf(p)>)";
public str ppx(XBGFCommand::undefine(list[str] xs)) = "\\mathbf{undefine}(<pp(xs)>)";
public str ppx(XBGFCommand::unfold(str x, XBGFScope w)) = "\\mathbf{unfold}(<x>,<pp(w)>)";
public str ppx(XBGFCommand::unite(str x, str y)) = "\\mathbf{unite}(\\textit{<x>},\\textit{<y>})";
public str ppx(XBGFCommand::unlabel(str x)) = "\\mathbf{unlabel}(<x>)";
public str ppx(XBGFCommand::upgrade(BGFProduction p1, BGFProduction p2)) = "\\mathbf{upgrade}(<ppnf(p1)>,<ppnf(p2)>)";
public str ppx(XBGFCommand::vertical(XBGFScope w)) = "\\mathbf{vertical}(<pp(w)>)";
public str ppx(XBGFCommand::widen(BGFExpression e1, BGFExpression e2, XBGFScope w)) = "\\mathbf{widen}(<ppnf(e1)>,<ppnf(e2)>,<pp(w)>)";
public str ppx(XBGFCommand::yaccify(list[BGFProduction] ps)) = "\\mathbf{yaccify}(<ppnf(ps)>)";
public str ppx(XBGFCommand::atomic(list[XBGFCommand] steps)) = "\\mathbf{atomic}(<pp(steps)>)";
public str ppx(XBGFCommand::strip(str a)) = "\\mathbf{strip}(<a>)";
public default str ppx(XBGFCommand smth) = "??<smth>??";

str pp(XBGFScope::globally()) = "globally()";
str pp(XBGFScope::nowhere()) = "nowhere()";
str pp(XBGFScope::inlabel(str l)) = "inlabel(<l>)";
str pp(XBGFScope::notinlabel(str l)) = "notinlabel(<l>)";
str pp(XBGFScope::innt(str x)) = "\\textrm{in} \\n(\\textit{<x>})";
str pp(XBGFScope::notinnt(str x)) = "notinnt(<x>)";
str pp(XBGFScope::comboscope(XBGFScope w1, XBGFScope w2)) = "comboscope(<pp(w1)>,<pp(w2)>)";
default str pp(XBGFScope smth) = "??<smth>??";

str pp(list[str] ss) = joinStrings(ss,",");
