@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module export::XBNF

import lib::Rascalware;
import syntax::XBGF;
import syntax::BGF;
import export::BNF;

// TODO this is only a first sketchy setup, need to generate the whole thing!
public str ppxs(XBGFSequence xs) = mapjoin(ppx,xs,"\n");
public str ppx(renameN(a,b)) = "renameN(<a>,<b>);";
public str ppx(extract(p,globally())) = "extract(<pp(p)>);";
public default str ppx(XBGFCommand s) = "??<s>??";
