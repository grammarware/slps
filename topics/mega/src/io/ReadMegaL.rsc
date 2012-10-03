@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::ReadMegaL

import structure::MegaGrammar;
import structure::MegaADT;
import backend::MegaImplode;
import ParseTree;
import String;
import IO;

public MegaModel readPT(loc f) = parse(#MegaModel,trim(readFile(f)));

public AMegaModel readAST(loc f) = backend::MegaImplode::mapmegal(readPT(f));
