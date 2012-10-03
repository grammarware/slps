@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::MegaL

import structure::MegaGrammar;
import structure::MegaADT;
import backend::MegaImplode;
import backend::MegaExport;
import ParseTree;
import String;
import IO;

public MegaModel readPT(loc f) = parse(#MegaModel,trim(readFile(f)));

public AMegaModel readAST(loc f) = backend::MegaImplode::mapmegal(readPT(f));

public void writePT(loc f, MegaModel pt) = writeFile(f,"<pt>");

public void writeAST(loc f, AMegaModel ast) = writeFile(f,backend::MegaExport::exportmega(ast));
