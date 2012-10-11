@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{Ralf LŠmmel - SLT}
module TestRalf

import io::MegaL;
import structure::MegaADT;

test bool testTech() = readFlat(|project://megal/tests/technology.megal|)!=megamodel("","",[],[],[]);

test bool testWrite() = readFlat(|project://megal/tests/write.megal|)!=megamodel("","",[],[],[]);

