@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module TestCompletion

import io::MegaL;
import structure::MegaADT;

import transform::Complete;
import backend::MegaDiffer;

test bool testAddLang() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/a.graphml|)),
									readYEd(|project://megal/lib/tests/al.graphml|));

test bool testAddOutput() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/if.graphml|)),
									readYEd(|project://megal/lib/tests/ifo.graphml|));

test bool testAddInput() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/fo.graphml|)),
									readYEd(|project://megal/lib/tests/ifo.graphml|));

test bool testAddDomain() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/fl.graphml|)),
									readYEd(|project://megal/lib/tests/lfl.graphml|));

test bool testAddRange() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/lf.graphml|)),
									readYEd(|project://megal/lib/tests/lfl.graphml|));

test bool testAddFunction() = megadiffnonames(completed(readYEd(|project://megal/lib/tests/ifoll.graphml|)),
									readYEd(|project://megal/lib/tests/ifolfl.graphml|));

