@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::MegaL

import structure::MegaGrammar;
import structure::MegaADT;
import backend::MegaImplode;
import backend::MegaExport;
import backend::MegaDot;
import ParseTree;
import String;
import List;
import IO;
import Exception;

public MegaModel readPT(loc f) = parse(#MegaModel,trim(readFile(f)));

public AMegaModel readAST(loc f) = backend::MegaImplode::mapmegal(readPT(f));

public void writePT(loc f, MegaModel pt) = writeFile(f,"<pt>");

public void writeAST(loc f, AMegaModel ast) = writeFile(f,backend::MegaExport::exportmega(ast));

public void writeDot(loc f, AMegaModel ast) = writeFile(f,backend::MegaDot::exportmega(ast));

public AMegaModel readFlat(loc f)
{
	AMegaModel a = readAST(f);
	list[str] includes = a.incs;
	a.incs = [];
	while(!isEmpty(includes))
	for (i <- includes)
	{
		//try
			b = readAST(f.parent+(substring(i,findLast(i,"/")+1)+".megal"));
		//catch:
		//	continue;
		a = megamodel(a.name,a.desc,[],a.decls + b.decls, a.rels + b.rels);
		includes += b.incs;
	}
	//println(includes);
	return a;
}
