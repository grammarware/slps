@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::MegaL

import structure::MegaGrammar;
import structure::MegaADT;

import backend::GraphML2MegaL;
import backend::MegaImplode;
import backend::MegaExport;
import backend::MegaDot;
import backend::MegaManipulate;

import ParseTree;
import String;
import List;
import Set;
import IO;
import Exception;

public MegaModel readPT(loc f) = parse(#MegaModel,trim(readFile(f)));

public AMegaModel readAST(loc f) = backend::MegaImplode::mapmegal(readPT(f));

public void writePT(loc f, MegaModel pt) = writeFile(f,"<pt>");

public void writeAST(loc f, AMegaModel ast) = writeFile(f,backend::MegaExport::exportmega(ast));

public void writeDot(loc f, AMegaModel ast) = writeFile(f,backend::MegaDot::exportmega(ast));
public void writeDot(loc f, AMegaModel ast, list[MegaDeclaration] ds, list[MegaRelation] rs) = writeFile(f,backend::MegaDot::exportmega(ast,ds,rs));

public AMegaModel readYEd(loc f) = backend::GraphML2MegaL::readGraphML(f);

public AMegaModel readFlat(loc f)
{
	AMegaModel a = readAST(f), b;
	list[str] includes = a.incs, done = [];
	a.incs = [];
	while(!isEmpty(includes - done))
	for (i <- includes)
	{
		name = f.parent+(substring(i,findLast(i,"/")+1)+".megal");
		if (name in done) continue;
		try
		{
			b = readAST(name);
			done += i;
		}
		catch IO:
		{
			println("Cannot find <name>");
			done += i;
			continue;
		}
		a = megamodel(a.name,a.desc,[],mergeLists(a.decls,b.decls), mergeLists(a.rels, b.rels));
		includes += b.incs;
	}
	//println(includes);
	return a;
}
