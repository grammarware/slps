@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RascalADT2BGF}
module extract::RascalADT2BGF

import lang::rascal::\syntax::RascalRascal;
import ParseTree;

import extract::RascalType;
import language::BGF;
import io::WriteBGF;
import normal::BGF;
import String;

import IO;
import export::BNF;

public void main(list[str] args) = writeBGF(extractBGF(|cwd:///|+args[0]),|cwd:///|+args[1]);
public void main(list[loc] las) = writeBGF(extractBGF(las[0]),las[1]);
public void main(loc z) = println(pp(extractBGF(z)));

BGFGrammar extractBGF(loc z)
{
	//= normalise(language::BGF::grammar([],module2aliases(parse(#Module,trim(readFile(z))))));
	Module m;
	try
		m = parse(#Module,trim(readFile(z)));
	catch:
		m = parse(#Module,readFile(z));
	BGFProdList ps = module2decls(m);
	println("Extraction completed.");
	return language::BGF::grammar([],ps);
}

public BGFProdList module2aliases(Module m) = [p | /Declaration d := m, p<-mapAlias(d)];
public BGFProdList module2datas(Module m) = [p | /Declaration d := m, p<-mapData(d)];
public BGFProdList module2decls(Module m) = [p | /Declaration d := m, p<-mapDecl(d)];
