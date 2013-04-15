@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@contributor{BGF2Rascal}
@wiki{BGF2Rascal}
module tools::BGF2Rascal

import lib::Rascalware;
import export::Rascal;
import String; //startsWith
import mutate::type1::HorizontalAll; //HorizontalAll
import io::ReadBGF;
import IO;

public void main(list[str] as)
{
	if (isEmpty(as))
	{
		println("BGF2Rascal tool, usage: bgf2rsc grammar.bgf [GrammarName]");
		return;
	}
	str fname = len(as)==1 ? "GrammarName.rsc" : (endsWith(as[1],".rsc") ? as[1] : as[1]+".rsc");
	str name = toCapital(replaceAll(replaceLast(fname,".rsc",""),"-","_"));
	if (len(as)>2) fname = as[2];
	writeFile(|cwd:///|+fname,pprsc(mutate::type1::HorizontalAll::HorizontalAll(readBGF(|cwd:///|+as[0])),name));
	println("The grammar <name> has been written to <fname>.");
}
