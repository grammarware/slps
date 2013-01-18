@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{XBGF2Rascal}
module metarascal::XBGF2Rascal

import String;
import IO;
import io::ReadBGF;
import io::ReadXBGF;
import export::XBNF;

public void main(list[str] args) = main(|cwd:///|+args[0]);

public void main(loc x)
{
	xbgf = readXBGF(x);
	writeFile(//|cwd:///Transformation.rsc|,
	|project://slps/src/metarascal/Transformation.rsc|,
	"@contributor{XBGF2Rascal}
	'module Transformation // feel free to change that after moving the file
	'
	'import syntax::BGF;
	'import syntax::XBGF;
	'import transform::XBGF;
	'
	'BGFGrammar doTrafo(BGFGrammar g)
	'	= transform([<for(step <- xbgf){>
	'		// <ppx(step)>
	'		<step>,<}>
	'		bypass()
	'	],g);
	'");
}

public void tst() = main(|home:///projects/slps/topics/island/csharp-ecma-334-1/skeletonise.xbgf|);