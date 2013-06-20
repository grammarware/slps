@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{Zoo}
module tools::UpdateZoo

import IO;
import io::WriteBGF;
import language::BGF;
// extractors
import extract::RascalADT2BGF;
import extract::RascalSyntax2BGF;

/*
slps/cbgf-adt/Makefile: # ${tooldir}/rscd2bgf ../../../../shared/rascal/src/language/CBGF.rsc ext.bgf
slps/xbgf-adt/Makefile: # ${tooldir}/rscd2bgf ../../../../shared/rascal/src/language/XBGF.rsc ext.xbgf.bgf
slps/xbgf-adt/Makefile: # ${tooldir}/rscd2bgf ../../../../shared/rascal/src/language/XScope.rsc ext.scope.bgf
*/
loc zoourl = |home:///projects/slps/topics/grammars/|;
rel[BGFGrammar(loc),rel[loc,str]] zoo =
 {
 <extract::RascalADT2BGF::extractBGF,
 {
 	<|home:///projects/slps/topics/grammars/bgf/v4.1-adt/XBGFSyntax.rsc|, "bgf/v4.1-adt/ext.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v4.2-adt/BGF.rsc|, "bgf/v4.2-adt/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v4.3-adt/BGF.rsc|, "bgf/v4.3-adt/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v5.0-adt/BGF.rsc|, "bgf/v5.0-adt/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v5.1-adt/BGF.rsc|, "bgf/v5.1-adt/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v5.3-adt/BGF.rsc|, "bgf/v5.3-adt/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/bgf/v5.4-adt/BGF.rsc|, "bgf/v5.4-adt/grammar.bgf">,
 	//
 	<|std:///lang/dot/Dot.rsc|, "dot/rascal-abstract/grammar.bgf">,
 	<|home:///projects/slps/topics/fl/rascal/src/Abstract.rsc|, "fl/adt/grammar.bgf">,
 	<|home:///projects/slps/shared/rascal/src/language/JSON.rsc|, "json/zaytsev-abstract/grammar.bgf">,
 	<|std:///lang/json/ast/JSON.rsc|, "json/rascal-abstract/grammar.bgf">,
 	<|home:///projects/slps/shared/rascal/src/language/XOutcome.rsc|, "xbgf/xoutcome/grammar.bgf">,
 	<|std:///lang/csv/ast/CSV.rsc|, "csv/rascal-abstract/grammar.bgf">,
 	// trouble with trailing comment
 	//<|std:///lang/jvm/ast/Level0.rsc|, "java/jvm-level0/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/java/jvm-level0/Level0.rsc|, "java/jvm-level0/grammar.bgf">,
 	<|std:///lang/jvm/ast/Level1.rsc|, "java/jvm-level1/grammar.bgf">,
 	<|std:///lang/logic/ast/Booleans.rsc|, "metamodels/logic-abstract/ext.bool.bgf">,
 	<|std:///lang/logic/ast/Propositions.rsc|, "metamodels/logic-abstract/ext.prop.bgf">,
 	<|std:///lang/saf/AST.rsc|, "saf/abstract/grammar.bgf">,
 	<|std:///lang/xml/DOM.rsc|, "xml/rascal/grammar.bgf">
 }>,
 //
 <extract::RascalSyntax2BGF::extractBGF,
 {
 	// TODO: this old module somehow give trouble in parsing its functions
 	//<|home:///projects/slps/topics/grammars/wiki/simple/MediaWiki.rsc|, "wiki/simple/grammar.bgf">,
 	<|home:///projects/slps/topics/fl/rascal/src/Concrete.rsc|, "fl/rascal/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/hats/abs/ABS.rsc|, "hats/abs/grammar.bgf">,
 	// TODO: does not work - too meta? ;)
 	//<|std:///lang/rascal/syntax/RascalRascal.rsc|, "metasyntax/rascal-new/grammar.bgf">
 	<|home:///projects/slps/shared/rascal/src/language/JSON.rsc|, "json/zaytsev-concrete/grammar.bgf">,
 	<|std:///lang/json/syntax/JSON.rsc|, "json/rascal-concrete/grammar.bgf">,
 	// TODO: why this does not work, is beyond me - but it works only without any functions
 	//<|home:///projects/slps/topics/grammars/metasyntax/rascal-adt-simple/RascalADT2BGF.rsc|, "metasyntax/rascal-adt-simple/grammar.bgf">,
 	<|std:///lang/dot/syntax/Dot.rsc|, "dot/rascal-concrete/grammar.bgf">,
 	<|std:///lang/aterm/syntax/ATerm.rsc|, "metasyntax/aterms-rascal/grammar.bgf">,
 	<|std:///lang/box/syntax/Box.rsc|, "metasyntax/box-rascal/grammar.bgf">,
 	// NoSuchKey("ParseTree")
 	//<|std:///lang/c90/syntax/C.rsc|, "c/lankamp/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/c/lankamp/C.rsc|, "c/lankamp/grammar.bgf">,
 	<|std:///lang/csv/syntax/CSV.rsc|, "csv/rascal-concrete/grammar.bgf">,
 	<|std:///lang/dimacs/syntax/Dimacs.rsc|, "markup/cnf/grammar.bgf">,
 	// some extends-related error
 	//<|std:///lang/hotdrink/Adam.rsc|, "adobe/hotdrink/ext.adam.bgf">,
 	//<|std:///lang/hotdrink/Eve.rsc|, "adobe/hotdrink/ext.eve.bgf">,
 	//<|std:///lang/hotdrink/Decls.rsc|, "adobe/hotdrink/ext.decls.bgf">,
 	//<|std:///lang/hotdrink/Expressions.rsc|, "adobe/hotdrink/ext.expr.bgf">
 	// some extends-related error
 	//<|std:///lang/logic/syntax/Booleans.rsc|, "metamodels/logic-concrete/ext.bool.bgf">,
 	//<|std:///lang/logic/syntax/Propositions.rsc|, "metamodels/logic-concrete/ext.prop.bgf">,
 	//<|std:///lang/logic/syntax/Truths.rsc|, "metamodels/logic-concrete/ext.truth.bgf">
 	// NoSuchKey("IO")
 	//<|std:///lang/pico/syntax/Main.rsc|, "pico/rascal/grammar.bgf">
 	<|home:///projects/slps/topics/grammars/pico/rascal/Main.rsc|, "pico/rascal/grammar.bgf">,
 	<|std:///lang/saf/SAF.rsc|, "saf/concrete/grammar.bgf">,
 	<|std:///lang/sdf2/syntax/Sdf2.rsc|, "metasyntax/sdf-rascal/grammar.bgf">,
 	<|std:///lang/sexp/syntax/SExp.rsc|, "metasyntax/s-expressions/grammar.bgf">,
 	// not in the released version of Rascal yet
 	//<|std:///lang/java/syntax/Java15.rsc|, "java-5/landman/grammar.bgf">,
 	<|home:///projects/slps/topics/grammars/java-5/landman/Java15.rsc|, "java-5/landman/grammar.bgf">
 }>
 //
 };
 
 public void main()
 {
 	for (<BGFGrammar(loc) x,rel[loc,str] sr> <- zoo, <loc s, str r> <- sr)
 	{
 		println("Extracting <s>...");
 		g = x(s);
 		writeBGF(g,zoourl+r);
 	}
 }
 