@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RascalADT2BGF}
module extract::RascalAlias2BGF

import lang::rascal::\syntax::RascalRascal;
import lang::rascal::grammar::definition::Modules;
import export::BNF;
import Grammar;
import ParseTree;
import language::BGF;
import io::WriteBGF;
import normal::BGF;
import String;

import IO;

BGFGrammar extractBGF(loc z)
	= language::BGF::grammar([],module2aliases(parse(#Module,trim(readFile(z)))));

public BGFProdList module2aliases(Module m) = [p | /Declaration d := m, p<-mapDecl(d)];

public void main(list[str] args) = writeBGF(extractBGF(|cwd:///|+args[0]),|cwd:///|+args[1]);

public void main(loc z) = println(pp(extractBGF(z)));

public void debug(loc z)
{
	str gs = trim(readFile(z));
	str name = split("\n",split("module ",gs)[1])[0];
	println("Extraction completed.");
	decls = [d | /Declaration d := parse(#Module,gs)];
	iprintln(decls);
	println(pp([p | d <- decls, p<-mapDecl(d)]));
}

list[BGFProduction] mapDecl((Declaration)`alias<UserType user>=<Type base>;`)
	= [production("","<user>",type2expr(base))];
default list[BGFProduction] mapDecl(Declaration d) = [];

/*
syntax Type
	= bracket \bracket: "(" Type type ")" 
	| user: UserType user \ HeaderKeyword
	| function: FunctionType function 
	| structured: StructuredType structured 
	| basic: BasicType basic 
	| selector: DataTypeSelector selector 
	| variable: TypeVar typeVar 
	| symbol: Sym!nonterminal!labeled!parametrized!parameter symbol
	;
syntax FunctionType
	= typeArguments: Type type "(" {TypeArg ","}* arguments ")" ;
syntax StructuredType
	= \default: BasicType basicType "[" {TypeArg ","}+ arguments "]" ;
syntax BasicType
	= \value: "value" 
	| \loc: "loc" 
	| \node: "node" 
	| \num: "num" 
	| \type: "type" 
	| \bag: "bag" 
	| \int: "int"
	| rational: "rat" 
	| relation: "rel" 
	| \real: "real" 
	| \tuple: "tuple" 
	| string: "str" 
	| \bool: "bool" 
	| \void: "void" 
	| dateTime: "datetime" 
	| \set: "set" 
	| \map: "map" 
	| \list: "list" 
	;
syntax DataTypeSelector
	= selector: QualifiedName sort "." Name production ;
*/

// NOT MAPPED (too tricky): type; list (w/o params), set, ...
BGFExpression type2expr((Type)`<UserType ut>`) = language::BGF::nonterminal("<ut>");
BGFExpression type2expr((Type)`(<Type t>)`) = type2expr(t);
BGFExpression type2expr((Type)`value`) = language::BGF::anything();
BGFExpression type2expr((Type)`loc`) = language::BGF::val(language::BGF::string()); // abstraction
BGFExpression type2expr((Type)`node`)
	= language::BGF::sequence([language::BGF::string(),language::BGF::star(language::BGF::anything())]);
BGFExpression type2expr((Type)`num`) = language::BGF::val(language::BGF::integer());
BGFExpression type2expr((Type)`bag[<Type t>]`) = language::BGF::star(type2expr(t));
BGFExpression type2expr((Type)`int`) = language::BGF::val(language::BGF::integer());
BGFExpression type2expr((Type)`rat`) = language::BGF::val(language::BGF::integer()); // not entirely true!
BGFExpression type2expr((Type)`rel[<{TypeArg ","}+ arguments>]`)
	= language::BGF::star(language::BGF::sequence([typearg2expr(argt) | TypeArg argt <- arguments]));
BGFExpression type2expr((Type)`real`) = language::BGF::val(language::BGF::integer()); // not entirely true!
BGFExpression type2expr((Type)`tuple[<{TypeArg ","}+ arguments>]`)
	= language::BGF::sequence([typearg2expr(argt) | TypeArg argt <- arguments]);
BGFExpression type2expr((Type)`str`) = language::BGF::val(language::BGF::string());
BGFExpression type2expr((Type)`bool`)
	= language::BGF::choice([language::BGF::terminal("true"),language::BGF::terminal("false")]);
BGFExpression type2expr((Type)`void`) = language::BGF::empty();
BGFExpression type2expr((Type)`datetime`) = language::BGF::val(language::BGF::string()); // abstraction
BGFExpression type2expr((Type)`set[<Type t>]`) = language::BGF::star(type2expr(t));
BGFExpression type2expr((Type)`map[<{TypeArg ","}+ arguments>]`)
	= language::BGF::star(language::BGF::sequence([typearg2expr(argt) | TypeArg argt <- arguments]));
BGFExpression type2expr((Type)`list[<Type t>]`) = language::BGF::star(type2expr(t));

// TODO: FunctionType (arguable!)
default BGFExpression type2expr(Type t)// = language::BGF::empty();
{
	iprintln(t);
	return language::BGF::empty();
}

BGFExpression typearg2expr((TypeArg)`<Type t>`) = type2expr(t);
BGFExpression typearg2expr((TypeArg)`<Type t><Name n>`) = language::BGF::selectable("<n>",type2expr(t));
default BGFExpression typearg2expr(TypeArg _) = empty();
