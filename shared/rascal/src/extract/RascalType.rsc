@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
@wiki{RascalADT2BGF}
module extract::RascalType

import lang::rascal::\syntax::RascalRascal;
import language::BGF;
import String;
import IO;

// This maps all abstract syntax declarations to production rules of a grammar in a broad sense
public list[BGFProduction] mapDecl((Declaration)`<Tags _><Visibility _>alias<UserType user> = <Type base>;`)
	= [production("","<user>",type2expr(base))];
public list[BGFProduction] mapDecl((Declaration)`<Tags _><Visibility _>data <UserType user> = <{Variant "|"}+ variants> ;`)
	= [production("","<user>",variants2expr(variants))];
public default list[BGFProduction] mapDecl(Declaration d) = [];

// This maps only type aliases to production rules of a grammar in a broad sense
public list[BGFProduction] mapAlias((Declaration)`<Tags _><Visibility _>alias<UserType user> = <Type base>;`)
	= [production("","<user>",type2expr(base))];
public default list[BGFProduction] mapAlias(Declaration d) = [];

// This maps only user type declarations to production rules of a grammar in a broad sense
public list[BGFProduction] mapData((Declaration)`<Tags _><Visibility _>data <UserType user> = <{Variant "|"}+ variants> ;`)
	= [production("","<user>",variants2expr(variants))];
public default list[BGFProduction] mapData(Declaration d) = [];

/*
syntax Variant
	= nAryConstructor: Name name "(" {TypeArg ","}* arguments ")" ;
*/

BGFExpression variants2expr({Variant "|"}+ variants)
 = language::BGF::choice([variant2expr(v) | Variant v <- variants]);

BGFExpression variant2expr((Variant)`<Name name> ()`)
	= language::BGF::selectable(name2string("<name>"),epsilon());
default BGFExpression variant2expr((Variant)`<Name name> ( <{TypeArg ","}+ arguments> )`)
	= language::BGF::selectable(name2string("<name>"),typeargs2seq(arguments));
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
BGFExpression type2expr((Type)`<UserType ut>`) = language::BGF::nonterminal(name2string("<ut>"));
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
	= language::BGF::star(typeargs2seq(arguments));
BGFExpression type2expr((Type)`real`) = language::BGF::val(language::BGF::integer()); // not entirely true!
BGFExpression type2expr((Type)`tuple[<{TypeArg ","}+ arguments>]`) = typeargs2seq(arguments);
BGFExpression type2expr((Type)`str`) = language::BGF::val(language::BGF::string());
BGFExpression type2expr((Type)`bool`)
	= language::BGF::choice([language::BGF::terminal("true"),language::BGF::terminal("false")]);
BGFExpression type2expr((Type)`void`) = language::BGF::empty();
BGFExpression type2expr((Type)`datetime`) = language::BGF::val(language::BGF::string()); // abstraction
BGFExpression type2expr((Type)`set[<Type t>]`) = language::BGF::star(type2expr(t));
BGFExpression type2expr((Type)`map[<{TypeArg ","}+ arguments>]`)
	= language::BGF::star(typeargs2seq(arguments));
BGFExpression type2expr((Type)`list[<Type t>]`) = language::BGF::star(type2expr(t));
BGFExpression type2expr((Type)`set[<TypeArg ta>]`) = language::BGF::star(typearg2expr(ta));
BGFExpression type2expr((Type)`list[<TypeArg ta>]`) = language::BGF::star(typearg2expr(ta));

// TODO: FunctionType (arguable!)
default BGFExpression type2expr(Type t)// = language::BGF::empty();
{
	println("Failure or ambiguity in type2expr(<t>).");
	// iprintln(t);
	return language::BGF::empty();
}

BGFExpression typearg2expr((TypeArg)`<Type t>`) = type2expr(t);
BGFExpression typearg2expr((TypeArg)`<Type t><Name n>`) = language::BGF::selectable(name2string("<n>"),type2expr(t));
default BGFExpression typearg2expr(TypeArg ta) //= empty();
{
	println("Failure or ambiguity in typearg2expr(<ta>).");
	// iprintln(ta);
	return language::BGF::nonterminal(name2string("<ta>"));
}

//BGFExpression typeargs2seq(({TypeArg ","}*)``) = epsilon();
BGFExpression typeargs2seq({TypeArg ","}+ args) = language::BGF::sequence([typearg2expr(argt) | TypeArg argt <- args]);
// TODO: fails at ambiguities

str name2string(str s)
{
	if (startsWith(s,"\\"))
		return substring(s,1);
	else
		return s;
} 