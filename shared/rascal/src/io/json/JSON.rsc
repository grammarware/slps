@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module \syntax::JSON

import IO;

layout L = WS;
lexical WS = [\ \n\r\t]* !>> [\ \n\r\t];

start syntax Data = BasicType;

syntax BasicType
	= Number
	| String
	| Boolean
	| Array
	| Object
	| Null
	;

syntax Number = "-"? Digits ("." Digits)?;
lexical Digits = [0-9]+ !>> [0-9];

syntax String = DoubleQuotedString;
lexical DoubleQuotedString = [\"] DQSElement* [\"]; //"
lexical DQSElement = ![\"] | [\\][\"] ; //"

syntax Boolean = "false" | "true" ;

syntax Array = "[" {BasicType ","}* "]";

syntax Object = "{" {KeyValue ","}* "}";
syntax KeyValue = BasicType ":" BasicType;

syntax Null = "null" ;

data JSO
	= jsnumber(real n)
	| jsstring(str s)
	| jsboolean(bool b)
	| jsarray(list[JSO] xs)
	| jsobject(map[JSO,JSO] kvs)
	| jsnull()
	;

public void main(list[str] args)
{
	loc src = |cwd:///|+args[0];
	PT = parse(#BasicType,src);
	println("Extraction completed.");
}
