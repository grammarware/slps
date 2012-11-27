@contributor{Vadim Zaytsev - vadim@grammarware.net - SWAT, CWI}
module io::json::Syntax

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
syntax KeyValue = BasicType key ":" BasicType val;

syntax Null = "null" ;
